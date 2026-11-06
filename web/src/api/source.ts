// GET api/source/<side>/<path>: one file as the source view, CBOR
// (D-265). Shape: see Note [Source view] in
// HaskellReviewer.Server.Wire.Source. The file's text comes from
// api/file. The decoder checks the shape and gives back the wire value
// as it is: runs and regions stay flat (expandRuns reads them).
import {
  DecodeError,
  array,
  bool,
  field,
  nat,
  nullable,
  object,
  oneOf,
  str,
} from "./decode";

// Why a link is a guess.
export const reasonCodes = [
  "ambiguous",
  "not-ready",
  "not-parsed",
  "made-by-splice",
  "made-by-splice-here",
  "via-splice",
  "wildcard",
  "record-dot",
  "data-kinds",
  "cpp-import",
  "import-cycle",
  "ambiguous-module",
  "unlisted-module",
  "loose-import",
  "listed-not-found",
  "this-module",
] as const;

export type ReasonCode = (typeof reasonCodes)[number];

// How sure an external link is.
export const guessHows = ["listed", "only-open", "one-of"] as const;

export type GuessHow = (typeof guessHows)[number];

export const targetKinds = ["local", "decl", "module", "external"] as const;

export const outlineKinds = [
  "type",
  "class",
  "value",
  "patsyn",
  "instance",
  "faminst",
] as const;

export type OutlineKind = (typeof outlineKinds)[number];

// Display classes, by their wire number (D-228).
export const displayClasses = [
  "keyword",
  "typeCon",
  "variable",
  "operator",
  "literal",
  "comment",
  "pragma",
  "cppExcluded",
  "cppDirective",
] as const;

// Region kinds, by their wire number.
export const regionKinds = ["directives", "excluded", "prose"] as const;

// A local binder: where it starts.
export type LocalTarget = ["local", line: number, col: number];

// A declaration of ours. line: of its name; reasons empty when exact;
// candidates 1 unless ambiguous; sig: its code as plain text, for the
// hover (cut at 300 code points plus "…"), null when it has none.
export type DeclTarget = [
  "decl",
  ref: string,
  path: string,
  line: number | null,
  id: string,
  reasons: ReasonCode[],
  candidates: number,
  sig: string | null,
];

// A module of ours, the declaration unknown.
export type ModuleTarget = [
  "module",
  ref: string,
  path: string | null,
  reasons: ReasonCode[],
  candidates: number,
];

// A module of a dependency, with a Hoogle query.
export type ExternalTarget = [
  "external",
  module: string,
  pkg: string | null,
  how: GuessHow,
  count: number,
  query: string,
];

export type Target = LocalTarget | DeclTarget | ModuleTarget | ExternalTarget;

export type OutlineEntry = [
  kind: OutlineKind,
  id: string,
  name: string | null,
  line: number,
  firstLine: number,
  lastLine: number,
];

interface SourceHead {
  generation: number;
  path: string;
  // Null if the file is no module.
  ref: string | null;
  tier: { kind: "syntactic"; reason: "no-hie" };
  // Resolution read a module not ready yet: ask again later.
  partial: boolean;
}

export type PlainSource = SourceHead & { highlight: "plain" };

export type LexedSource = SourceHead & {
  highlight: "lexed";
  // 6 per run: line delta, start col, line span, end col, class, link.
  runs: number[];
  // 3 per region: kind, first line, last line.
  regions: number[];
  targets: Target[];
  outline: OutlineEntry[];
};

export type SourceView = PlainSource | LexedSource;

export interface SourceNotFound {
  generation: number;
  kind: "not-found";
  what: "file";
}

// One run, read back.
export interface Run {
  startLine: number;
  startCol: number;
  endLine: number;
  endCol: number;
  // Index in displayClasses.
  cls: number;
  // 0 none, else 1 + the target's index.
  link: number;
}

// The flat runs as records, lines absolute.
export function expandRuns(runs: readonly number[]): Run[] {
  const out: Run[] = [];
  let line = 0;
  for (let i = 0; i + 5 < runs.length; i += 6) {
    const at = (k: number) => runs[i + k] ?? 0;
    line += at(0);
    out.push({
      startLine: line,
      startCol: at(1),
      endLine: line + at(2),
      endCol: at(3),
      cls: at(4),
      link: at(5),
    });
  }
  return out;
}

// Back to the flat form.
export function packRuns(runs: readonly Run[]): number[] {
  let line = 0;
  return runs.flatMap((r) => {
    const delta = r.startLine - line;
    line = r.startLine;
    return [
      delta,
      r.startCol,
      r.endLine - r.startLine,
      r.endCol,
      r.cls,
      r.link,
    ];
  });
}

function decodeTier(value: unknown, path: string): SourceHead["tier"] {
  const o = object(value, path);
  return {
    kind: field(o, "kind", path, oneOf(["syntactic"] as const)),
    reason: field(o, "reason", path, oneOf(["no-hie"] as const)),
  };
}

function tuple(value: unknown, path: string, length: number): unknown[] {
  if (!Array.isArray(value) || value.length !== length) {
    throw new DecodeError(path, `an array of ${length}`);
  }
  return value as unknown[];
}

const reasons = array(oneOf(reasonCodes));

// A count: 1 or more.
function positive(value: unknown, path: string): number {
  const n = nat(value, path);
  if (n < 1) throw new DecodeError(path, "an integer, 1 or more");
  return n;
}

// Region runs: CPP directive and excluded lines. Whole lines, as the
// server makes them.
const regionClasses: readonly number[] = [
  displayClasses.indexOf("cppExcluded"),
  displayClasses.indexOf("cppDirective"),
];

function decodeTarget(value: unknown, path: string): Target {
  if (!Array.isArray(value)) throw new DecodeError(path, "an array");
  const kind = oneOf(targetKinds)(value[0], `${path}[0]`);
  const at = (i: number) => `${path}[${i}]`;
  switch (kind) {
    case "local": {
      const t = tuple(value, path, 3);
      return [kind, nat(t[1], at(1)), nat(t[2], at(2))];
    }
    case "decl": {
      const t = tuple(value, path, 8);
      return [
        kind,
        str(t[1], at(1)),
        str(t[2], at(2)),
        nullable(nat)(t[3], at(3)),
        str(t[4], at(4)),
        reasons(t[5], at(5)),
        positive(t[6], at(6)),
        nullable(str)(t[7], at(7)),
      ];
    }
    case "module": {
      const t = tuple(value, path, 5);
      return [
        kind,
        str(t[1], at(1)),
        nullable(str)(t[2], at(2)),
        reasons(t[3], at(3)),
        positive(t[4], at(4)),
      ];
    }
    case "external": {
      const t = tuple(value, path, 6);
      return [
        kind,
        str(t[1], at(1)),
        nullable(str)(t[2], at(2)),
        oneOf(guessHows)(t[3], at(3)),
        positive(t[4], at(4)),
        str(t[5], at(5)),
      ];
    }
  }
}

function decodeOutlineEntry(value: unknown, path: string): OutlineEntry {
  const t = tuple(value, path, 6);
  const at = (i: number) => `${path}[${i}]`;
  return [
    oneOf(outlineKinds)(t[0], at(0)),
    str(t[1], at(1)),
    nullable(str)(t[2], at(2)),
    nat(t[3], at(3)),
    nat(t[4], at(4)),
    nat(t[5], at(5)),
  ];
}

// Runs: 6 ints each, known classes, links into the table, sorted and
// not overlapping once expanded.
function checkRuns(runs: number[], targets: number, path: string): void {
  if (runs.length % 6 !== 0) throw new DecodeError(path, "6 ints a run");
  let prevLine = -1;
  let prevCol = -1;
  for (const [i, r] of expandRuns(runs).entries()) {
    const at = `${path}[${6 * i}]`;
    if (r.cls >= displayClasses.length) {
      throw new DecodeError(at, "a known display class");
    }
    if (r.link > targets) throw new DecodeError(at, "a link into targets");
    const before = (l1: number, c1: number, l2: number, c2: number) =>
      l1 < l2 || (l1 === l2 && c1 <= c2);
    if (!before(prevLine, prevCol, r.startLine, r.startCol)) {
      throw new DecodeError(at, "runs in order, not overlapping");
    }
    const empty = r.startLine === r.endLine && r.startCol === r.endCol;
    if (empty || !before(r.startLine, r.startCol, r.endLine, r.endCol)) {
      throw new DecodeError(at, "a run that ends after it starts");
    }
    if (regionClasses.includes(r.cls) && (r.startCol !== 1 || r.endCol !== 1)) {
      throw new DecodeError(at, "a region run of whole lines");
    }
    prevLine = r.endLine;
    prevCol = r.endCol;
  }
}

function checkRegions(regions: number[], path: string): void {
  if (regions.length % 3 !== 0) {
    throw new DecodeError(path, "3 ints a region");
  }
  for (let i = 0; i < regions.length; i += 3) {
    const [kind = 0, first = 0, last = 0] = regions.slice(i, i + 3);
    if (kind >= regionKinds.length) {
      throw new DecodeError(`${path}[${i}]`, "a known region kind");
    }
    if (first > last)
      throw new DecodeError(`${path}[${i + 1}]`, "first ≤ last");
  }
}

export function decodeSourceView(value: unknown): SourceView {
  const path = "$";
  const o = object(value, path);
  const head: SourceHead = {
    generation: field(o, "generation", path, nat),
    path: field(o, "path", path, str),
    ref: field(o, "ref", path, nullable(str)),
    tier: field(o, "tier", path, decodeTier),
    partial: field(o, "partial", path, bool),
  };
  const highlight = field(
    o,
    "highlight",
    path,
    oneOf(["plain", "lexed"] as const),
  );
  if (highlight === "plain") return { ...head, highlight };
  const runs = field(o, "runs", path, array(nat));
  const regions = field(o, "regions", path, array(nat));
  const targets = field(o, "targets", path, array(decodeTarget));
  checkRuns(runs, targets.length, `${path}.runs`);
  checkRegions(regions, `${path}.regions`);
  return {
    ...head,
    highlight,
    runs,
    regions,
    targets,
    outline: field(o, "outline", path, array(decodeOutlineEntry)),
  };
}

export function decodeSourceNotFound(value: unknown): SourceNotFound {
  const path = "$";
  const o = object(value, path);
  return {
    generation: field(o, "generation", path, nat),
    kind: field(o, "kind", path, oneOf(["not-found"] as const)),
    what: field(o, "what", path, oneOf(["file"] as const)),
  };
}
