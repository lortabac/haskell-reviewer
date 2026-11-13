// GET api/refs/<side>?module=&decl=: the references to one declaration
// (D-294). Shape: see encodeRefs in HaskellReviewer.Server.Wire.
import {
  DecodeError,
  array,
  bool,
  field,
  nat,
  nullable,
  object,
  oneOf,
  positive,
  str,
} from "./decode";
import { decodeSpan, type Span } from "./richtext";
import { reasonCodes, type ReasonCode } from "./source";

export const refKinds = ["use", "import", "export", "binding"] as const;

export type RefKind = (typeof refKinds)[number];

export type Ref = {
  // The module's name (as search has it), its ref for the URL, its file.
  module: string;
  ref: string;
  path: string | null;
  // The token: 1-based, end exclusive, columns in code points.
  span: Span;
  kind: RefKind;
  // A guess, and why; reasons is empty when exact.
  heuristic: boolean;
  reasons: ReasonCode[];
  // The token's line, cut to 240 code points around it; lineCol is the
  // column of its first code point.
  line: string;
  lineCol: number;
};

export type Refs = {
  generation: number;
  // The declaration's module ref, and its id as asked.
  module: string;
  decl: string;
  // Still indexing, or a module read isn't ready: ask again later.
  partial: boolean;
  // All references; results has the first 1000 at most.
  total: number;
  results: Ref[];
};

function decodeRef(value: unknown, path: string): Ref {
  const o = object(value, path);
  const reasons = field(o, "reasons", path, array(oneOf(reasonCodes)));
  const heuristic = field(o, "heuristic", path, bool);
  if (heuristic !== reasons.length > 0) {
    throw new DecodeError(`${path}.reasons`, "reasons for guesses only");
  }
  return {
    module: field(o, "module", path, str),
    ref: field(o, "ref", path, str),
    path: field(o, "path", path, nullable(str)),
    span: field(o, "span", path, decodeSpan),
    kind: field(o, "kind", path, oneOf(refKinds)),
    heuristic,
    reasons,
    line: field(o, "line", path, str),
    lineCol: field(o, "lineCol", path, positive),
  };
}

export function decodeRefs(value: unknown, path = "$"): Refs {
  const o = object(value, path);
  const refs = {
    generation: field(o, "generation", path, nat),
    module: field(o, "module", path, str),
    decl: field(o, "decl", path, str),
    partial: field(o, "partial", path, bool),
    total: field(o, "total", path, nat),
    results: field(o, "results", path, array(decodeRef)),
  };
  if (refs.results.length > refs.total) {
    throw new DecodeError(path, "results.length <= total");
  }
  return refs;
}

// The 404 body: an unknown module, or a name it doesn't declare.
export type RefsNotFound = {
  generation: number;
  kind: "not-found";
  what: "module" | "decl";
};

export function decodeRefsNotFound(value: unknown): RefsNotFound {
  const path = "$";
  const o = object(value, path);
  return {
    generation: field(o, "generation", path, nat),
    kind: field(o, "kind", path, oneOf(["not-found"] as const)),
    what: field(o, "what", path, oneOf(["module", "decl"] as const)),
  };
}

// The route for a declaration, relative to the token, for getJson.
export function refsUrl(moduleRef: string, decl: string): string {
  return (
    `/api/refs/tree?module=${encodeURIComponent(moduleRef)}` +
    `&decl=${encodeURIComponent(decl)}`
  );
}
