// The body of a module page as a list of blocks (SPEC §4.2): headings,
// docs and declarations, in one of three modes.
//
// A. The body has headings (-- * H): source order, headings and named
//    chunks in place. A declaration is placed by its first part that
//    isn't a pragma (a fixity or DEPRECATED block can be far away).
// B. No body headings, but the export list has sections: export list
//    order, each declaration once; names not defined here are
//    "x (re-exported)"; then "Private" in source order (D-203).
// C. Neither: one "Declarations" heading, source order.
//
// Instances and family instances are never in the flow: under their
// type, class or family, or in the Instances block (unattached, or
// their home is hidden). Heading levels never skip one (h2 then h4).
// Linear in the page.
import type {
  AnyDecl,
  Declaration,
  DocItem,
  Export,
  Item,
  ItemName,
  Located,
  ParsedModule,
  Span,
  WarningText,
} from "../api/module";
import type { Doc, Inline } from "../api/richtext";
import type { PageIndex } from "./pageIndex";

export type Mode = "A" | "B" | "C";

// depth: the level of the heading it sits under, 0 for none.
export type Block =
  | { kind: "heading"; level: number; inlines: Inline[] }
  | { kind: "doc"; depth: number; doc: Doc }
  | { kind: "warning"; warning: WarningText }
  | { kind: "decl"; depth: number; decl: Declaration }
  // An export of a name this module doesn't define.
  | { kind: "reexport"; item: Item }
  // link: false when an import names another module "as X".
  | { kind: "module"; name: Located; link: boolean }
  // An export of a nested name (a method, a field): see its parent.
  | { kind: "nested"; name: ItemName; decl: AnyDecl };

export interface Body {
  mode: Mode;
  blocks: Block[];
  // Instances with no home shown, source order.
  instances: Declaration[];
  // Instance (or family instance) handle -> its home: the first shown
  // type, class or family that lists it, page order.
  homes: ReadonlyMap<number, AnyDecl>;
}

const text = (t: string): Inline[] => [{ kind: "text", text: t }];

// In the main flow: not an instance.
export function inFlow(d: Declaration): boolean {
  return d.kind !== "instance" && d.kind !== "faminst";
}

// Something in it is exported: a constructor, field, method,
// associated type or pattern synonym field.
function exportsNested(d: Declaration): boolean {
  const any = (xs: { exported: boolean }[]) => xs.some((x) => x.exported);
  switch (d.kind) {
    case "type":
      return d.constructors.some((c) => c.exported || any(c.fields));
    case "class":
      return any(d.methods) || any(d.associated);
    case "patsyn":
      return any(d.fields);
    default:
      return false;
  }
}

// Shown at all. With exportedOnly: exported, or with something
// exported in it (a method exported alone needs its class).
export const shown = (d: Declaration, exportedOnly: boolean): boolean =>
  !exportedOnly || d.exported || exportsNested(d);

// How many top-level declarations "exported only" would drop, for the
// toolbar's status (D-204). Independent of the current setting.
// Declarations only: an instance or family instance the toggle also
// hides (its listers all hidden, D-204) isn't counted here.
export function hiddenCount(page: ParsedModule): number {
  return page.declarations.filter((d) => inFlow(d) && !shown(d, true)).length;
}

// Heading levels as written, made so none is skipped: a level is at
// most one more than the last one.
function levels(): (written: number) => number {
  let last = 0;
  return (written) => {
    last = Math.max(1, Math.min(written, last + 1));
    return last;
  };
}

export function body(
  page: ParsedModule,
  ix: PageIndex,
  exportedOnly = false,
): Body {
  const flow = page.declarations.filter(
    (d) => inFlow(d) && shown(d, exportedOnly),
  );
  // Homes among the shown ones (with exportedOnly the first lister
  // can be hidden and a later one shown).
  const homes = new Map<number, AnyDecl>();
  const list = (owner: AnyDecl, ns: number[]) => {
    for (const n of ns) if (!homes.has(n)) homes.set(n, owner);
  };
  for (const d of flow) {
    if (d.kind === "type") list(d, d.instances);
    if (d.kind === "class") {
      list(d, d.instances);
      for (const a of d.associated) list(a, a.instances);
    }
  }
  // No shown home: the block. With exportedOnly, one attached (some
  // local type or class lists it) whose listers are all hidden is
  // dropped instead (D-204): it isn't an orphan, just invisible along
  // with its home. A true orphan (unattached, D-191) always stays.
  const instances = page.declarations.filter((d) => {
    if (d.kind !== "instance" && d.kind !== "faminst") return false;
    if (homes.has(d.n)) return false;
    return !(exportedOnly && d.attached);
  });
  const hasHeading = page.docItems.some((i) => i.kind === "heading");
  if (hasHeading) {
    return {
      mode: "A",
      blocks: byHeadings(flow, page.docItems),
      instances,
      homes,
    };
  }
  const exps = page.exports;
  if (exps !== null && exps.some((e) => e.kind === "section")) {
    return {
      mode: "B",
      blocks: byExports(flow, page, exps, ix),
      instances,
      homes,
    };
  }
  const blocks: Block[] = [];
  if (flow.length > 0) {
    blocks.push({ kind: "heading", level: 1, inlines: text("Declarations") });
  }
  for (const d of flow) blocks.push({ kind: "decl", depth: 1, decl: d });
  return { mode: "C", blocks, instances, homes };
}

// Where a declaration is placed: its first part that isn't a pragma
// (docs included).
export function startOf(d: Declaration): [number, number] {
  let best: Span | undefined;
  for (const p of d.source) {
    if (p.role === "pragma") continue;
    if (
      best === undefined ||
      p.span[0] < best[0] ||
      (p.span[0] === best[0] && p.span[1] < best[1])
    ) {
      best = p.span;
    }
  }
  return best === undefined ? [d.line, 1] : [best[0], best[1]];
}

const before = (a: [number, number], b: [number, number]) =>
  a[0] < b[0] || (a[0] === b[0] && a[1] < b[1]);

// Mode A: merge declarations and doc items by position.
function byHeadings(flow: Declaration[], items: DocItem[]): Block[] {
  const sorted = [...items].sort((a, b) =>
    before([a.span[0], a.span[1]], [b.span[0], b.span[1]]) ? -1 : 1,
  );
  const blocks: Block[] = [];
  const level = levels();
  let depth = 0;
  let i = 0;
  const flushUntil = (pos: [number, number] | undefined) => {
    for (; i < sorted.length; i++) {
      const item = sorted[i];
      if (item === undefined) break;
      if (pos !== undefined && !before([item.span[0], item.span[1]], pos)) {
        break;
      }
      if (item.kind === "heading") {
        depth = level(item.level);
        blocks.push({ kind: "heading", level: depth, inlines: item.inlines });
      } else {
        blocks.push({ kind: "doc", depth, doc: item.doc });
      }
    }
  };
  // Sorted by where each is placed (stable: ties keep source order).
  const placed = flow.map((d) => ({ d, at: startOf(d) }));
  placed.sort((a, b) => (before(a.at, b.at) ? -1 : before(b.at, a.at) ? 1 : 0));
  for (const { d, at } of placed) {
    flushUntil(at);
    blocks.push({ kind: "decl", depth, decl: d });
  }
  flushUntil(undefined);
  return blocks;
}

type Space = "value" | "type" | "pattern";

// Where a name exported alone may be nested (a method, a field, an
// associated type): its namespace only.
function nestedSpaces(name: ItemName): ("value" | "type")[] {
  switch (name.keyword) {
    case "type":
    case "default":
      return ["type"];
    case "pattern":
    case "data":
      return ["value"];
    case null:
      break;
  }
  const c = name.occ.charAt(0);
  if (c === "_" || /\p{Ll}/u.test(c)) return ["value"];
  if (/[\p{Lu}\p{Lt}]/u.test(c) || c === ":") return ["type"];
  return ["value", "type"];
}

// Where an export list name may be, most likely first.
function spaces(name: ItemName): Space[] {
  switch (name.keyword) {
    case "type":
    case "default":
      return ["type"];
    case "pattern":
      return ["pattern"];
    case "data":
      return ["value", "pattern"];
    case null:
      break;
  }
  const c = name.occ.charAt(0);
  if (c === "_" || /\p{Ll}/u.test(c)) return ["value"];
  if (/[\p{Lu}\p{Lt}]/u.test(c) || c === ":") return ["type", "pattern"];
  return ["value", "type"];
}

// Mode B: export list order.
function byExports(
  flow: Declaration[],
  page: ParsedModule,
  exps: Export[],
  ix: PageIndex,
): Block[] {
  const self = page.header.name?.value ?? null;
  const names: Record<Space, Map<string, Declaration>> = {
    value: new Map(),
    type: new Map(),
    pattern: new Map(),
  };
  for (const d of flow) {
    if (d.name === null) continue;
    const space: Space | undefined =
      d.kind === "value"
        ? "value"
        : d.kind === "type" || d.kind === "class"
          ? "type"
          : d.kind === "patsyn"
            ? "pattern"
            : undefined;
    if (space !== undefined && !names[space].has(d.name)) {
      names[space].set(d.name, d);
    }
  }
  const chunks = new Map<string, Doc>();
  for (const i of page.docItems) {
    if (i.kind === "chunk" && !chunks.has(i.name)) chunks.set(i.name, i.doc);
  }
  // The local declaration an item names, if any.
  const local = (n: ItemName) => n.qualifier === null || n.qualifier === self;
  const find = (n: ItemName): Declaration | undefined => {
    if (!local(n)) return undefined;
    for (const s of spaces(n)) {
      const d = names[s].get(n.occ);
      if (d !== undefined) return d;
    }
    return undefined;
  };
  // Named on their own somewhere: module Self leaves them to their
  // place.
  const named = new Set<Declaration>();
  for (const e of exps) {
    if (e.kind !== "item") continue;
    const d = find(e.name);
    if (d !== undefined) named.add(d);
  }
  // Modules some import names "as X": module X is not that module.
  const aliases = new Set<string>();
  for (const i of page.imports) {
    if (i.as !== null && i.as.value !== i.name.value) aliases.add(i.as.value);
  }
  const done = new Set<Declaration>();
  const blocks: Block[] = [];
  const level = levels();
  let depth = 0;
  const add = (d: Declaration) => {
    if (done.has(d)) return;
    done.add(d);
    blocks.push({ kind: "decl", depth, decl: d });
  };
  for (const e of exps) {
    switch (e.kind) {
      case "section":
        depth = level(e.level);
        blocks.push({
          kind: "heading",
          level: depth,
          inlines: e.heading.inlines,
        });
        break;
      case "doc":
        blocks.push({ kind: "doc", depth, doc: e.doc });
        break;
      case "named-doc": {
        const doc = chunks.get(e.name);
        if (doc !== undefined) blocks.push({ kind: "doc", depth, doc });
        break;
      }
      case "module":
        if (e.name.value === self) {
          // module M in M's own list: all it exports, but what the
          // list names on its own.
          for (const d of flow) if (d.exported && !named.has(d)) add(d);
        } else {
          blocks.push({
            kind: "module",
            name: e.name,
            link: !aliases.has(e.name.value),
          });
        }
        if (e.warning !== null) {
          blocks.push({ kind: "warning", warning: e.warning });
        }
        break;
      case "item": {
        const n = e.name;
        const d = find(n);
        if (d !== undefined) {
          // Listed twice: shown once, its docs and warnings each time.
          add(d);
        } else if (!local(n)) {
          blocks.push({ kind: "reexport", item: e });
        } else {
          let nested: AnyDecl | undefined;
          for (const s of nestedSpaces(n)) {
            nested = ix.byName[s].get(n.occ);
            if (nested !== undefined) break;
          }
          blocks.push(
            nested === undefined
              ? { kind: "reexport", item: e }
              : { kind: "nested", name: n, decl: nested },
          );
        }
        if (e.doc !== null) blocks.push({ kind: "doc", depth, doc: e.doc.doc });
        if (e.warning !== null) {
          blocks.push({ kind: "warning", warning: e.warning });
        }
        break;
      }
    }
  }
  // Exported, but the list didn't name it (an odd page): still shown.
  const others = flow.filter((d) => d.exported && !done.has(d));
  if (others.length > 0) {
    blocks.push({ kind: "heading", level: 1, inlines: text("Also exported") });
    for (const d of others) blocks.push({ kind: "decl", depth: 1, decl: d });
  }
  // With exportedOnly, flow has only the ones with something exported
  // in them (a method exported alone).
  const hidden = flow.filter((d) => !d.exported && !done.has(d));
  if (hidden.length > 0) {
    blocks.push({ kind: "heading", level: 1, inlines: text("Private") });
    for (const d of hidden) blocks.push({ kind: "decl", depth: 1, decl: d });
  }
  return blocks;
}

export type SynopsisDecl = Extract<
  Declaration,
  { kind: "type" | "class" | "value" | "patsyn" }
>;

// The synopsis: top-level types, classes, values and pattern synonyms,
// source order.
export function synopsis(
  page: ParsedModule,
  exportedOnly = false,
): SynopsisDecl[] {
  return page.declarations.filter(
    (d): d is SynopsisDecl =>
      (d.kind === "type" ||
        d.kind === "class" ||
        d.kind === "value" ||
        d.kind === "patsyn") &&
      shown(d, exportedOnly),
  );
}
