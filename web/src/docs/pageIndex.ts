// Lookups on one module page, built once per page: declarations by
// handle, by id, by Haddock name, parents, instance homes, shared
// pieces. See Notes [Handles] and [Sent once] in
// HaskellReviewer.Server.Wire.Page.
import type { Code } from "../api/code";
import type {
  AnyDecl,
  Constructor,
  Handle,
  ParsedModule,
  Signature,
  SourcePart,
  Span,
  WarningText,
  WireId,
} from "../api/module";
import type { DocComment } from "../api/richtext";

// Every declaration of the page with a handle, nested ones too, in
// page order: constructors, fields, methods, associated types and
// instances, pattern synonym fields. With the parent of each nested
// one.
function walk(
  page: ParsedModule,
  visit: (d: AnyDecl, parent: AnyDecl | null) => void,
): void {
  const inType = (t: AnyDecl & { constructors: Constructor[] }) => {
    for (const c of t.constructors) {
      visit(c, t);
      for (const f of c.fields) visit(f, c);
    }
  };
  for (const d of page.declarations) {
    if (d.kind === "splice") continue;
    visit(d, null);
    switch (d.kind) {
      case "type":
      case "faminst":
        inType(d);
        break;
      case "class":
        for (const m of d.methods) visit(m, d);
        for (const a of d.associated) {
          visit(a, d);
          inType(a);
        }
        break;
      case "patsyn":
        for (const f of d.fields) visit(f, d);
        break;
      case "instance":
        for (const a of d.associated) {
          visit(a, d);
          inType(a);
        }
        break;
      case "value":
        break;
    }
  }
}

export function allDecls(page: ParsedModule): AnyDecl[] {
  const out: AnyDecl[] = [];
  walk(page, (d) => out.push(d));
  return out;
}

// An id as text, like type:T, field:T.f or inst:Show:Maybe a.
export function idText(page: ParsedModule, id: WireId): string {
  if (typeof id === "string") return id;
  const part = (k: number) => page.idParts[k] ?? "";
  return "field" in id
    ? "field:" + part(id.parent) + "." + id.field
    : "inst:" + id.inst + ":" + id.head.map(part).join("");
}

// Haddock's anchor namespaces: v: for values (functions, methods,
// constructors, fields, pattern synonyms), t: for types and classes
// (and type data constructors).
const valueSpaces = new Set(["val", "con", "field", "pat"]);
const typeSpaces = new Set(["type", "class"]);

export interface PageIndex {
  page: ParsedModule;
  // Every declaration, page order. A repeated handle (a CPP macro
  // making one declaration twice) is listed once: the first.
  decls: AnyDecl[];
  byHandle: ReadonlyMap<Handle, AnyDecl>;
  // Id text of a declaration of the page.
  idOf: (d: AnyDecl) => string;
  // Id text of a handle, if the page has it.
  idOfHandle: (n: Handle) => string | undefined;
  // First in page order: ids aren't unique (a field in several
  // constructors).
  byId: ReadonlyMap<string, AnyDecl>;
  // The declaration a nested one is in (a field: its constructor).
  parentOf: (d: AnyDecl) => AnyDecl | undefined;
  // The top-level declaration it is in, or itself.
  topOf: (d: AnyDecl) => AnyDecl;
  // For Haddock anchors v:x and t:T: first in page order.
  byName: {
    value: ReadonlyMap<string, AnyDecl>;
    type: ReadonlyMap<string, AnyDecl>;
  };
  // Instance handle -> the first type, class or family that lists it.
  instanceHome: ReadonlyMap<Handle, AnyDecl>;
  // Shared pieces (Note [Sent once]): the piece as the sender has it.
  sentDoc: (x: { span: Span; sharedWith: Handle }) => DocComment | undefined;
  sentWarning: (x: {
    span: Span;
    sharedWith: Handle;
  }) => WarningText | undefined;
  sentPart: (x: {
    span: Span;
    sharedWith: Handle;
  }) => (SourcePart & { text: string }) | undefined;
  sentSignature: (
    sharedWith: Handle,
  ) => { code: Code; names: string[] } | undefined;
  sentFieldType: (sharedWith: Handle) => Code | undefined;
  sentVia: (sharedWith: Handle) => Code | undefined;
}

const sameSpan = (a: Span, b: Span) =>
  a[0] === b[0] && a[1] === b[1] && a[2] === b[2] && a[3] === b[3];

const cache = new WeakMap<ParsedModule, PageIndex>();

// The index of a page, built on first use.
export function pageIndex(page: ParsedModule): PageIndex {
  let ix = cache.get(page);
  if (ix === undefined) {
    ix = build(page);
    cache.set(page, ix);
  }
  return ix;
}

function build(page: ParsedModule): PageIndex {
  const decls: AnyDecl[] = [];
  const byHandle = new Map<Handle, AnyDecl>();
  const parents = new Map<AnyDecl, AnyDecl>();
  const ids = new Map<AnyDecl, string>();
  const byId = new Map<string, AnyDecl>();
  const value = new Map<string, AnyDecl>();
  const type = new Map<string, AnyDecl>();
  const instanceHome = new Map<Handle, AnyDecl>();
  const setFirst = <K, V>(m: Map<K, V>, k: K, v: V) => {
    if (!m.has(k)) m.set(k, v);
  };
  walk(page, (d, parent) => {
    // Handles can repeat under CPP: the first wins, the copy is
    // skipped (its nested ones are copies too).
    if (byHandle.has(d.n)) return;
    byHandle.set(d.n, d);
    decls.push(d);
    if (parent !== null) parents.set(d, parent);
    const id = idText(page, d.id);
    ids.set(d, id);
    setFirst(byId, id, d);
    const ns = id.slice(0, id.indexOf(":"));
    // type data constructors are types (Haddock puts them under t:).
    const typeData =
      ns === "con" &&
      parent !== null &&
      "form" in parent &&
      parent.form === "type-data";
    if (d.name !== null) {
      if (typeData) setFirst(type, d.name, d);
      else if (valueSpaces.has(ns)) setFirst(value, d.name, d);
      else if (typeSpaces.has(ns)) setFirst(type, d.name, d);
    }
    if ("instances" in d) {
      for (const n of d.instances) setFirst(instanceHome, n, d);
    }
  });
  const idOf = (d: AnyDecl): string => ids.get(d) ?? idText(page, d.id);
  const parentOf = (d: AnyDecl) => parents.get(d);
  const topOf = (d: AnyDecl): AnyDecl => {
    let x = d;
    for (let p = parents.get(x); p !== undefined; p = parents.get(x)) x = p;
    return x;
  };
  const sender = (n: Handle) => byHandle.get(n);
  return {
    page,
    decls,
    byHandle,
    idOf,
    idOfHandle: (n) => {
      const d = byHandle.get(n);
      return d === undefined ? undefined : idOf(d);
    },
    byId,
    parentOf,
    topOf,
    byName: { value, type },
    instanceHome,
    sentDoc: (x) => {
      for (const c of sender(x.sharedWith)?.docs ?? []) {
        if ("doc" in c && sameSpan(c.span, x.span)) return c;
      }
      return undefined;
    },
    sentWarning: (x) => {
      const w = sender(x.sharedWith)?.warning;
      return w && !("sharedWith" in w) && sameSpan(w.span, x.span)
        ? w
        : undefined;
    },
    sentPart: (x) => {
      const s = sender(x.sharedWith);
      if (s === undefined || !("source" in s)) return undefined;
      for (const p of s.source) {
        if ("text" in p && sameSpan(p.span, x.span)) return p;
      }
      return undefined;
    },
    sentSignature: (n) => {
      const s = sender(n);
      if (s === undefined || !("signature" in s)) return undefined;
      const sig: Signature | null = s.signature;
      return sig !== null && "code" in sig ? sig : undefined;
    },
    sentFieldType: (n) => {
      const s = sender(n);
      return s !== undefined && "type" in s && Array.isArray(s.type)
        ? s.type
        : undefined;
    },
    sentVia: (n) => {
      const s = sender(n);
      if (s === undefined || !("form" in s) || typeof s.form !== "object") {
        return undefined;
      }
      const via = s.form.via;
      return Array.isArray(via) ? via : undefined;
    },
  };
}
