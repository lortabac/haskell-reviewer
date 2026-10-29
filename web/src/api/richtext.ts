// Rich text: Haddock comments as the module page sends them. Shape: see
// Note [Module page] in HaskellReviewer.Server.Wire.Page.
import {
  array,
  DecodeError,
  field,
  nat,
  nullable,
  object,
  oneOf,
  str,
} from "./decode";

export type Span = [number, number, number, number];

export const blockKinds = [
  "paragraph",
  "heading",
  "code",
  "bullets",
  "numbered",
  "definitions",
  "examples",
  "property",
  "table",
] as const;

export const inlineKinds = [
  "text",
  "emphasis",
  "bold",
  "code",
  "math",
  "link",
  "name",
  "module",
] as const;

export const mathStyles = ["inline", "display"] as const;
export const nameSpaces = ["value", "type", "any"] as const;

// A whole comment. since.version is like "1.2.3", decimal text (parts
// can be over 2^53). since is null too when it could be wrong.
export interface Doc {
  blocks: Block[];
  since: { package: string | null; version: string } | null;
}

export interface Cell {
  colspan: number;
  rowspan: number;
  inlines: Inline[];
}

export type Block =
  | { kind: "paragraph" | "code"; inlines: Inline[] }
  | { kind: "heading"; level: number; inlines: Inline[] }
  | { kind: "bullets"; items: Block[][] }
  // number: decimal text (can be over 2^53), or null: number the
  // items yourself (the comment had a number haddock-library got wrong).
  | { kind: "numbered"; items: { number: string | null; blocks: Block[] }[] }
  | { kind: "definitions"; items: { term: Inline[]; blocks: Block[] }[] }
  | { kind: "examples"; items: { expression: string; results: string[] }[] }
  | { kind: "property"; text: string }
  | { kind: "table"; header: Cell[][]; body: Cell[][] };

export type Inline =
  | { kind: "text"; text: string }
  | { kind: "emphasis" | "bold" | "code"; inlines: Inline[] }
  // TeX source: show it as text.
  | { kind: "math"; style: (typeof mathStyles)[number]; tex: string }
  // url: http or https only, checked here too.
  | { kind: "link"; url: string; label: Inline[] | null }
  // target: the handle of a declaration of this module, if the name is
  // one (module.ts, byHandle).
  | {
      kind: "name";
      written: string;
      ns: (typeof nameSpaces)[number];
      target: number | null;
    }
  // ref: the module's ref, if it's in the project. anchor as written.
  | {
      kind: "module";
      module: string;
      anchor: string | null;
      ref: string | null;
      label: Inline[] | null;
    };

// A Haddock comment and where it is.
export interface DocComment {
  span: Span;
  doc: Doc;
}

export function decodeSpan(value: unknown, path: string): Span {
  if (!Array.isArray(value) || value.length !== 4) {
    throw new DecodeError(path, "a span: 4 integers");
  }
  const n = (i: number) => nat(value[i], `${path}[${i}]`);
  return [n(0), n(1), n(2), n(3)];
}

// The server sends only http and https links; a bad one is a bad
// payload, not something to show.
function safeUrl(value: unknown, path: string): string {
  const url = str(value, path);
  if (!/^https?:\/\/\S/i.test(url)) {
    throw new DecodeError(path, "an http or https URL");
  }
  return url;
}

export function decodeInline(value: unknown, path: string): Inline {
  const o = object(value, path);
  const kind = field(o, "kind", path, oneOf(inlineKinds));
  switch (kind) {
    case "text":
      return { kind, text: field(o, "text", path, str) };
    case "emphasis":
    case "bold":
    case "code":
      return { kind, inlines: field(o, "inlines", path, decodeInlines) };
    case "math":
      return {
        kind,
        style: field(o, "style", path, oneOf(mathStyles)),
        tex: field(o, "tex", path, str),
      };
    case "link":
      return {
        kind,
        url: field(o, "url", path, safeUrl),
        label: field(o, "label", path, nullable(decodeInlines)),
      };
    case "name":
      return {
        kind,
        written: field(o, "written", path, str),
        ns: field(o, "ns", path, oneOf(nameSpaces)),
        target: field(o, "target", path, nullable(nat)),
      };
    case "module":
      return {
        kind,
        module: field(o, "module", path, str),
        anchor: field(o, "anchor", path, nullable(str)),
        ref: field(o, "ref", path, nullable(str)),
        label: field(o, "label", path, nullable(decodeInlines)),
      };
  }
}

export const decodeInlines = array(decodeInline);

function decodeCell(value: unknown, path: string): Cell {
  const o = object(value, path);
  return {
    colspan: field(o, "colspan", path, nat),
    rowspan: field(o, "rowspan", path, nat),
    inlines: field(o, "inlines", path, decodeInlines),
  };
}

// Decimal text: digits only.
function decimal(value: unknown, path: string): string {
  const s = str(value, path);
  if (!/^\d+$/.test(s)) throw new DecodeError(path, "decimal digits");
  return s;
}

export function decodeBlock(value: unknown, path: string): Block {
  const o = object(value, path);
  const kind = field(o, "kind", path, oneOf(blockKinds));
  switch (kind) {
    case "paragraph":
    case "code":
      return { kind, inlines: field(o, "inlines", path, decodeInlines) };
    case "heading":
      return {
        kind,
        level: field(o, "level", path, nat),
        inlines: field(o, "inlines", path, decodeInlines),
      };
    case "bullets":
      return { kind, items: field(o, "items", path, array(decodeBlocks)) };
    case "numbered":
      return {
        kind,
        items: field(
          o,
          "items",
          path,
          array((v, p) => {
            const i = object(v, p);
            return {
              number: field(i, "number", p, nullable(decimal)),
              blocks: field(i, "blocks", p, decodeBlocks),
            };
          }),
        ),
      };
    case "definitions":
      return {
        kind,
        items: field(
          o,
          "items",
          path,
          array((v, p) => {
            const i = object(v, p);
            return {
              term: field(i, "term", p, decodeInlines),
              blocks: field(i, "blocks", p, decodeBlocks),
            };
          }),
        ),
      };
    case "examples":
      return {
        kind,
        items: field(
          o,
          "items",
          path,
          array((v, p) => {
            const i = object(v, p);
            return {
              expression: field(i, "expression", p, str),
              results: field(i, "results", p, array(str)),
            };
          }),
        ),
      };
    case "property":
      return { kind, text: field(o, "text", path, str) };
    case "table":
      return {
        kind,
        header: field(o, "header", path, array(array(decodeCell))),
        body: field(o, "body", path, array(array(decodeCell))),
      };
  }
}

export const decodeBlocks = array(decodeBlock);

export function decodeDoc(value: unknown, path: string): Doc {
  const o = object(value, path);
  return {
    blocks: field(o, "blocks", path, decodeBlocks),
    since: field(
      o,
      "since",
      path,
      nullable((v, p) => {
        const s = object(v, p);
        return {
          package: field(s, "package", p, nullable(str)),
          version: field(s, "version", p, (x, q) => {
            const t = str(x, q);
            if (!/^\d+(\.\d+)*$/.test(t)) {
              throw new DecodeError(q, "a version like 1.2.3");
            }
            return t;
          }),
        };
      }),
    ),
  };
}

export function decodeDocComment(value: unknown, path: string): DocComment {
  const o = object(value, path);
  return {
    span: field(o, "span", path, decodeSpan),
    doc: field(o, "doc", path, decodeDoc),
  };
}
