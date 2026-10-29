// One module, from the index. Shape: see encodeModuleView in
// HaskellReviewer.Server.Wire.
import {
  array,
  bool,
  DecodeError,
  field,
  type Fields,
  int,
  nat,
  nullable,
  object,
  oneOf,
  str,
} from "./decode";
import { noSourceReasons, type NoSourceReason } from "./modules";

// [startLine, startCol, endLine, endCol]: 1-based, columns in code
// points, end exclusive.
export type Span = [number, number, number, number];

export interface Located {
  span: Span;
  value: string;
}

// A Haddock comment as written, without the | or ^.
export interface RawDoc {
  span: Span;
  text: string;
}

export const warningKinds = ["deprecated", "warning"] as const;

// A DEPRECATED or WARNING pragma. category is null for DEPRECATED.
export interface WarningText {
  span: Span;
  kind: (typeof warningKinds)[number];
  category: string | null;
  messages: string[];
}

export const nameKeywords = ["type", "pattern", "data", "default"] as const;

export interface ItemName {
  span: Span;
  qualifier: string | null;
  // Operators without parens.
  occ: string;
  keyword: (typeof nameKeywords)[number] | null;
}

export const subsKinds = ["none", "all", "some"] as const;

export type Subs =
  | { kind: "none" }
  | { kind: "all" }
  // wildcardAt: where ".." is among the names, if it is.
  | { kind: "some"; names: ItemName[]; wildcardAt: number | null };

// A name in an export or import list. doc and warning are for exports.
export interface Item {
  span: Span;
  name: ItemName;
  subs: Subs;
  doc: RawDoc | null;
  warning: WarningText | null;
}

export const exportKinds = [
  "item",
  "module",
  "section",
  "doc",
  "named-doc",
] as const;

export type Export =
  | ({ kind: "item" } & Item)
  | { kind: "module"; name: Located; warning: WarningText | null }
  | { kind: "section"; level: number; heading: RawDoc }
  | { kind: "doc"; span: Span; text: string }
  // -- $name: a chunk defined further down.
  | { kind: "named-doc"; span: Span; name: string };

export const qualifiedKinds = ["no", "pre", "post"] as const;
export const importLevels = ["normal", "splice", "quote"] as const;
export const listKinds = ["only", "hiding"] as const;

export interface Import {
  span: Span;
  name: Located;
  package: string | null;
  // import {-# SOURCE #-}
  source: boolean;
  safe: boolean;
  qualified: (typeof qualifiedKinds)[number];
  level: (typeof importLevels)[number];
  as: Located | null;
  list: { kind: (typeof listKinds)[number]; items: Item[] } | null;
}

export interface Extensions {
  language: string;
  enabled: string[];
  fromPragmas: string[];
}

export interface ParsedModule {
  kind: "parsed";
  header: {
    // null: no module line.
    name: Located | null;
    haddock: RawDoc | null;
    warning: WarningText | null;
  };
  // null: no export list.
  exports: Export[] | null;
  imports: Import[];
  extensions: Extensions;
  warnings: string[];
}

export const reasonKinds = [
  "parse-error",
  "pragma-error",
  "preprocessed",
  "parser-crashed",
  "too-slow",
  "generated",
  "no-source",
  "not-text",
  "too-large",
] as const;

// Why a module is text only.
export type TextOnlyReason =
  | {
      kind: "parse-error" | "pragma-error";
      message: string;
      code: string | null;
      span: Span | null;
    }
  // -F -pgmF or -pgmL; generated: a preprocessor input (alex, ...).
  | { kind: "preprocessed" | "generated"; tool: string }
  | { kind: "parser-crashed"; message: string }
  // The parse (CPP and GHC) took too long.
  | { kind: "too-slow" }
  | { kind: "no-source"; reason: NoSourceReason }
  | { kind: "not-text"; size: number }
  | { kind: "too-large"; size: number; limit: number };

export interface TextOnlyModule {
  kind: "text-only";
  reason: TextOnlyReason;
  warnings: string[];
}

export interface ModuleView {
  generation: number;
  id: string;
  ref: string;
  // null when there is no source.
  path: string | null;
  // No HIE files yet: always this.
  tier: { kind: "syntactic"; reason: "no-hie" };
  parse: ParsedModule | TextOnlyModule;
}

function decodeSpan(value: unknown, path: string): Span {
  if (!Array.isArray(value) || value.length !== 4) {
    throw new DecodeError(path, "a span: 4 integers");
  }
  const n = (i: number) => nat(value[i], `${path}[${i}]`);
  return [n(0), n(1), n(2), n(3)];
}

function decodeLocated(value: unknown, path: string): Located {
  const o = object(value, path);
  return {
    span: field(o, "span", path, decodeSpan),
    value: field(o, "value", path, str),
  };
}

function decodeDoc(value: unknown, path: string): RawDoc {
  const o = object(value, path);
  return {
    span: field(o, "span", path, decodeSpan),
    text: field(o, "text", path, str),
  };
}

function decodeWarning(value: unknown, path: string): WarningText {
  const o = object(value, path);
  return {
    span: field(o, "span", path, decodeSpan),
    kind: field(o, "kind", path, oneOf(warningKinds)),
    category: field(o, "category", path, nullable(str)),
    messages: field(o, "messages", path, array(str)),
  };
}

function decodeItemName(value: unknown, path: string): ItemName {
  const o = object(value, path);
  return {
    span: field(o, "span", path, decodeSpan),
    qualifier: field(o, "qualifier", path, nullable(str)),
    occ: field(o, "occ", path, str),
    keyword: field(o, "keyword", path, nullable(oneOf(nameKeywords))),
  };
}

function decodeSubs(value: unknown, path: string): Subs {
  const o = object(value, path);
  const kind = field(o, "kind", path, oneOf(subsKinds));
  switch (kind) {
    case "none":
    case "all":
      return { kind };
    case "some":
      return {
        kind,
        names: field(o, "names", path, array(decodeItemName)),
        wildcardAt: field(o, "wildcardAt", path, nullable(nat)),
      };
  }
}

function itemFields(o: Fields, path: string): Item {
  return {
    span: field(o, "span", path, decodeSpan),
    name: field(o, "name", path, decodeItemName),
    subs: field(o, "subs", path, decodeSubs),
    doc: field(o, "doc", path, nullable(decodeDoc)),
    warning: field(o, "warning", path, nullable(decodeWarning)),
  };
}

function decodeItem(value: unknown, path: string): Item {
  return itemFields(object(value, path), path);
}

function decodeExport(value: unknown, path: string): Export {
  const o = object(value, path);
  const kind = field(o, "kind", path, oneOf(exportKinds));
  switch (kind) {
    case "item":
      return { kind, ...itemFields(o, path) };
    case "module":
      return {
        kind,
        name: field(o, "name", path, decodeLocated),
        warning: field(o, "warning", path, nullable(decodeWarning)),
      };
    case "section":
      return {
        kind,
        level: field(o, "level", path, int),
        heading: field(o, "heading", path, decodeDoc),
      };
    case "doc":
      return { kind, ...decodeDoc(o, path) };
    case "named-doc":
      return {
        kind,
        span: field(o, "span", path, decodeSpan),
        name: field(o, "name", path, str),
      };
  }
}

function decodeImportList(value: unknown, path: string): Import["list"] {
  const o = object(value, path);
  return {
    kind: field(o, "kind", path, oneOf(listKinds)),
    items: field(o, "items", path, array(decodeItem)),
  };
}

function decodeImport(value: unknown, path: string): Import {
  const o = object(value, path);
  return {
    span: field(o, "span", path, decodeSpan),
    name: field(o, "name", path, decodeLocated),
    package: field(o, "package", path, nullable(str)),
    source: field(o, "source", path, bool),
    safe: field(o, "safe", path, bool),
    qualified: field(o, "qualified", path, oneOf(qualifiedKinds)),
    level: field(o, "level", path, oneOf(importLevels)),
    as: field(o, "as", path, nullable(decodeLocated)),
    list: field(o, "list", path, nullable(decodeImportList)),
  };
}

function decodeExtensions(value: unknown, path: string): Extensions {
  const o = object(value, path);
  return {
    language: field(o, "language", path, str),
    enabled: field(o, "enabled", path, array(str)),
    fromPragmas: field(o, "fromPragmas", path, array(str)),
  };
}

function decodeHeader(value: unknown, path: string): ParsedModule["header"] {
  const o = object(value, path);
  return {
    name: field(o, "name", path, nullable(decodeLocated)),
    haddock: field(o, "haddock", path, nullable(decodeDoc)),
    warning: field(o, "warning", path, nullable(decodeWarning)),
  };
}

function decodeReason(value: unknown, path: string): TextOnlyReason {
  const o = object(value, path);
  const kind = field(o, "kind", path, oneOf(reasonKinds));
  switch (kind) {
    case "parse-error":
    case "pragma-error":
      return {
        kind,
        message: field(o, "message", path, str),
        code: field(o, "code", path, nullable(str)),
        span: field(o, "span", path, nullable(decodeSpan)),
      };
    case "preprocessed":
    case "generated":
      return { kind, tool: field(o, "tool", path, str) };
    case "parser-crashed":
      return { kind, message: field(o, "message", path, str) };
    case "too-slow":
      return { kind };
    case "no-source":
      return { kind, reason: field(o, "reason", path, oneOf(noSourceReasons)) };
    case "not-text":
      return { kind, size: field(o, "size", path, nat) };
    case "too-large":
      return {
        kind,
        size: field(o, "size", path, nat),
        limit: field(o, "limit", path, nat),
      };
  }
}

const parseKinds = ["parsed", "text-only"] as const;

function decodeParse(
  value: unknown,
  path: string,
): ParsedModule | TextOnlyModule {
  const o = object(value, path);
  const kind = field(o, "kind", path, oneOf(parseKinds));
  switch (kind) {
    case "parsed":
      return {
        kind,
        header: field(o, "header", path, decodeHeader),
        exports: field(o, "exports", path, nullable(array(decodeExport))),
        imports: field(o, "imports", path, array(decodeImport)),
        extensions: field(o, "extensions", path, decodeExtensions),
        warnings: field(o, "warnings", path, array(str)),
      };
    case "text-only":
      return {
        kind,
        reason: field(o, "reason", path, decodeReason),
        warnings: field(o, "warnings", path, array(str)),
      };
  }
}

function decodeTier(value: unknown, path: string): ModuleView["tier"] {
  const o = object(value, path);
  return {
    kind: field(o, "kind", path, oneOf(["syntactic"] as const)),
    reason: field(o, "reason", path, oneOf(["no-hie"] as const)),
  };
}

export function decodeModuleView(value: unknown): ModuleView {
  const path = "$";
  const o = object(value, path);
  return {
    generation: field(o, "generation", path, nat),
    id: field(o, "id", path, str),
    ref: field(o, "ref", path, str),
    path: field(o, "path", path, nullable(str)),
    tier: field(o, "tier", path, decodeTier),
    parse: field(o, "parse", path, decodeParse),
  };
}

// The 404 body of an unknown module: see encodeModuleNotFound in
// HaskellReviewer.Server.Wire.
export interface ModuleNotFound {
  generation: number;
  kind: "not-found";
  what: "module";
}

export function decodeModuleNotFound(value: unknown): ModuleNotFound {
  const path = "$";
  const o = object(value, path);
  return {
    generation: field(o, "generation", path, nat),
    kind: field(o, "kind", path, oneOf(["not-found"] as const)),
    what: field(o, "what", path, oneOf(["module"] as const)),
  };
}
