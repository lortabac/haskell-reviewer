// One module's page (CBOR). Shape: see Note [Module page] in
// HaskellReviewer.Server.Wire.Page.
import { type Code, decodeCode } from "./code";
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
import { pageIndex } from "../docs/pageIndex";
import { encodePath } from "../files/paths";
import { noSourceReasons, type NoSourceReason } from "./modules";
import {
  decodeDocComment,
  decodeInlines,
  decodeSpan,
  type DocComment,
  type Inline,
  type Span,
} from "./richtext";

// [startLine, startCol, endLine, endCol]: 1-based, columns in code
// points, end exclusive.
export type { Span };

export interface Located {
  span: Span;
  value: string;
}

// A section heading: inline text only.
export interface Heading {
  span: Span;
  inlines: Inline[];
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
  doc: DocComment | null;
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
  | { kind: "section"; level: number; heading: Heading }
  | { kind: "doc"; span: Span; doc: DocComment["doc"] }
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
    haddock: DocComment | null;
    warning: WarningText | null;
  };
  // null: no export list.
  exports: Export[] | null;
  imports: Import[];
  extensions: Extensions;
  warnings: string[];
  // The long parts of ids (WireId, idText).
  idParts: string[];
  // In source order.
  declarations: Declaration[];
  // Headings and named chunks among the declarations.
  docItems: DocItem[];
  // The Pragmas block, in source order.
  pragmas: Pragma[];
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

export interface ModulePage {
  generation: number;
  id: string;
  ref: string;
  // null when there is no source.
  path: string | null;
  // null for a loose module. version null: not in the index any more.
  package: { name: string; version: string | null } | null;
  component: string | null;
  // No HIE files yet: always this.
  tier: { kind: "syntactic"; reason: "no-hie" };
  parse: ParsedModule | TextOnlyModule;
}

// * Declarations

export const fixityDirections = ["infixl", "infixr", "infix"] as const;

export interface Fixity {
  direction: (typeof fixityDirections)[number];
  precedence: number;
}

// A declaration's handle: unique on the page (ids aren't). Every
// reference to a declaration is one: see Note [Handles] in
// HaskellReviewer.Server.Wire.Page, and byHandle.
export type Handle = number;

// A piece another declaration sends (a shared signature, doc, warning,
// source part...): find it on that one (byHandle), at the same span.
// See Note [Sent once] in HaskellReviewer.Server.Wire.Page.
export interface Shared {
  sharedWith: Handle;
}

// An id as sent: text, or with its long parts in the page's idParts
// (idText joins them).
export type WireId =
  string | { field: string; parent: number } | { inst: string; head: number[] };

// What every declaration has, nested ones too. name is null for
// instances. line: where the name is.
export interface DeclInfo {
  n: Handle;
  id: WireId;
  name: string | null;
  exported: boolean;
  docs: (DocComment | ({ span: Span } & Shared))[];
  warning: WarningText | ({ span: Span } & Shared) | null;
  fixity: Fixity | null;
  line: number;
}

export const partRoles = ["head", "body", "pragma", "doc"] as const;

// One part of a declaration's extent, with its exact text (SPEC §4.3),
// or shared with another declaration.
export type SourcePart = { span: Span; role: (typeof partRoles)[number] } & (
  { text: string } | Shared
);

export type Signature =
  | {
      code: Code;
      // A shared signature (f, g :: T) has all the names. Only its first
      // name sends it; the others have Shared.
      names: string[];
    }
  | Shared;

export const strictnesses = ["none", "strict", "lazy"] as const;
export const unpacks = ["none", "unpack", "nounpack"] as const;

export interface ArgMarks {
  strictness: (typeof strictnesses)[number];
  unpack: (typeof unpacks)[number];
}

export type ConArg = ArgMarks & { type: Code; doc: DocComment | null };
// type: Shared for f2 in f1, f2 :: T.
export type FieldDecl = DeclInfo & ArgMarks & { type: Code | Shared };

export const conShapes = [
  "prefix",
  "infix",
  "record",
  "gadt",
  "gadt-record",
] as const;

// sharedWith: a constructor sharing its line with an earlier one (A, B
// :: T); then code is null and args and fields are empty: see that one.
export type Constructor = DeclInfo & {
  shape: (typeof conShapes)[number];
  sharedWith: Handle | null;
  code: Code | null;
  args: ConArg[];
  fields: FieldDecl[];
};

export const strategies = [
  "none",
  "stock",
  "newtype",
  "anyclass",
  "via",
] as const;

export interface Strategy {
  strategy: (typeof strategies)[number];
  // The type, for via. Shared in a derived instance: one clause gives
  // several instances.
  via: Code | Shared | null;
}

export type Deriving = Strategy & {
  // instance: null only if the page is broken.
  classes: { code: Code; instance: Handle | null }[];
};

export const typeForms = [
  "data",
  "newtype",
  "type-data",
  "synonym",
  "open-family",
  "data-family",
  "closed-family",
] as const;

export interface TypeFields {
  form: (typeof typeForms)[number];
  head: Code;
  kindSig: Code | null;
  roles: Code | null;
  constructors: Constructor[];
  derivings: Deriving[];
  // Closed families.
  equations: Code[];
  // Associated families.
  defaults: Code[];
  // Instance ids, each a declaration of the page (Note [Instance
  // placement]).
  instances: Handle[];
}

export type Method = DeclInfo & {
  signature: Signature;
  defaultSig: Code | null;
  hasDefault: boolean;
};

export type AssociatedType = { kind: "type" } & DeclInfo & TypeFields;

export interface ClassFields {
  head: Code;
  kindSig: Code | null;
  roles: Code | null;
  methods: Method[];
  associated: AssociatedType[];
  minimal: Code | null;
  instances: Handle[];
}

export const definitions = [
  "equations",
  "pattern",
  "foreign-import",
  "none",
] as const;

export interface ValueFields {
  signature: Signature | null;
  definition: (typeof definitions)[number];
  foreignExports: Code[];
}

export const patSynDirections = [
  "unidirectional",
  "bidirectional",
  "explicit",
] as const;

export interface PatSynFields {
  signature: Signature | null;
  direction: (typeof patSynDirections)[number];
  fields: DeclInfo[];
}

export const famInstForms = ["type", "data", "newtype"] as const;

export interface FamInstFields {
  family: string;
  head: Code;
  form: (typeof famInstForms)[number];
  constructors: Constructor[];
  derivings: Deriving[];
  orphan: boolean;
}

// A type or data instance inside an instance.
export type AssociatedFamInst = { kind: "faminst" } & DeclInfo & FamInstFields;

export const instanceForms = ["explicit", "standalone", "derived"] as const;

export interface InstanceFields {
  head: Code;
  orphan: boolean;
  // onType: the type of a derived instance's clause.
  form: {
    kind: (typeof instanceForms)[number];
    onType: Handle | null;
  } & Strategy;
  associated: AssociatedFamInst[];
  // false: no type or class of this module lists it.
  attached: boolean;
}

export const declKinds = [
  "type",
  "class",
  "value",
  "patsyn",
  "instance",
  "faminst",
  "splice",
] as const;

type Top = DeclInfo & { source: SourcePart[] };

export type Declaration =
  | ({ kind: "type" } & Top & TypeFields)
  | ({ kind: "class" } & Top & ClassFields)
  | ({ kind: "value" } & Top & ValueFields)
  | ({ kind: "patsyn" } & Top & PatSynFields)
  | ({ kind: "instance" } & Top & InstanceFields)
  | ({ kind: "faminst" } & Top & FamInstFields & { attached: boolean })
  // A splice has no name.
  | {
      kind: "splice";
      id: null;
      name: null;
      exported: false;
      docs: DocComment[];
      warning: null;
      fixity: null;
      line: number;
      source: SourcePart[];
      head: Code;
    };

export const docItemKinds = ["heading", "chunk"] as const;

export type DocItem =
  | { kind: "heading"; level: number; inlines: Inline[]; span: Span }
  // -- $name: shown where the export list names it.
  | { kind: "chunk"; name: string; doc: DocComment["doc"]; span: Span };

export const pragmaKinds = ["rules", "complete", "ann", "default"] as const;
export const annTargets = ["value", "type", "module"] as const;

export type Pragma = { span: Span; code: Code } & (
  | { kind: "rules"; names: string[] }
  | { kind: "complete"; names: string[]; type: string | null }
  | {
      kind: "ann";
      target: { kind: (typeof annTargets)[number]; name: string | null };
    }
  | { kind: "default"; class: string | null }
);

function decodeLocated(value: unknown, path: string): Located {
  const o = object(value, path);
  return {
    span: field(o, "span", path, decodeSpan),
    value: field(o, "value", path, str),
  };
}

function decodeHeading(value: unknown, path: string): Heading {
  const o = object(value, path);
  return {
    span: field(o, "span", path, decodeSpan),
    inlines: field(o, "inlines", path, decodeInlines),
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
    doc: field(o, "doc", path, nullable(decodeDocComment)),
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
        heading: field(o, "heading", path, decodeHeading),
      };
    case "doc":
      return { kind, ...decodeDocComment(o, path) };
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
    haddock: field(o, "haddock", path, nullable(decodeDocComment)),
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
        idParts: field(o, "idParts", path, array(str)),
        declarations: field(o, "declarations", path, array(decodeDeclaration)),
        docItems: field(o, "docItems", path, array(decodeDocItem)),
        pragmas: field(o, "pragmas", path, array(decodePragma)),
      };
    case "text-only":
      return {
        kind,
        reason: field(o, "reason", path, decodeReason),
        warnings: field(o, "warnings", path, array(str)),
      };
  }
}

function decodeTier(value: unknown, path: string): ModulePage["tier"] {
  const o = object(value, path);
  return {
    kind: field(o, "kind", path, oneOf(["syntactic"] as const)),
    reason: field(o, "reason", path, oneOf(["no-hie"] as const)),
  };
}

function decodePackage(value: unknown, path: string): ModulePage["package"] {
  const o = object(value, path);
  return {
    name: field(o, "name", path, str),
    version: field(o, "version", path, nullable(str)),
  };
}

export function decodeModulePage(value: unknown): ModulePage {
  const path = "$";
  const o = object(value, path);
  return {
    generation: field(o, "generation", path, nat),
    id: field(o, "id", path, str),
    ref: field(o, "ref", path, str),
    path: field(o, "path", path, nullable(str)),
    package: field(o, "package", path, nullable(decodePackage)),
    component: field(o, "component", path, nullable(str)),
    tier: field(o, "tier", path, decodeTier),
    parse: field(o, "parse", path, decodeParse),
  };
}

// * Declarations

function decodeFixity(value: unknown, path: string): Fixity {
  const o = object(value, path);
  return {
    direction: field(o, "direction", path, oneOf(fixityDirections)),
    precedence: field(o, "precedence", path, nat),
  };
}

// {sharedWith: id}, and nothing else.
function decodeShared(value: unknown, path: string): Shared {
  const o = object(value, path);
  if (Object.keys(o).length !== 1) throw new DecodeError(path, "{sharedWith}");
  return { sharedWith: field(o, "sharedWith", path, nat) };
}

// What decode takes, or Shared: an object with a sharedWith key, and
// these fields with it.
function orShared<T, F extends object>(
  decode: (value: unknown, path: string) => T,
  first: (o: Fields, path: string) => F,
): (value: unknown, path: string) => T | (F & Shared) {
  return (value, path) => {
    if (
      typeof value === "object" &&
      value !== null &&
      !Array.isArray(value) &&
      "sharedWith" in value
    ) {
      const o = object(value, path);
      return {
        ...first(o, path),
        sharedWith: field(o, "sharedWith", path, nat),
      };
    }
    return decode(value, path);
  };
}

const withSpan = (o: Fields, path: string) => ({
  span: field(o, "span", path, decodeSpan),
});

function decodeWireId(value: unknown, path: string): WireId {
  if (typeof value === "string") return value;
  const o = object(value, path);
  if ("field" in o) {
    return {
      field: field(o, "field", path, str),
      parent: field(o, "parent", path, nat),
    };
  }
  return {
    inst: field(o, "inst", path, str),
    head: field(o, "head", path, array(nat)),
  };
}

function infoFields(o: Fields, path: string): DeclInfo {
  return {
    n: field(o, "n", path, nat),
    id: field(o, "id", path, decodeWireId),
    name: field(o, "name", path, nullable(str)),
    exported: field(o, "exported", path, bool),
    docs: field(o, "docs", path, array(orShared(decodeDocComment, withSpan))),
    warning: field(
      o,
      "warning",
      path,
      nullable(orShared(decodeWarning, withSpan)),
    ),
    fixity: field(o, "fixity", path, nullable(decodeFixity)),
    line: field(o, "line", path, nat),
  };
}

function decodeInfo(value: unknown, path: string): DeclInfo {
  return infoFields(object(value, path), path);
}

function decodeSourcePart(value: unknown, path: string): SourcePart {
  const o = object(value, path);
  const first = {
    span: field(o, "span", path, decodeSpan),
    role: field(o, "role", path, oneOf(partRoles)),
  };
  return "sharedWith" in o
    ? { ...first, sharedWith: field(o, "sharedWith", path, nat) }
    : { ...first, text: field(o, "text", path, str) };
}

function decodeSignature(value: unknown, path: string): Signature {
  const o = object(value, path);
  if ("sharedWith" in o) return decodeShared(o, path);
  return {
    code: field(o, "code", path, decodeCode),
    names: field(o, "names", path, array(str)),
  };
}

function argMarks(o: Fields, path: string): ArgMarks {
  return {
    strictness: field(o, "strictness", path, oneOf(strictnesses)),
    unpack: field(o, "unpack", path, oneOf(unpacks)),
  };
}

const codeOrShared = (value: unknown, path: string): Code | Shared =>
  Array.isArray(value) ? decodeCode(value, path) : decodeShared(value, path);

function decodeConstructor(value: unknown, path: string): Constructor {
  const o = object(value, path);
  return {
    ...infoFields(o, path),
    shape: field(o, "shape", path, oneOf(conShapes)),
    sharedWith: field(o, "sharedWith", path, nullable(nat)),
    code: field(o, "code", path, nullable(decodeCode)),
    args: field(
      o,
      "args",
      path,
      array((v, p) => {
        const a = object(v, p);
        return {
          type: field(a, "type", p, decodeCode),
          ...argMarks(a, p),
          doc: field(a, "doc", p, nullable(decodeDocComment)),
        };
      }),
    ),
    fields: field(
      o,
      "fields",
      path,
      array((v, p) => {
        const f = object(v, p);
        return {
          ...infoFields(f, p),
          type: field(f, "type", p, codeOrShared),
          ...argMarks(f, p),
        };
      }),
    ),
  };
}

function strategyFields(o: Fields, path: string): Strategy {
  return {
    strategy: field(o, "strategy", path, oneOf(strategies)),
    via: field(o, "via", path, nullable(codeOrShared)),
  };
}

function decodeDeriving(value: unknown, path: string): Deriving {
  const o = object(value, path);
  return {
    ...strategyFields(o, path),
    classes: field(
      o,
      "classes",
      path,
      array((v, p) => {
        const c = object(v, p);
        return {
          code: field(c, "code", p, decodeCode),
          instance: field(c, "instance", p, nullable(nat)),
        };
      }),
    ),
  };
}

function typeFields(o: Fields, path: string): TypeFields {
  return {
    form: field(o, "form", path, oneOf(typeForms)),
    head: field(o, "head", path, decodeCode),
    kindSig: field(o, "kindSig", path, nullable(decodeCode)),
    roles: field(o, "roles", path, nullable(decodeCode)),
    constructors: field(o, "constructors", path, array(decodeConstructor)),
    derivings: field(o, "derivings", path, array(decodeDeriving)),
    equations: field(o, "equations", path, array(decodeCode)),
    defaults: field(o, "defaults", path, array(decodeCode)),
    instances: field(o, "instances", path, array(nat)),
  };
}

function decodeMethod(value: unknown, path: string): Method {
  const o = object(value, path);
  return {
    ...infoFields(o, path),
    signature: field(o, "signature", path, decodeSignature),
    defaultSig: field(o, "defaultSig", path, nullable(decodeCode)),
    hasDefault: field(o, "hasDefault", path, bool),
  };
}

function decodeAssociatedType(value: unknown, path: string): AssociatedType {
  const o = object(value, path);
  return {
    kind: field(o, "kind", path, oneOf(["type"] as const)),
    ...infoFields(o, path),
    ...typeFields(o, path),
  };
}

function famInstFields(o: Fields, path: string): FamInstFields {
  return {
    family: field(o, "family", path, str),
    head: field(o, "head", path, decodeCode),
    form: field(o, "form", path, oneOf(famInstForms)),
    constructors: field(o, "constructors", path, array(decodeConstructor)),
    derivings: field(o, "derivings", path, array(decodeDeriving)),
    orphan: field(o, "orphan", path, bool),
  };
}

function decodeAssociatedFamInst(
  value: unknown,
  path: string,
): AssociatedFamInst {
  const o = object(value, path);
  return {
    kind: field(o, "kind", path, oneOf(["faminst"] as const)),
    ...infoFields(o, path),
    ...famInstFields(o, path),
  };
}

function decodeInstanceForm(
  value: unknown,
  path: string,
): InstanceFields["form"] {
  const o = object(value, path);
  return {
    kind: field(o, "kind", path, oneOf(instanceForms)),
    ...strategyFields(o, path),
    onType: field(o, "onType", path, nullable(nat)),
  };
}

function decodeDeclaration(value: unknown, path: string): Declaration {
  const o = object(value, path);
  const kind = field(o, "kind", path, oneOf(declKinds));
  const source = () => field(o, "source", path, array(decodeSourcePart));
  switch (kind) {
    case "type":
      return {
        kind,
        ...infoFields(o, path),
        source: source(),
        ...typeFields(o, path),
      };
    case "class":
      return {
        kind,
        ...infoFields(o, path),
        source: source(),
        head: field(o, "head", path, decodeCode),
        kindSig: field(o, "kindSig", path, nullable(decodeCode)),
        roles: field(o, "roles", path, nullable(decodeCode)),
        methods: field(o, "methods", path, array(decodeMethod)),
        associated: field(o, "associated", path, array(decodeAssociatedType)),
        minimal: field(o, "minimal", path, nullable(decodeCode)),
        instances: field(o, "instances", path, array(nat)),
      };
    case "value":
      return {
        kind,
        ...infoFields(o, path),
        source: source(),
        signature: field(o, "signature", path, nullable(decodeSignature)),
        definition: field(o, "definition", path, oneOf(definitions)),
        foreignExports: field(o, "foreignExports", path, array(decodeCode)),
      };
    case "patsyn":
      return {
        kind,
        ...infoFields(o, path),
        source: source(),
        signature: field(o, "signature", path, nullable(decodeSignature)),
        direction: field(o, "direction", path, oneOf(patSynDirections)),
        fields: field(o, "fields", path, array(decodeInfo)),
      };
    case "instance":
      return {
        kind,
        ...infoFields(o, path),
        source: source(),
        head: field(o, "head", path, decodeCode),
        orphan: field(o, "orphan", path, bool),
        form: field(o, "form", path, decodeInstanceForm),
        associated: field(
          o,
          "associated",
          path,
          array(decodeAssociatedFamInst),
        ),
        attached: field(o, "attached", path, bool),
      };
    case "faminst":
      return {
        kind,
        ...infoFields(o, path),
        source: source(),
        ...famInstFields(o, path),
        attached: field(o, "attached", path, bool),
      };
    case "splice":
      return {
        kind,
        id: field(o, "id", path, nothing),
        name: field(o, "name", path, nothing),
        exported: field(o, "exported", path, (v, p) => {
          if (v !== false) throw new DecodeError(p, "false");
          return v;
        }),
        docs: field(o, "docs", path, array(decodeDocComment)),
        warning: field(o, "warning", path, nothing),
        fixity: field(o, "fixity", path, nothing),
        line: field(o, "line", path, nat),
        source: source(),
        head: field(o, "head", path, decodeCode),
      };
  }
}

// Only null.
function nothing(value: unknown, path: string): null {
  if (value !== null) throw new DecodeError(path, "null");
  return null;
}

function decodeDocItem(value: unknown, path: string): DocItem {
  const o = object(value, path);
  const kind = field(o, "kind", path, oneOf(docItemKinds));
  switch (kind) {
    case "heading":
      return {
        kind,
        level: field(o, "level", path, nat),
        inlines: field(o, "inlines", path, decodeInlines),
        span: field(o, "span", path, decodeSpan),
      };
    case "chunk":
      return {
        kind,
        name: field(o, "name", path, str),
        doc: decodeDocComment(o, path).doc,
        span: field(o, "span", path, decodeSpan),
      };
  }
}

function decodePragma(value: unknown, path: string): Pragma {
  const o = object(value, path);
  const kind = field(o, "kind", path, oneOf(pragmaKinds));
  const common = {
    span: field(o, "span", path, decodeSpan),
    code: field(o, "code", path, decodeCode),
  };
  switch (kind) {
    case "rules":
      return { ...common, kind, names: field(o, "names", path, array(str)) };
    case "complete":
      return {
        ...common,
        kind,
        names: field(o, "names", path, array(str)),
        type: field(o, "type", path, nullable(str)),
      };
    case "ann":
      return {
        ...common,
        kind,
        target: field(o, "target", path, (v, p) => {
          const t = object(v, p);
          return {
            kind: field(t, "kind", p, oneOf(annTargets)),
            name: field(t, "name", p, nullable(str)),
          };
        }),
      };
    case "default":
      return { ...common, kind, class: field(o, "class", path, nullable(str)) };
  }
}

// Any declaration with an id: top-level or nested.
export type AnyDecl =
  | Exclude<Declaration, { kind: "splice" }>
  | Constructor
  | FieldDecl
  | Method
  | AssociatedType
  | AssociatedFamInst
  | DeclInfo;

// Built once per page in docs/pageIndex.ts.
export { allDecls, idText } from "../docs/pageIndex";

// Every declaration by handle: what sharedWith, instances, onType and
// links refer to. A repeated handle (CPP) gives the first.
export function byHandle(page: ParsedModule): ReadonlyMap<Handle, AnyDecl> {
  return pageIndex(page).byHandle;
}

// The declaration with this id (the #<decl> part of a docs URL), top
// level or nested. Ids aren't unique: this is the first in page order
// (a field in several constructors: in the first). Not found is not an
// error: the page shows a notice (SPEC §4.1).
export function findDecl(
  page: ParsedModule,
  id: string,
): AnyDecl | "not-found" {
  return pageIndex(page).byId.get(id) ?? "not-found";
}

// The docs view URL of a module, relative to the token, with a
// declaration or Haddock anchor if given. Both percent-encoded: refs
// are paths for loose modules, anchors can hold "#", "%", spaces, any
// character (an instance head).
export function docsHref(ref: string, anchor: string | null = null): string {
  return (
    "/docs/tree/" +
    encodePath(ref) +
    (anchor === null ? "" : "#" + encodeURIComponent(anchor))
  );
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
