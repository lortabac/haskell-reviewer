// The module tree payload. Shape: see encodeModuleTree in
// HaskellReviewer.Server.Wire.
import {
  array,
  bool,
  field,
  nat,
  nullable,
  object,
  oneOf,
  str,
} from "./decode";

export const noticeKinds = ["stack", "implicit", "hpack"] as const;

export type Notice =
  | { kind: "stack" }
  | { kind: "implicit" }
  // Dirs with a package.yaml but no .cabal file. "." is ROOT.
  | { kind: "hpack"; dirs: string[] };

export const moduleRoles = [
  "exposed",
  "other",
  "signature",
  "test-module",
  "main",
] as const;

export type ModuleRole = (typeof moduleRoles)[number];

export const sourceKinds = [
  "module",
  "generated",
  "preprocessed",
  "no-source",
] as const;

export const noSourceReasons = [
  "autogen",
  "missing",
  "outside",
  "symlink",
] as const;

export type NoSourceReason = (typeof noSourceReasons)[number];

export type ModuleSource =
  | { kind: "module"; path: string }
  // A preprocessor input (alex, happy, ...): text only.
  | { kind: "generated"; path: string; tool: string }
  // Run through a -pgmF / -pgmL tool: text only.
  | { kind: "preprocessed"; path: string; tool: string }
  | { kind: "no-source"; reason: NoSourceReason };

export interface ModuleEntry {
  name: string;
  // Long form, like pkg:lib:Foo.
  id: string;
  // What URLs use: the name when unique, else the id.
  ref: string;
  role: ModuleRole;
  source: ModuleSource;
}

export interface ComponentModules {
  // lib, lib:x, exe:x, test:x, bench:x
  name: string;
  buildable: boolean;
  modules: ModuleEntry[];
}

export interface PackageModules {
  name: string;
  version: string;
  cabalFile: string;
  components: ComponentModules[];
}

export interface BrokenPackage {
  cabalFile: string;
  problem: string;
}

export const otherKinds = ["module", "boot"] as const;

export type OtherFile =
  // A Haskell file no component claims. The id is the path; name is
  // null when the path isn't a module name.
  | {
      kind: "module";
      path: string;
      name: string | null;
      id: string;
      ref: string;
    }
  // .hs-boot / .lhs-boot: text only.
  | { kind: "boot"; path: string };

export interface ModuleTree {
  generation: number;
  notices: Notice[];
  packages: PackageModules[];
  broken: BrokenPackage[];
  // Sorted by path. With an implicit notice, this is the implicit package.
  other: OtherFile[];
  warnings: string[];
}

function decodeNotice(value: unknown, path: string): Notice {
  const o = object(value, path);
  const kind = field(o, "kind", path, oneOf(noticeKinds));
  switch (kind) {
    case "stack":
    case "implicit":
      return { kind };
    case "hpack":
      return { kind, dirs: field(o, "dirs", path, array(str)) };
  }
}

function decodeSource(value: unknown, path: string): ModuleSource {
  const o = object(value, path);
  const kind = field(o, "kind", path, oneOf(sourceKinds));
  switch (kind) {
    case "module":
      return { kind, path: field(o, "path", path, str) };
    case "generated":
    case "preprocessed":
      return {
        kind,
        path: field(o, "path", path, str),
        tool: field(o, "tool", path, str),
      };
    case "no-source":
      return { kind, reason: field(o, "reason", path, oneOf(noSourceReasons)) };
  }
}

function decodeEntry(value: unknown, path: string): ModuleEntry {
  const o = object(value, path);
  return {
    name: field(o, "name", path, str),
    id: field(o, "id", path, str),
    ref: field(o, "ref", path, str),
    role: field(o, "role", path, oneOf(moduleRoles)),
    source: field(o, "source", path, decodeSource),
  };
}

function decodeComponent(value: unknown, path: string): ComponentModules {
  const o = object(value, path);
  return {
    name: field(o, "name", path, str),
    buildable: field(o, "buildable", path, bool),
    modules: field(o, "modules", path, array(decodeEntry)),
  };
}

function decodePackage(value: unknown, path: string): PackageModules {
  const o = object(value, path);
  return {
    name: field(o, "name", path, str),
    version: field(o, "version", path, str),
    cabalFile: field(o, "cabalFile", path, str),
    components: field(o, "components", path, array(decodeComponent)),
  };
}

function decodeBroken(value: unknown, path: string): BrokenPackage {
  const o = object(value, path);
  return {
    cabalFile: field(o, "cabalFile", path, str),
    problem: field(o, "problem", path, str),
  };
}

function decodeOther(value: unknown, path: string): OtherFile {
  const o = object(value, path);
  const kind = field(o, "kind", path, oneOf(otherKinds));
  switch (kind) {
    case "module":
      return {
        kind,
        path: field(o, "path", path, str),
        name: field(o, "name", path, nullable(str)),
        id: field(o, "id", path, str),
        ref: field(o, "ref", path, str),
      };
    case "boot":
      return { kind, path: field(o, "path", path, str) };
  }
}

export function decodeModuleTree(value: unknown): ModuleTree {
  const path = "$";
  const o = object(value, path);
  return {
    generation: field(o, "generation", path, nat),
    notices: field(o, "notices", path, array(decodeNotice)),
    packages: field(o, "packages", path, array(decodePackage)),
    broken: field(o, "broken", path, array(decodeBroken)),
    other: field(o, "other", path, array(decodeOther)),
    warnings: field(o, "warnings", path, array(str)),
  };
}
