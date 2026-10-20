// The file list and file view payloads. Shapes: see
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

export type ListSource = "git" | "disk";

export type Entry =
  | { path: string; kind: "file" }
  | { path: string; kind: "symlink" }
  // commit is null for a nested repo that is not a submodule.
  | { path: string; kind: "submodule"; commit: string | null };

export const entryKinds = ["file", "symlink", "submodule"] as const;

export interface FileList {
  generation: number;
  source: ListSource;
  // Sorted by path, in code point order (UTF-8 byte order). Not the
  // order of JS string compare, which goes by UTF-16 code units.
  entries: Entry[];
  // Names skipped because they are not UTF-8.
  skippedNames: number;
  // More files than the cap: only the first ones are listed.
  truncated: boolean;
}

export type FileView =
  | { generation: number; kind: "text"; text: string }
  | { generation: number; kind: "binary"; size: number }
  | { generation: number; kind: "too-large"; size: number; limit: number }
  // resolved: the listed file the link points to, if any.
  | {
      generation: number;
      kind: "symlink";
      target: string;
      resolved: string | null;
    }
  | { generation: number; kind: "submodule"; commit: string | null };

export const fileViewKinds = [
  "text",
  "binary",
  "too-large",
  "symlink",
  "submodule",
] as const;

function decodeEntry(value: unknown, path: string): Entry {
  const o = object(value, path);
  const p = field(o, "path", path, str);
  const kind = field(o, "kind", path, oneOf(entryKinds));
  switch (kind) {
    case "file":
    case "symlink":
      return { path: p, kind };
    case "submodule":
      return { path: p, kind, commit: field(o, "commit", path, nullable(str)) };
  }
}

export function decodeFileList(value: unknown): FileList {
  const path = "$";
  const o = object(value, path);
  return {
    generation: field(o, "generation", path, nat),
    source: field(o, "source", path, oneOf(["git", "disk"] as const)),
    entries: field(o, "entries", path, array(decodeEntry)),
    skippedNames: field(o, "skippedNames", path, nat),
    truncated: field(o, "truncated", path, bool),
  };
}

export function decodeFileView(value: unknown): FileView {
  const path = "$";
  const o = object(value, path);
  const generation = field(o, "generation", path, nat);
  const kind = field(o, "kind", path, oneOf(fileViewKinds));
  switch (kind) {
    case "text":
      return { generation, kind, text: field(o, "text", path, str) };
    case "binary":
      return { generation, kind, size: field(o, "size", path, nat) };
    case "too-large":
      return {
        generation,
        kind,
        size: field(o, "size", path, nat),
        limit: field(o, "limit", path, nat),
      };
    case "symlink":
      return {
        generation,
        kind,
        target: field(o, "target", path, str),
        resolved: field(o, "resolved", path, nullable(str)),
      };
    case "submodule":
      return {
        generation,
        kind,
        commit: field(o, "commit", path, nullable(str)),
      };
  }
}
