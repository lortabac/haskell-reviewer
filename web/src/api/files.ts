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

// filtered: read from disk, and git would filter it (LFS, say): what
// git stores may differ.
interface FileViewHead {
  generation: number;
  filtered: boolean;
}

export type FileView = FileViewHead &
  (
    | { kind: "text"; text: string }
    | { kind: "binary"; size: number }
    | { kind: "too-large"; size: number; limit: number }
    // resolved: the listed file the link points to, if any.
    | { kind: "symlink"; target: string; resolved: string | null }
    | { kind: "submodule"; commit: string | null }
    // The blob is not in the local repo (partial clone).
    | { kind: "not-local" }
  );

export const fileViewKinds = [
  "text",
  "binary",
  "too-large",
  "symlink",
  "submodule",
  "not-local",
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
  const filtered = field(o, "filtered", path, bool);
  const kind = field(o, "kind", path, oneOf(fileViewKinds));
  switch (kind) {
    case "text":
      return { generation, filtered, kind, text: field(o, "text", path, str) };
    case "binary":
      return { generation, filtered, kind, size: field(o, "size", path, nat) };
    case "too-large":
      return {
        generation,
        filtered,
        kind,
        size: field(o, "size", path, nat),
        limit: field(o, "limit", path, nat),
      };
    case "symlink":
      return {
        generation,
        filtered,
        kind,
        target: field(o, "target", path, str),
        resolved: field(o, "resolved", path, nullable(str)),
      };
    case "submodule":
      return {
        generation,
        filtered,
        kind,
        commit: field(o, "commit", path, nullable(str)),
      };
    case "not-local":
      return { generation, filtered, kind };
  }
}
