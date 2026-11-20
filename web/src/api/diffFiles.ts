// The changed files of a diff session (GET api/diff-files). Shape: see
// encodeDiffFiles in HaskellReviewer.Server.Wire.
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

export const statuses = ["added", "removed", "modified", "renamed"] as const;

export type FileStatus = (typeof statuses)[number];

export const changeNotes = [
  "type-changed",
  "mode-changed",
  "filtered",
] as const;

export type ChangeNote = (typeof changeNotes)[number];

// One side: the revision as typed ("working tree" for the disk), and
// its full commit hash, null for the working tree.
export interface DiffEnd {
  label: string;
  commit: string | null;
}

export interface ChangedFile {
  // The head path; the base path for "removed".
  path: string;
  // Only for "renamed".
  oldPath: string | null;
  status: FileStatus;
  // Percent, only for "renamed" (100: same contents).
  similarity: number | null;
  note: ChangeNote | null;
}

// Changed paths outside ROOT.
export interface Outside {
  total: number;
  // The first 100, by path.
  paths: string[];
  // Too many rows on a side: total may be short.
  truncated: boolean;
}

export interface DiffFiles {
  generation: number;
  base: DiffEnd;
  head: DiffEnd;
  // Sorted by path. Unchanged files are left out.
  files: ChangedFile[];
  // null: not known (the working tree).
  outside: Outside | null;
  // Too many files: only exact and same-name renames were looked for.
  renameLimitHit: boolean;
  // A side has more files than the cap: files past the smaller last
  // path are not listed.
  truncated: boolean;
}

function decodeEnd(value: unknown, path: string): DiffEnd {
  const o = object(value, path);
  return {
    label: field(o, "label", path, str),
    commit: field(o, "commit", path, nullable(str)),
  };
}

function decodeFile(value: unknown, path: string): ChangedFile {
  const o = object(value, path);
  return {
    path: field(o, "path", path, str),
    oldPath: field(o, "oldPath", path, nullable(str)),
    status: field(o, "status", path, oneOf(statuses)),
    similarity: field(o, "similarity", path, nullable(nat)),
    note: field(o, "note", path, nullable(oneOf(changeNotes))),
  };
}

function decodeOutside(value: unknown, path: string): Outside {
  const o = object(value, path);
  return {
    total: field(o, "total", path, nat),
    paths: field(o, "paths", path, array(str)),
    truncated: field(o, "truncated", path, bool),
  };
}

export function decodeDiffFiles(value: unknown): DiffFiles {
  const path = "$";
  const o = object(value, path);
  return {
    generation: field(o, "generation", path, nat),
    base: field(o, "base", path, decodeEnd),
    head: field(o, "head", path, decodeEnd),
    files: field(o, "files", path, array(decodeFile)),
    outside: field(o, "outside", path, nullable(decodeOutside)),
    renameLimitHit: field(o, "renameLimitHit", path, bool),
    truncated: field(o, "truncated", path, bool),
  };
}
