// One file's diff (GET api/diff/<path…>[?ws=ignore]), CBOR. Shape: see
// Note [Diff view] in HaskellReviewer.Server.Wire.Diff.
import {
  array,
  bool,
  DecodeError,
  field,
  nat,
  nullable,
  object,
  oneOf,
  positive,
  str,
} from "./decode";
import { changeNotes, statuses as changeStatuses } from "./diffFiles";
import type { ChangeNote } from "./diffFiles";
import { encodePath } from "../files/paths";

export const statuses = [...changeStatuses, "unchanged"] as const;

export type DiffStatus = (typeof statuses)[number];

export const sideKinds = [
  "absent",
  "text",
  "binary",
  "too-large",
  "symlink",
  "submodule",
  "not-local",
] as const;

// What one side has at the path.
export type DiffSide =
  | { kind: "absent" }
  | { kind: "text" }
  | { kind: "binary"; size: number }
  | { kind: "too-large"; size: number; limit: number }
  | { kind: "symlink"; target: string }
  // commit is null for a nested repo.
  | { kind: "submodule"; commit: string | null }
  // A blob not in a partial clone.
  | { kind: "not-local" };

export const diffKinds = ["lines", "too-slow", "not-text"] as const;

export const ops = ["equal", "delete", "insert"] as const;

export type Op = (typeof ops)[number];

export const bomChanges = ["added", "removed"] as const;

// Base lines are equal + delete, head lines equal + insert.
export type DiffRun = [op: Op, count: number];

export type DiffBody =
  | {
      kind: "lines";
      runs: DiffRun[];
      // Equal lines where only one side ends in CRLF.
      crlfLines: number;
      bom: (typeof bomChanges)[number] | null;
    }
  // Over the server's time limit.
  | { kind: "too-slow" }
  // A side has no text: the side kinds say why.
  | { kind: "not-text" };

export interface DiffView {
  generation: number;
  // The head path; the base path for "removed".
  path: string;
  // Only for "renamed".
  oldPath: string | null;
  status: DiffStatus;
  // Percent, only for "renamed".
  similarity: number | null;
  note: ChangeNote | null;
  ignoreWhitespace: boolean;
  base: DiffSide;
  head: DiffSide;
  diff: DiffBody;
}

function decodeSide(value: unknown, path: string): DiffSide {
  const o = object(value, path);
  const kind = field(o, "kind", path, oneOf(sideKinds));
  switch (kind) {
    case "absent":
    case "text":
    case "not-local":
      return { kind };
    case "binary":
      return { kind, size: field(o, "size", path, nat) };
    case "too-large":
      return {
        kind,
        size: field(o, "size", path, nat),
        limit: field(o, "limit", path, nat),
      };
    case "symlink":
      return { kind, target: field(o, "target", path, str) };
    case "submodule":
      return { kind, commit: field(o, "commit", path, nullable(str)) };
  }
}

function decodeRun(value: unknown, path: string): DiffRun {
  if (!Array.isArray(value) || value.length !== 2) {
    throw new DecodeError(path, "an [op, count] pair");
  }
  return [oneOf(ops)(value[0], `${path}[0]`), positive(value[1], `${path}[1]`)];
}

function decodeBody(value: unknown, path: string): DiffBody {
  const o = object(value, path);
  const kind = field(o, "kind", path, oneOf(diffKinds));
  switch (kind) {
    case "lines":
      return {
        kind,
        runs: field(o, "runs", path, array(decodeRun)),
        crlfLines: field(o, "crlfLines", path, nat),
        bom: field(o, "bom", path, nullable(oneOf(bomChanges))),
      };
    case "too-slow":
    case "not-text":
      return { kind };
  }
}

export function decodeDiffView(value: unknown): DiffView {
  const path = "$";
  const o = object(value, path);
  return {
    generation: field(o, "generation", path, nat),
    path: field(o, "path", path, str),
    oldPath: field(o, "oldPath", path, nullable(str)),
    status: field(o, "status", path, oneOf(statuses)),
    similarity: field(o, "similarity", path, nullable(nat)),
    note: field(o, "note", path, nullable(oneOf(changeNotes))),
    ignoreWhitespace: field(o, "ignoreWhitespace", path, bool),
    base: field(o, "base", path, decodeSide),
    head: field(o, "head", path, decodeSide),
    diff: field(o, "diff", path, decodeBody),
  };
}

// The diff API URL of a head or base path, relative to the token.
export function diffUrl(path: string, ignoreWhitespace: boolean): string {
  return (
    "/api/diff/" + encodePath(path) + (ignoreWhitespace ? "?ws=ignore" : "")
  );
}
