// The session's mode, and in diff mode its revisions (GET api/session,
// D-332). Fixed for the process. Shape: see encodeSession in
// HaskellReviewer.Server.Wire.
import type { Side } from "../sides";
import { field, nullable, object, oneOf, positive, str } from "./decode";

export const modes = ["browse", "diff"] as const;

// A revision as typed ("working tree" for the disk), and its full
// commit hash.
export interface SessionEnd<C> {
  label: string;
  commit: C;
}

export type Session =
  | { mode: "browse" }
  | {
      mode: "diff";
      // The --diff argument, empty ends as HEAD.
      spec: string;
      // For "...", the first merge base.
      base: SessionEnd<string>;
      // null: the working tree.
      head: SessionEnd<string | null>;
      // How many merge bases, for "..." only.
      mergeBases: number | null;
    };

export const browseSession: Session = { mode: "browse" };

function decodeEnd<C>(
  commit: (value: unknown, path: string) => C,
): (value: unknown, path: string) => SessionEnd<C> {
  return (value, path) => {
    const o = object(value, path);
    return {
      label: field(o, "label", path, str),
      commit: field(o, "commit", path, commit),
    };
  };
}

export function decodeSession(value: unknown): Session {
  const path = "$";
  const o = object(value, path);
  const mode = field(o, "mode", path, oneOf(modes));
  if (mode === "browse") return { mode };
  return {
    mode,
    spec: field(o, "spec", path, str),
    base: field(o, "base", path, decodeEnd(str)),
    head: field(o, "head", path, decodeEnd(nullable(str))),
    mergeBases: field(o, "mergeBases", path, nullable(positive)),
  };
}

// The sides the session has.
export function sessionSides(s: Session): readonly Side[] {
  return s.mode === "browse" ? ["tree"] : ["base", "head"];
}

// The side of pages with none in the URL (Home, not found): tree in
// browse mode, head in diff mode (G15).
export function defaultSide(s: Session): Side {
  return s.mode === "browse" ? "tree" : "head";
}
