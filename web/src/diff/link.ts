// The diff view's link for a page's file (D-348). Pure.
import type { Side } from "../sides";
import { type DiffFilesApi, useDiffFiles } from "./store";
import { type DiffLine, diffHref } from "./urls";

type Files = Pick<DiffFilesApi, "files" | "marks" | "loading" | "failed">;

// The path to ask the diff view for. Listed in api/diff-files: its
// path; a renamed file seen from base: the head path. Not listed: none,
// unless the list can't be trusted (cut, loading, failed): then the
// path, and the server decides.
export function diffPathFor(
  d: Files,
  side: Side,
  path: string,
): string | undefined {
  const m = d.marks()?.marks.get(path);
  if (m !== undefined) {
    return m.status === "renamed" && side === "base" ? m.other : path;
  }
  if (side === "tree") return undefined;
  const unsure = d.loading() || d.failed() || d.files()?.truncated === true;
  return unsure ? path : undefined;
}

// The diff view's URL for the page's file, or none (see diffPathFor).
export function diffHrefFor(
  d: Files,
  side: Side,
  path: string,
  at?: DiffLine,
): string | undefined {
  const p = diffPathFor(d, side, path);
  return p === undefined
    ? undefined
    : diffHref(p, at === undefined ? {} : { at });
}

// diffHrefFor with the session's api/diff-files.
export function useDiffLink(): (
  side: Side,
  path: string,
  at?: DiffLine,
) => string | undefined {
  const d = useDiffFiles();
  return (side, path, at) => diffHrefFor(d, side, path, at);
}
