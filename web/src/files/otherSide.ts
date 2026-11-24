// Where a file page goes on the other side (D-349). Pure.
//
// Added or removed: none. Renamed: the other path. Unchanged: the same
// line. With the diff rows: the line through mapLine. No rows (too
// slow, not text, failed): the file's top, and say why.
import { mapLine } from "../diff/mapLine";
import type { DiffFilesApi } from "../diff/store";
import { onlyOn } from "../diff/words";
import type { SideGo, SideTarget } from "../keys/KeysProvider";
import { type DiffSessionSide, otherSide } from "../sides";
import type { FileMark } from "./changes";
import { hrefFor, lineHref, mdHref } from "./paths";
import { type ChangesState, changesTexts } from "./sourceChanges";

export type FileTarget =
  | { kind: "go"; path: string; line?: number; words?: string }
  | { kind: "off"; why: string };

// api/diff-files: loading; in and whole (an unlisted file is
// unchanged); or cut or failed (the page's own diff decides).
export type ListState = "loading" | "trusted" | "partial";

export const cantMap = {
  slow: "Couldn't map the line: the diff took over 2 s.",
  notText: "Couldn't map the line: the other side isn't text.",
  error: "Couldn't map the line: couldn't load the changes.",
};

// mark: this side's mark of the file (undefined: not listed). state:
// the page's changes; undefined: the page has no lines (binary,
// symlink, still loading). line: the cursor's; undefined: the top.
export function otherFile(o: {
  side: DiffSessionSide;
  path: string;
  mark: FileMark | undefined;
  list: ListState;
  state: ChangesState | undefined;
  line: number | undefined;
}): FileTarget {
  if (o.list === "loading") return { kind: "off", why: changesTexts.loading };
  const m = o.mark;
  if (
    m?.status === "added" ||
    m?.status === "removed" ||
    o.state?.kind === "empty-file"
  ) {
    return { kind: "off", why: onlyOn(o.side) };
  }
  const path = m?.status === "renamed" ? m.other : o.path;
  const same: FileTarget =
    o.line === undefined
      ? { kind: "go", path }
      : { kind: "go", path, line: o.line };
  const st = o.state;
  // Not listed, and the list is whole: the same file.
  if ((m === undefined && o.list === "trusted") || st === undefined) {
    return st === undefined ? { kind: "go", path } : same;
  }
  switch (st.kind) {
    case "loading":
      return { kind: "off", why: changesTexts.loading };
    case "none":
    case "unchanged":
      return same;
    case "slow":
      return { kind: "go", path, words: cantMap.slow };
    case "not-text":
      return { kind: "go", path, words: cantMap.notText };
    case "error":
      return { kind: "go", path, words: cantMap.error };
    case "lines": {
      if (o.line === undefined) return { kind: "go", path };
      const r = mapLine(st.rows, o.side, o.line);
      if (r === undefined) return { kind: "go", path };
      const other = otherSide(o.side);
      switch (r.how) {
        case "exact":
          return { kind: "go", path, line: r.line };
        case "paired":
          return {
            kind: "go",
            path,
            line: r.line,
            words: `Line ${o.line} changed: ${other} line ${r.line} shown.`,
          };
        case "nearest":
          return {
            kind: "go",
            path,
            line: r.line,
            words: `Line ${o.line} isn't on ${other}: nearest line ${r.line}.`,
          };
      }
    }
  }
}

// What otherFile needs from api/diff-files.
export function listState(
  d: Pick<DiffFilesApi, "marks" | "loading" | "failed" | "files">,
  path: string,
): { mark: FileMark | undefined; list: ListState } {
  const list: ListState = d.loading()
    ? "loading"
    : !d.failed() && d.files()?.truncated === false
      ? "trusted"
      : "partial";
  return { mark: d.marks()?.marks.get(path), list };
}

// A page with no lines (or none yet): the path only.
export function pathTarget(
  d: Parameters<typeof listState>[0],
  side: DiffSessionSide,
  path: string,
): FileTarget {
  return otherFile({
    side,
    path,
    ...listState(d, path),
    state: undefined,
    line: undefined,
  });
}

// A source page's place there. A line lands with focus, like a code
// link.
export function sourceGo(
  t: Extract<FileTarget, { kind: "go" }>,
  side: DiffSessionSide,
): SideGo {
  const other = otherSide(side);
  const words = t.words === undefined ? {} : { words: t.words };
  return t.line === undefined
    ? { href: hrefFor(other, t.path), ...words }
    : {
        href: lineHref(other, t.path, t.line),
        state: { codeJump: true },
        ...words,
      };
}

// The header item's target for a source page. t: the path only (cheap,
// read on every change); at: the exact place, only on use.
export function sourceTarget(
  t: FileTarget,
  side: DiffSessionSide,
  at?: () => FileTarget,
  leave?: () => void,
): SideTarget {
  const other = otherSide(side);
  if (t.kind === "off") return { kind: "off", side: other, why: t.why };
  const exact = (): SideGo => {
    const a = at?.() ?? t;
    return sourceGo(a.kind === "go" ? a : t, side);
  };
  return {
    kind: "go",
    side: other,
    top: hrefFor(other, t.path),
    at: exact,
    ...(leave === undefined ? {} : { leave }),
  };
}

// A Markdown page: the path only, the hash kept.
export function markdownTarget(
  t: FileTarget,
  side: DiffSessionSide,
  hash: () => string,
): SideTarget {
  const other = otherSide(side);
  if (t.kind === "off") return { kind: "off", side: other, why: t.why };
  const top = mdHref(other, t.path);
  return { kind: "go", side: other, top, at: () => ({ href: top + hash() }) };
}
