// What changed in the file of a source page (diff mode; D-348): the
// diff of the page's side, as rows and per-line signs. Browse mode: no
// state, no fetch.
//
// Added file: all lines added; removed file: all removed; no fetch for
// either. Unlisted in api/diff-files: unchanged, no fetch (unless the
// list can't be trusted: then the server decides). The rest: one
// api/diff fetch. Its rows and the page's text must be the same
// snapshot with the same line count, else no marks (reload: T16).
import { type Accessor, createMemo, createResource } from "solid-js";
import { getCbor } from "../api/client";
import { decodeDiffView, diffUrl } from "../api/diff";
import { diffPathFor } from "../diff/link";
import { type Rows, rowsOf } from "../diff/rows";
import { loadingText, useDiffFiles } from "../diff/store";
import { usePageSide, useSession } from "../session";
import { sideChanges, type SideChanges } from "../source/diffMarks";
import type { DiffSessionSide } from "../sides";

export type ChangesState =
  | { kind: "none" }
  | { kind: "loading" }
  | { kind: "unchanged" }
  // An added or removed file with no lines.
  | { kind: "empty-file"; added: boolean }
  | { kind: "slow" }
  | { kind: "not-text" }
  | { kind: "error" }
  // rows: for T15b, which maps lines between the sides.
  | { kind: "lines"; rows: Rows; changes: SideChanges };

export const changesTexts = {
  loading: loadingText,
  unchanged: "No changes in this file.",
  emptyAdded: "Empty file added.",
  emptyRemoved: "Empty file removed.",
  noTextChanges: "No changes in the text.",
  error: "Couldn't load the changes.",
  slow: "The diff took over 2 s, so changes aren't marked.",
  notText: "The other side isn't text, so changes aren't marked.",
};

export interface SourceChanges {
  state: Accessor<ChangesState>;
  // The page's side when in diff mode.
  side: Accessor<DiffSessionSide | undefined>;
}

// The note under the heading, for a diff with no marks.
export function changesNote(s: ChangesState): string | undefined {
  if (s.kind === "slow") return changesTexts.slow;
  if (s.kind === "not-text") return changesTexts.notText;
  return undefined;
}

// What n / p say when there is nothing to step to.
export function changesWhy(s: ChangesState): string | undefined {
  switch (s.kind) {
    case "none":
      return undefined;
    case "loading":
      return changesTexts.loading;
    case "unchanged":
      return changesTexts.unchanged;
    case "empty-file":
      return s.added ? changesTexts.emptyAdded : changesTexts.emptyRemoved;
    case "slow":
      return changesTexts.slow;
    case "not-text":
      return changesTexts.notText;
    case "error":
      return changesTexts.error;
    case "lines":
      return s.changes.starts.length === 0
        ? changesTexts.noTextChanges
        : undefined;
  }
}

export function createSourceChanges(opts: {
  path: string;
  // The lines of the page's text, and its snapshot.
  lines: () => number;
  generation: number;
}): SourceChanges {
  const session = useSession();
  const pageSide = usePageSide();
  const diff = useDiffFiles();
  const side = (): DiffSessionSide | undefined => {
    const s = pageSide();
    return session.mode === "diff" && s !== "tree" ? s : undefined;
  };
  const mark = () => diff.marks()?.marks.get(opts.path);
  const whole = () => {
    const m = mark();
    return m?.status === "added" || m?.status === "removed"
      ? m.status
      : undefined;
  };
  // The path to fetch, or undefined: none needed (yet).
  const fetchPath = () => {
    if (side() === undefined || diff.loading() || whole() !== undefined) {
      return undefined;
    }
    const s = pageSide();
    return diffPathFor(diff, s, opts.path);
  };
  const [res] = createResource(fetchPath, (p) =>
    getCbor(diffUrl(p, false), decodeDiffView),
  );
  const fromRows = (rows: Rows, s: DiffSessionSide): ChangesState => {
    const n = s === "head" ? rows.headCount : rows.baseCount;
    if (n !== opts.lines()) {
      console.warn(
        `diff of ${opts.path}: runs have ${n} lines on ${s}, the text ${opts.lines()}`,
      );
      return { kind: "error" };
    }
    return { kind: "lines", rows, changes: sideChanges(rows, s) };
  };
  const state = createMemo((): ChangesState => {
    const s = side();
    if (s === undefined) return { kind: "none" };
    if (diff.loading()) return { kind: "loading" };
    const w = whole();
    const n = opts.lines();
    if (w !== undefined) {
      return n === 0
        ? { kind: "empty-file", added: w === "added" }
        : fromRows(rowsOf([[w === "added" ? "insert" : "delete", n]]), s);
    }
    if (fetchPath() === undefined) return { kind: "unchanged" };
    if (res.state === "errored") return { kind: "error" };
    if (res.state !== "ready") return { kind: "loading" };
    const r = res();
    if (r === undefined || r.kind !== "ok") return { kind: "error" };
    const v = r.value;
    // api/diff has head's generation: only head's text can be checked
    // (base's has its own).
    if (s === "head" && v.generation !== opts.generation) {
      console.warn(
        `diff of ${opts.path}: generation ${v.generation}, the text ${opts.generation}`,
      );
      return { kind: "error" };
    }
    if (v.status === "unchanged") return { kind: "unchanged" };
    if (v.diff.kind === "too-slow") return { kind: "slow" };
    if (v.diff.kind === "not-text") return { kind: "not-text" };
    return fromRows(rowsOf(v.diff.runs), s);
  });
  return { state, side };
}
