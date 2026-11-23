import { useLocation, useResolvedPath } from "@solidjs/router";
import {
  createMemo,
  createResource,
  type JSX,
  Match,
  Show,
  Switch,
} from "solid-js";
import { getJson, type Result } from "../api/client";
import { decodeFileList, type FileList } from "../api/files";
import { DiffFilesNotes, openAllUpTo, useDiffFiles } from "../diff/store";
import { diffPathFromPathname } from "../diff/urls";
import { usePrefs } from "../prefs/store";
import { usePageSide } from "../session";
import type { Side } from "../sides";
import { changedFilter, unionEntries } from "./changes";
import { FileTree } from "./FileTree";
import { apiFilesUrl, hrefFor, pathFromPathname, withBase } from "./paths";
import { buildTree, isEmptyTree } from "./tree";

// The Files view of the sidebar: the file list of the page's side, as a
// tree. Loaded once per side: the last list of each side is kept, so
// going back to a side is instant (D-332). In diff mode, with markers
// and ghosts from api/diff-files, and the "changed only" filter (D-337).
export function FilesPanel(): JSX.Element {
  const side = usePageSide();
  const lists = new Map<Side, Result<FileList>>();
  const [list] = createResource(side, async (s) => {
    const got = lists.get(s) ?? (await getJson(apiFilesUrl(s), decodeFileList));
    if (got.kind === "ok") lists.set(s, got);
    return { side: s, got };
  });
  const location = useLocation();
  // On a diff page: its path, on head (D-342; ghosts are never current).
  const current = createMemo(
    () =>
      pathFromPathname(location.pathname, side()) ??
      (side() === "head" ? diffPathFromPathname(location.pathname) : undefined),
  );
  // "/<token>", from the router.
  const base = useResolvedPath(() => "/");
  const href = (s: Side, path: string): string =>
    withBase(base(), hrefFor(s, path));
  // Only the page's side: never the last side's list while loading.
  const ok = (): FileList | undefined => {
    const r = list();
    // Safety net: createResource already drops late answers.
    return r?.side === side() && r.got.kind === "ok" ? r.got.value : undefined;
  };
  const diffFiles = useDiffFiles();
  const prefs = usePrefs();
  const tree = createMemo(() => {
    const l = ok();
    if (l === undefined) return undefined;
    return buildTree(unionEntries(l.entries, diffFiles.marks()?.ghosts ?? []));
  });
  // Only on (tree, marks, the box): never on a navigation (D-337).
  // On a failure: everything, unfiltered.
  const filter = createMemo(() => {
    const t = tree();
    const m = diffFiles.marks();
    return t === undefined || m === undefined || !prefs.changedOnly()
      ? undefined
      : changedFilter(t, m.marks);
  });
  const defaultOpen = () => {
    const f = filter();
    return f !== undefined && f.count <= openAllUpTo;
  };
  // Changed only, while api/diff-files loads: only the note.
  const waiting = () => prefs.changedOnly() && diffFiles.loading();
  return (
    <Switch>
      <Match when={list.loading}>
        <p>Loading files…</p>
      </Match>
      <Match when={ok()}>
        {(l) => (
          <>
            <Show when={l().truncated}>
              <p>Only the first 100,000 files are listed.</p>
            </Show>
            <Show when={l().skippedNames > 0}>
              <p>{skippedText(l().skippedNames)}</p>
            </Show>
            <DiffFilesNotes loading />
            <Show when={!waiting() && tree()}>
              {(t) => (
                <Show
                  when={!isEmptyTree(t())}
                  fallback={<p>No files under ROOT.</p>}
                >
                  {/* Above the tree, not instead of it: the current file
                      still shows, and open folders are kept. */}
                  <Show when={filter()?.count === 0}>
                    <p class="tree-note">No changed files under ROOT.</p>
                  </Show>
                  <FileTree
                    root={t()}
                    side={side()}
                    current={current()}
                    href={href}
                    marks={diffFiles.marks()?.marks}
                    filter={filter()}
                    defaultOpen={defaultOpen()}
                  />
                </Show>
              )}
            </Show>
          </>
        )}
      </Match>
      <Match when={true}>
        <p>Couldn't load the file list. See the terminal for details.</p>
      </Match>
    </Switch>
  );
}

function skippedText(n: number): string {
  return n === 1
    ? "1 file name isn't UTF-8 and isn't listed."
    : `${n.toLocaleString("en-US")} file names aren't UTF-8 and aren't listed.`;
}
