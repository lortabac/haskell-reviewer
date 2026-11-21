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
import { usePageSide } from "../session";
import type { Side } from "../sides";
import { FileTree } from "./FileTree";
import { apiFilesUrl, hrefFor, pathFromPathname, withBase } from "./paths";
import { buildTree } from "./tree";

// The Files view of the sidebar: the file list of the page's side, as a
// tree. Loaded once per side: the last list of each side is kept, so
// going back to a side is instant (D-332).
export function FilesPanel(): JSX.Element {
  const side = usePageSide();
  const lists = new Map<Side, Result<FileList>>();
  const [list] = createResource(side, async (s) => {
    const got = lists.get(s) ?? (await getJson(apiFilesUrl(s), decodeFileList));
    if (got.kind === "ok") lists.set(s, got);
    return { side: s, got };
  });
  const location = useLocation();
  const current = createMemo(() => pathFromPathname(location.pathname, side()));
  // "/<token>", from the router.
  const base = useResolvedPath(() => "/");
  const href = (path: string): string =>
    withBase(base(), hrefFor(side(), path));
  // Only the page's side: never the last side's list while loading.
  const ok = (): FileList | undefined => {
    const r = list();
    // Safety net: createResource already drops late answers.
    return r?.side === side() && r.got.kind === "ok" ? r.got.value : undefined;
  };
  const tree = createMemo(() => {
    const l = ok();
    return l === undefined ? undefined : buildTree(l.entries);
  });
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
            <Show
              when={l().entries.length > 0}
              fallback={<p>No files under ROOT.</p>}
            >
              <FileTree
                root={tree() ?? buildTree([])}
                current={current()}
                href={href}
              />
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
