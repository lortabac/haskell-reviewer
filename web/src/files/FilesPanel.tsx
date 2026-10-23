import { useLocation, useResolvedPath } from "@solidjs/router";
import {
  createMemo,
  createResource,
  type JSX,
  Match,
  Show,
  Switch,
} from "solid-js";
import { getJson } from "../api/client";
import { decodeFileList, type FileList } from "../api/files";
import { FileTree } from "./FileTree";
import { hrefFor, pathFromPathname, withBase } from "./paths";
import { buildTree } from "./tree";

// The Files view of the sidebar: the file list, as a tree. Loaded once.
export function FilesPanel(): JSX.Element {
  const [list] = createResource(() =>
    getJson("/api/files/tree", decodeFileList),
  );
  const location = useLocation();
  const current = createMemo(() => pathFromPathname(location.pathname));
  // "/<token>", from the router.
  const base = useResolvedPath(() => "/");
  const href = (path: string): string => withBase(base(), hrefFor(path));
  const ok = (): FileList | undefined => {
    const r = list();
    return r?.kind === "ok" ? r.value : undefined;
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
