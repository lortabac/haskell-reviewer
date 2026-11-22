import {
  createEffect,
  createMemo,
  createSelector,
  createSignal,
  For,
  type JSX,
  on,
  onCleanup,
  onMount,
  Show,
  type Signal,
} from "solid-js";
import type { Entry } from "../api/files";
import { CopyButton } from "../copy/CopyButton";
import { ghostChange, type Side } from "../sides";
import { ChangeSign, GhostSide } from "../ui/ChangeSign";
import { type FileMark, renameText, type TreeFilter } from "./changes";
import {
  ancestors,
  type DirNode,
  type FileNode,
  type GhostEntry,
} from "./tree";

// The file tree in the sidebar: nested lists, folders as disclosure
// buttons. Not an ARIA tree yet (M9).

export interface FileTreeProps {
  root: DirNode;
  // The page's side. Ghosts link to their own.
  side: Side;
  // The file shown in the main view, if any.
  current: string | undefined;
  // The link for a file path on a side.
  href: (side: Side, path: string) => string;
  // Diff mode: markers by path (D-337).
  marks?: ReadonlyMap<string, FileMark> | undefined;
  // "Changed only". The current file and its folders always show.
  filter?: TreeFilter | undefined;
  // Folders the user hasn't opened or closed: open? Default no.
  defaultOpen?: boolean;
}

interface Ctx {
  side: () => Side;
  isCurrent: (path: string) => boolean;
  href: (side: Side, path: string) => string;
  mark: (path: string) => FileMark | undefined;
  showDir: (dir: DirNode) => boolean;
  showFile: (file: FileNode) => boolean;
  isOpen: (path: string) => boolean;
  toggle: (path: string) => void;
  // A file link mounted or went away.
  addLink: (path: string, el: HTMLAnchorElement) => void;
  removeLink: (path: string, el: HTMLAnchorElement) => void;
}

export function FileTree(props: FileTreeProps): JSX.Element {
  // One signal per folder, made when first asked for. Toggling a folder
  // only touches its own signal. Kept while a parent is closed.
  // Undefined: not touched, the default applies.
  const folders = new Map<string, Signal<boolean | undefined>>();
  const folder = (path: string): Signal<boolean | undefined> => {
    let s = folders.get(path);
    if (s === undefined) {
      const [open, setOpen] = createSignal<boolean | undefined>(undefined);
      s = [open, setOpen];
      folders.set(path, s);
    }
    return s;
  };
  // Only the old and the new current link update on a navigation.
  const isCurrent = createSelector(() => props.current);
  // The current file's folders: they show even when filtered out.
  const currentDirs = createMemo(() => {
    const c = props.current;
    return new Set(c === undefined ? [] : ancestors(c));
  });
  // Mounted file links, to scroll the current one into view.
  const links = new Map<string, HTMLAnchorElement>();
  // The path to scroll to once its link is mounted.
  let scrollTo: string | undefined;
  const scroll = (path: string): void => {
    const el = links.get(path);
    if (el === undefined) return;
    el.scrollIntoView({ block: "nearest" });
    scrollTo = undefined;
  };
  // When the current file changes: open its folders (the user can close
  // them again) and scroll to it. Not when a folder is reopened later.
  createEffect(
    on(
      () => props.current,
      (current) => {
        scrollTo = current;
        if (current === undefined) return;
        for (const dir of ancestors(current)) folder(dir)[1](true);
        scroll(current);
      },
    ),
  );
  const isOpen = (path: string): boolean =>
    folder(path)[0]() ?? props.defaultOpen ?? false;
  const ctx: Ctx = {
    side: () => props.side,
    isCurrent,
    href: (side, path) => props.href(side, path),
    mark: (path) => props.marks?.get(path),
    showDir: (dir) =>
      props.filter === undefined ||
      props.filter.dirs.has(dir.path) ||
      currentDirs().has(dir.path),
    showFile: (file) =>
      props.filter === undefined ||
      props.filter.keep(file) ||
      file.entry.path === props.current,
    isOpen,
    toggle: (path) => folder(path)[1](!isOpen(path)),
    addLink: (path, el) => {
      links.set(path, el);
      if (scrollTo === path) scroll(path);
    },
    removeLink: (path, el) => {
      if (links.get(path) === el) links.delete(path);
    },
  };
  return <Level node={props.root} ctx={ctx} class="file-tree" />;
}

// The nodes are the tree's own objects, filtered here: a filter change
// or a navigation mounts only the rows that come or go.
function Level(props: { node: DirNode; ctx: Ctx; class?: string }) {
  const dirs = createMemo(() => props.node.dirs.filter(props.ctx.showDir));
  const files = createMemo(() => props.node.files.filter(props.ctx.showFile));
  return (
    <ul class={props.class}>
      <For each={dirs()}>{(dir) => <Dir node={dir} ctx={props.ctx} />}</For>
      <For each={files()}>{(file) => <File file={file} ctx={props.ctx} />}</For>
    </ul>
  );
}

function Dir(props: { node: DirNode; ctx: Ctx }) {
  const isOpen = () => props.ctx.isOpen(props.node.path);
  return (
    <li>
      <button
        type="button"
        class="tree-folder"
        aria-expanded={isOpen()}
        onClick={() => props.ctx.toggle(props.node.path)}
      >
        {props.node.name}
      </button>
      {/* Children only when open: big folders stay cheap until then. */}
      <Show when={isOpen()}>
        <Level node={props.node} ctx={props.ctx} />
      </Show>
    </li>
  );
}

// A node's entry does not change: one of the two shows.
function File(props: { file: FileNode; ctx: Ctx }) {
  const ghost = () => {
    const e = props.file.entry;
    return e.kind === "ghost" ? e : undefined;
  };
  const real = () => {
    const e = props.file.entry;
    return e.kind === "ghost" ? undefined : e;
  };
  return (
    <>
      <Show when={ghost()}>
        {(g) => <Ghost name={props.file.name} entry={g()} ctx={props.ctx} />}
      </Show>
      <Show when={real()}>
        {(e) => <RealFile name={props.file.name} entry={e()} ctx={props.ctx} />}
      </Show>
    </>
  );
}

// Only on the other side: links there, and says so. Never current, not
// in the link index (its path may clash with a real one).
function Ghost(props: { name: string; entry: GhostEntry; ctx: Ctx }) {
  const path = () => props.entry.path;
  return (
    <li>
      <div class="tree-row">
        <a
          class="tree-file tree-ghost"
          href={props.ctx.href(props.entry.side, path())}
          data-copy={path()}
        >
          {props.name}
          <Show when={ghostChange(props.entry.side)}>
            {(c) => <ChangeSign change={c()} />}
          </Show>
          <GhostSide on={props.entry.side} />
        </a>
        <CopyButton text={path} label={`Copy path ${path()}`} mouseOnly />
      </div>
    </li>
  );
}

function RealFile(props: { name: string; entry: Entry; ctx: Ctx }) {
  const path = () => props.entry.path;
  let link: HTMLAnchorElement | undefined;
  // After mount, so the link is in the page and can scroll.
  onMount(() => {
    if (link !== undefined) props.ctx.addLink(path(), link);
  });
  onCleanup(() => {
    if (link !== undefined) props.ctx.removeLink(path(), link);
  });
  const mark = () => props.ctx.mark(path());
  const renamed = () => {
    const m = mark();
    return m?.status === "renamed"
      ? renameText(m, props.ctx.side())
      : undefined;
  };
  const marker = () => {
    switch (props.entry.kind) {
      case "file":
        return undefined;
      case "symlink":
        return "link";
      case "submodule":
        return "submodule";
    }
  };
  return (
    <li>
      <div class="tree-row">
        <a
          ref={(el) => (link = el)}
          class="tree-file"
          href={props.ctx.href(props.ctx.side(), path())}
          data-copy={path()}
          aria-current={props.ctx.isCurrent(path()) ? "page" : undefined}
          data-tip={renamed()}
        >
          {props.name}
          <Show when={marker()}>
            {(m) => <span class="tree-marker"> {m()}</span>}
          </Show>
          <Show when={mark()}>
            {(m) => <ChangeSign change={m().status} word={renamed()} />}
          </Show>
        </a>
        {/* Mouse only: the keyboard has y. */}
        <CopyButton text={path} label={`Copy path ${path()}`} mouseOnly />
      </div>
    </li>
  );
}
