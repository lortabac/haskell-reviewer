import {
  createEffect,
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
import { ancestors, type DirNode, type FileNode } from "./tree";

// The file tree in the sidebar: nested lists, folders as disclosure
// buttons. Not an ARIA tree yet (M9).

export interface FileTreeProps {
  root: DirNode;
  // The file shown in the main view, if any.
  current: string | undefined;
  // The link for a file path.
  href: (path: string) => string;
}

interface Ctx {
  isCurrent: (path: string) => boolean;
  href: (path: string) => string;
  isOpen: (path: string) => boolean;
  toggle: (path: string) => void;
  // A file link mounted or went away.
  addLink: (path: string, el: HTMLAnchorElement) => void;
  removeLink: (path: string, el: HTMLAnchorElement) => void;
}

export function FileTree(props: FileTreeProps): JSX.Element {
  // One signal per folder, made when first asked for. Toggling a folder
  // only touches its own signal. Kept while a parent is closed.
  const folders = new Map<string, Signal<boolean>>();
  const folder = (path: string): Signal<boolean> => {
    let s = folders.get(path);
    if (s === undefined) {
      const [open, setOpen] = createSignal(false);
      s = [open, setOpen];
      folders.set(path, s);
    }
    return s;
  };
  // Only the old and the new current link update on a navigation.
  const isCurrent = createSelector(() => props.current);
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
  const ctx: Ctx = {
    isCurrent,
    href: (path) => props.href(path),
    isOpen: (path) => folder(path)[0](),
    toggle: (path) => {
      const [open, setOpen] = folder(path);
      setOpen(!open());
    },
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

function Level(props: { node: DirNode; ctx: Ctx; class?: string }) {
  return (
    <ul class={props.class}>
      <For each={props.node.dirs}>
        {(dir) => <Dir node={dir} ctx={props.ctx} />}
      </For>
      <For each={props.node.files}>
        {(file) => <File file={file} ctx={props.ctx} />}
      </For>
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

function File(props: { file: FileNode; ctx: Ctx }) {
  const path = () => props.file.entry.path;
  let link: HTMLAnchorElement | undefined;
  // After mount, so the link is in the page and can scroll.
  onMount(() => {
    if (link !== undefined) props.ctx.addLink(path(), link);
  });
  onCleanup(() => {
    if (link !== undefined) props.ctx.removeLink(path(), link);
  });
  const marker = () => {
    switch (props.file.entry.kind) {
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
      <a
        ref={(el) => (link = el)}
        class="tree-file"
        href={props.ctx.href(path())}
        aria-current={props.ctx.isCurrent(path()) ? "page" : undefined}
      >
        {props.file.name}
        <Show when={marker()}>
          {(m) => <span class="tree-marker"> {m()}</span>}
        </Show>
      </a>
    </li>
  );
}
