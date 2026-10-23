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
import type {
  BrokenPackage,
  ComponentModules,
  ModuleEntry,
  ModuleTree as Tree,
  OtherFile,
  PackageModules,
} from "../api/modules";
import { compareCodePoints } from "../files/tree";
import { moduleMarker } from "./notices";
import { componentKey, otherKey, packageKey, type SourceInfo } from "./store";

// The module tree in the sidebar: packages, components, modules, then
// "Other files". Nested lists with disclosure buttons, like the file
// tree. Not an ARIA tree yet (M9).

export interface ModuleTreeProps {
  tree: Tree;
  bySource: ReadonlyMap<string, SourceInfo>;
  // The file shown in the main view, if any.
  current: string | undefined;
  // The link for a file path.
  href: (path: string) => string;
}

interface Ctx {
  isCurrent: (path: string) => boolean;
  href: (path: string) => string;
  isOpen: (key: string) => boolean;
  toggle: (key: string) => void;
  addLink: (path: string, el: HTMLAnchorElement) => void;
  removeLink: (path: string, el: HTMLAnchorElement) => void;
}

export function ModuleTree(props: ModuleTreeProps): JSX.Element {
  // Same pattern as FileTree: one signal per group, made when first
  // asked for.
  const groups = new Map<string, Signal<boolean>>();
  const group = (key: string): Signal<boolean> => {
    let s = groups.get(key);
    if (s === undefined) {
      const [open, setOpen] = createSignal(false);
      s = [open, setOpen];
      groups.set(key, s);
    }
    return s;
  };
  const isCurrent = createSelector(() => props.current);
  const links = new Map<string, HTMLAnchorElement>();
  let scrollTo: string | undefined;
  const scroll = (path: string): void => {
    const el = links.get(path);
    if (el === undefined) return;
    el.scrollIntoView({ block: "nearest" });
    scrollTo = undefined;
  };
  // When the current file changes: open its groups and scroll to it.
  createEffect(
    on(
      () => props.current,
      (current) => {
        scrollTo = current;
        if (current === undefined) return;
        const info = props.bySource.get(current);
        if (info === undefined) return;
        for (const key of info.groups) group(key)[1](true);
        scroll(current);
      },
    ),
  );
  const ctx: Ctx = {
    isCurrent,
    href: (path) => props.href(path),
    isOpen: (key) => group(key)[0](),
    toggle: (key) => {
      const [open, setOpen] = group(key);
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
  const implicit = () => props.tree.notices.some((n) => n.kind === "implicit");
  return (
    <ul class="module-tree">
      <For each={props.tree.packages}>
        {(p) => <Package pkg={p} ctx={ctx} />}
      </For>
      <For each={props.tree.broken}>{(b) => <Broken pkg={b} ctx={ctx} />}</For>
      <Show when={props.tree.other.length > 0}>
        <Group
          groupKey={otherKey}
          label={implicit() ? "Implicit package" : "Other files"}
          ctx={ctx}
        >
          <For each={props.tree.other}>
            {(o) => <Other file={o} ctx={ctx} />}
          </For>
        </Group>
      </Show>
    </ul>
  );
}

// A disclosure button and its list. Children only when open.
function Group(props: {
  groupKey: string;
  label: string;
  marker?: string | undefined;
  ctx: Ctx;
  children: JSX.Element;
}): JSX.Element {
  const isOpen = () => props.ctx.isOpen(props.groupKey);
  return (
    <li>
      <button
        type="button"
        class="tree-folder"
        aria-expanded={isOpen()}
        onClick={() => props.ctx.toggle(props.groupKey)}
      >
        {props.label}
        <Show when={props.marker}>
          {(m) => <span class="tree-marker"> {m()}</span>}
        </Show>
      </button>
      <Show when={isOpen()}>
        <ul>{props.children}</ul>
      </Show>
    </li>
  );
}

function Package(props: { pkg: PackageModules; ctx: Ctx }): JSX.Element {
  return (
    <Group
      groupKey={packageKey(props.pkg.cabalFile)}
      label={`${props.pkg.name} ${props.pkg.version}`}
      ctx={props.ctx}
    >
      <For each={props.pkg.components}>
        {(c) => (
          <Component cabalFile={props.pkg.cabalFile} comp={c} ctx={props.ctx} />
        )}
      </For>
    </Group>
  );
}

function Component(props: {
  cabalFile: string;
  comp: ComponentModules;
  ctx: Ctx;
}): JSX.Element {
  // Code point order, like the file tree.
  const sorted = createMemo(() =>
    [...props.comp.modules].sort((a, b) => compareCodePoints(a.name, b.name)),
  );
  return (
    <Group
      groupKey={componentKey(props.cabalFile, props.comp.name)}
      label={props.comp.name}
      marker={props.comp.buildable ? undefined : "not buildable"}
      ctx={props.ctx}
    >
      <For each={sorted()}>{(m) => <Module mod={m} ctx={props.ctx} />}</For>
    </Group>
  );
}

function Module(props: { mod: ModuleEntry; ctx: Ctx }): JSX.Element {
  const path = () => {
    const s = props.mod.source;
    return s.kind === "no-source" ? undefined : s.path;
  };
  return (
    <li>
      <Show
        when={path()}
        fallback={
          <span class="tree-entry">
            {props.mod.name}
            <Marker text={moduleMarker(props.mod)} />
          </span>
        }
      >
        {(p) => (
          <Link path={p()} ctx={props.ctx}>
            {props.mod.name}
            <Marker text={moduleMarker(props.mod)} />
          </Link>
        )}
      </Show>
    </li>
  );
}

function Broken(props: { pkg: BrokenPackage; ctx: Ctx }): JSX.Element {
  return (
    <li>
      <Link path={props.pkg.cabalFile} ctx={props.ctx}>
        {props.pkg.cabalFile}
        <Marker text="broken" />
      </Link>
      <p class="tree-problem">{props.pkg.problem}</p>
    </li>
  );
}

function Other(props: { file: OtherFile; ctx: Ctx }): JSX.Element {
  return (
    <li>
      <Link path={props.file.path} ctx={props.ctx}>
        {props.file.path}
        <Marker text={props.file.kind === "boot" ? "boot" : undefined} />
      </Link>
    </li>
  );
}

function Marker(props: { text: string | undefined }): JSX.Element {
  return (
    <Show when={props.text}>
      {(t) => <span class="tree-marker"> {t()}</span>}
    </Show>
  );
}

function Link(props: {
  path: string;
  ctx: Ctx;
  children: JSX.Element;
}): JSX.Element {
  let link: HTMLAnchorElement | undefined;
  onMount(() => {
    if (link !== undefined) props.ctx.addLink(props.path, link);
  });
  onCleanup(() => {
    if (link !== undefined) props.ctx.removeLink(props.path, link);
  });
  return (
    <a
      ref={(el) => (link = el)}
      class="tree-file"
      href={props.ctx.href(props.path)}
      aria-current={props.ctx.isCurrent(props.path) ? "page" : undefined}
    >
      {props.children}
    </a>
  );
}
