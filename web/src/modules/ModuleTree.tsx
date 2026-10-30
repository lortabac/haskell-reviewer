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
import {
  componentKey,
  otherKey,
  packageKey,
  type RefInfo,
  type SourceInfo,
} from "./store";

// The module tree in the sidebar: packages, components, modules, then
// "Other files". Nested lists with disclosure buttons, like the file
// tree. Not an ARIA tree yet (M9).

// What the main view shows: a module (by ref or id, as typed in the
// URL) or a file.
export type Current =
  { kind: "ref"; ref: string } | { kind: "path"; path: string };

// A sidebar link: a module's docs page, or a file.
type Target = { kind: "ref"; ref: string } | { kind: "path"; path: string };

export interface ModuleTreeProps {
  tree: Tree;
  bySource: ReadonlyMap<string, SourceInfo>;
  byRef: ReadonlyMap<string, RefInfo>;
  current: Current | undefined;
  // Links to a docs page and to a file.
  docsHref: (ref: string) => string;
  srcHref: (path: string) => string;
}

// A module is current on its docs page (by ref or id) and on its source
// file's page (D-182).
interface Ctx {
  isCurrent: (t: Target, path: string | undefined) => boolean;
  href: (t: Target) => string;
  isOpen: (key: string) => boolean;
  toggle: (key: string) => void;
  addLink: (keys: string[], el: HTMLAnchorElement) => void;
  removeLink: (keys: string[], el: HTMLAnchorElement) => void;
}

const refKey = (ref: string) => "r:" + ref;
const pathKey = (path: string) => "p:" + path;

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
  // The current module's ref as the tree has it (a URL can have the
  // id), or the current file.
  const currentRef = createMemo(() => {
    const c = props.current;
    return c?.kind === "ref"
      ? (props.byRef.get(c.ref)?.ref ?? c.ref)
      : undefined;
  });
  const currentPath = createMemo(() => {
    const c = props.current;
    return c?.kind === "path" ? c.path : undefined;
  });
  const isRef = createSelector(currentRef);
  const isPath = createSelector(currentPath);
  // Where it shows: groups to open, link to scroll to.
  const place = createMemo(() => {
    const ref = currentRef();
    if (ref !== undefined) {
      return { key: refKey(ref), groups: props.byRef.get(ref)?.groups };
    }
    const path = currentPath();
    if (path !== undefined) {
      return { key: pathKey(path), groups: props.bySource.get(path)?.groups };
    }
    return undefined;
  });
  const links = new Map<string, HTMLAnchorElement>();
  let scrollTo: string | undefined;
  const scroll = (key: string): void => {
    const el = links.get(key);
    if (el === undefined) return;
    el.scrollIntoView({ block: "nearest" });
    scrollTo = undefined;
  };
  // When the current module or file changes: open its groups and
  // scroll to it.
  createEffect(
    on(
      () => place()?.key,
      () => {
        const p = place();
        scrollTo = p?.key;
        if (p === undefined || p.groups === undefined) return;
        for (const key of p.groups) group(key)[1](true);
        scroll(p.key);
      },
    ),
  );
  const ctx: Ctx = {
    isCurrent: (t, path) =>
      (t.kind === "ref" && isRef(t.ref)) ||
      (path !== undefined && isPath(path)),
    href: (t) =>
      t.kind === "ref" ? props.docsHref(t.ref) : props.srcHref(t.path),
    isOpen: (key) => group(key)[0](),
    toggle: (key) => {
      const [open, setOpen] = group(key);
      setOpen(!open());
    },
    addLink: (keys, el) => {
      for (const k of keys) {
        if (!links.has(k)) links.set(k, el);
        if (scrollTo === k) scroll(k);
      }
    },
    removeLink: (keys, el) => {
      for (const k of keys) if (links.get(k) === el) links.delete(k);
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

// A module with a source links to its docs page (generated and
// preprocessed ones too: the page says why it's text only). No source:
// plain text.
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
          <Link
            target={{ kind: "ref", ref: props.mod.ref }}
            path={p()}
            ctx={props.ctx}
          >
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
      <Link
        target={{ kind: "path", path: props.pkg.cabalFile }}
        path={props.pkg.cabalFile}
        ctx={props.ctx}
      >
        {props.pkg.cabalFile}
        <Marker text="broken" />
      </Link>
      <p class="tree-problem">{props.pkg.problem}</p>
    </li>
  );
}

// A loose module links to its docs page; a boot file to the file.
function Other(props: { file: OtherFile; ctx: Ctx }): JSX.Element {
  const target = (): Target =>
    props.file.kind === "module"
      ? { kind: "ref", ref: props.file.ref }
      : { kind: "path", path: props.file.path };
  return (
    <li>
      <Link target={target()} path={props.file.path} ctx={props.ctx}>
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

// path: its file, so it is current on the file's page too.
function Link(props: {
  target: Target;
  path: string;
  ctx: Ctx;
  children: JSX.Element;
}): JSX.Element {
  let link: HTMLAnchorElement | undefined;
  const keys = () => {
    const t = props.target;
    return t.kind === "ref"
      ? [refKey(t.ref), pathKey(props.path)]
      : [pathKey(t.path)];
  };
  onMount(() => {
    if (link !== undefined) props.ctx.addLink(keys(), link);
  });
  onCleanup(() => {
    if (link !== undefined) props.ctx.removeLink(keys(), link);
  });
  return (
    <a
      ref={(el) => (link = el)}
      class="tree-file"
      href={props.ctx.href(props.target)}
      aria-current={
        props.ctx.isCurrent(props.target, props.path) ? "page" : undefined
      }
    >
      {props.children}
    </a>
  );
}
