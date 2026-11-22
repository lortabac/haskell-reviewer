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
import type { BrokenPackage, Marker as Marks } from "../api/modules";
import { CopyButton } from "../copy/CopyButton";
import { type FileMark, renameText } from "../files/changes";
import type { Side } from "../sides";
import { ChangeSign, GhostSide, ghostLabel } from "../ui/ChangeSign";
import { componentKey, otherKey, packageKey } from "./keys";
import { moduleMarker } from "./notices";
import type { RefInfo, SourceInfo } from "./store";
import {
  type ComponentView,
  keepModule,
  keepOther,
  type ModuleFilter,
  type ModuleItem,
  type ModuleView,
  type OtherItem,
  type PackageView,
} from "./union";

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
  // From unionModules: the page side's tree, with ghosts in diff mode.
  view: ModuleView;
  // The page's side. Ghosts link to their own.
  side: Side;
  bySource: ReadonlyMap<string, SourceInfo>;
  byRef: ReadonlyMap<string, RefInfo>;
  current: Current | undefined;
  // Links to a docs page and to a file, on a side.
  docsHref: (side: Side, ref: string) => string;
  srcHref: (side: Side, path: string) => string;
  // Diff mode: rename markers by source path, for "renamed from".
  renames?: ReadonlyMap<string, FileMark> | undefined;
  // "Changed only" (D-337). The current module and its groups always
  // show.
  filter?: ModuleFilter | undefined;
  // Groups the user hasn't opened or closed: open? Default no.
  defaultOpen?: boolean;
}

// A module is current on its docs page (by ref or id) and on its source
// file's page (D-182).
interface Ctx {
  side: () => Side;
  isCurrent: (t: Target, path: string | undefined) => boolean;
  href: (side: Side, t: Target) => string;
  renameTip: (
    change: Marks["change"],
    path: string | undefined,
  ) => string | undefined;
  showGroup: (key: string) => boolean;
  showModule: (m: ModuleItem) => boolean;
  showOther: (o: OtherItem) => boolean;
  isOpen: (key: string) => boolean;
  toggle: (key: string) => void;
  addLink: (keys: string[], el: HTMLAnchorElement) => void;
  removeLink: (keys: string[], el: HTMLAnchorElement) => void;
}

const refKey = (ref: string) => "r:" + ref;
const pathKey = (path: string) => "p:" + path;

const modulePath = (m: ModuleItem): string | undefined =>
  m.entry.source.kind === "no-source" ? undefined : m.entry.source.path;

export function ModuleTree(props: ModuleTreeProps): JSX.Element {
  // Same pattern as FileTree: one signal per group, made when first
  // asked for. Undefined: not touched, the default applies.
  const groups = new Map<string, Signal<boolean | undefined>>();
  const group = (key: string): Signal<boolean | undefined> => {
    let s = groups.get(key);
    if (s === undefined) {
      const [open, setOpen] = createSignal<boolean | undefined>(undefined);
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
  const currentGroups = createMemo(() => new Set(place()?.groups ?? []));
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
  const isOpen = (key: string): boolean =>
    group(key)[0]() ?? props.defaultOpen ?? false;
  const isCurrent = (t: Target, path: string | undefined): boolean =>
    (t.kind === "ref" && isRef(t.ref)) || (path !== undefined && isPath(path));
  const ctx: Ctx = {
    side: () => props.side,
    isCurrent,
    href: (side, t) =>
      t.kind === "ref"
        ? props.docsHref(side, t.ref)
        : props.srcHref(side, t.path),
    renameTip: (change, path) => {
      if (change !== "renamed" || path === undefined) return undefined;
      const m = props.renames?.get(path);
      return m?.status === "renamed" ? renameText(m, props.side) : undefined;
    },
    showGroup: (key) =>
      props.filter === undefined ||
      props.filter.groups.has(key) ||
      currentGroups().has(key),
    showModule: (m) =>
      props.filter === undefined ||
      keepModule(m) ||
      isCurrent({ kind: "ref", ref: m.entry.ref }, modulePath(m)),
    showOther: (o) =>
      props.filter === undefined ||
      keepOther(o) ||
      isCurrent(
        o.file.kind === "module"
          ? { kind: "ref", ref: o.file.ref }
          : { kind: "path", path: o.file.path },
        o.file.path,
      ),
    isOpen,
    toggle: (key) => group(key)[1](!isOpen(key)),
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
  // The view's own objects, filtered here: a filter change or a
  // navigation mounts only the rows that come or go.
  const packages = createMemo(() =>
    props.view.packages.filter((p) => ctx.showGroup(packageKey(p.cabalFile))),
  );
  const other = createMemo(() =>
    ctx.showGroup(otherKey) ? props.view.other.filter(ctx.showOther) : [],
  );
  return (
    <ul class="module-tree">
      <For each={packages()}>{(p) => <Package pkg={p} ctx={ctx} />}</For>
      <For each={props.view.broken}>{(b) => <Broken pkg={b} ctx={ctx} />}</For>
      <Show when={other().length > 0}>
        <Group
          groupKey={otherKey}
          label={props.view.implicit ? "Implicit package" : "Other files"}
          ctx={ctx}
        >
          <For each={other()}>{(o) => <Other item={o} ctx={ctx} />}</For>
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

function Package(props: { pkg: PackageView; ctx: Ctx }): JSX.Element {
  const components = createMemo(() =>
    props.pkg.components.filter((c) =>
      props.ctx.showGroup(componentKey(props.pkg.cabalFile, c.name)),
    ),
  );
  return (
    <Group
      groupKey={packageKey(props.pkg.cabalFile)}
      label={`${props.pkg.name} ${props.pkg.version}`}
      marker={props.pkg.ghostOn && ghostLabel(props.pkg.ghostOn)}
      ctx={props.ctx}
    >
      <For each={components()}>
        {(c) => (
          <Component cabalFile={props.pkg.cabalFile} comp={c} ctx={props.ctx} />
        )}
      </For>
    </Group>
  );
}

function Component(props: {
  cabalFile: string;
  comp: ComponentView;
  ctx: Ctx;
}): JSX.Element {
  const marker = () => {
    const on = props.comp.ghostOn;
    const parts = [
      props.comp.buildable ? undefined : "not buildable",
      on && ghostLabel(on),
    ].filter((x) => x !== undefined);
    return parts.length === 0 ? undefined : parts.join(" ");
  };
  // The modules come in code point order, like the file tree.
  const modules = createMemo(() =>
    props.comp.modules.filter(props.ctx.showModule),
  );
  return (
    <Group
      groupKey={componentKey(props.cabalFile, props.comp.name)}
      label={props.comp.name}
      marker={marker()}
      ctx={props.ctx}
    >
      <For each={modules()}>{(m) => <Module item={m} ctx={props.ctx} />}</For>
    </Group>
  );
}

// The diff signs of a module: its change, and settings.
function Signs(props: {
  mod: Marks;
  renamed: string | undefined;
}): JSX.Element {
  return (
    <>
      <Show when={props.mod.change}>
        {(c) => <ChangeSign change={c()} word={props.renamed} />}
      </Show>
      <Show when={props.mod.settingsChanged}>
        <ChangeSign change="settings" />
      </Show>
    </>
  );
}

// A module with a source links to its docs page (generated and
// preprocessed ones too: the page says why it's text only). No source:
// plain text.
function Module(props: { item: ModuleItem; ctx: Ctx }): JSX.Element {
  const mod = () => props.item.entry;
  const path = () => modulePath(props.item);
  const renamed = () => props.ctx.renameTip(mod().change, path());
  const body = () => (
    <>
      {mod().name}
      <Marker text={moduleMarker(mod())} />
      <Signs mod={mod()} renamed={renamed()} />
      <Show when={props.item.ghostOn}>{(on) => <GhostSide on={on()} />}</Show>
    </>
  );
  return (
    <li>
      <Show when={path()} fallback={<span class="tree-entry">{body()}</span>}>
        {(p) => (
          <Link
            target={{ kind: "ref", ref: mod().ref }}
            path={p()}
            copy={mod().name}
            copyIs="module name"
            ghostOn={props.item.ghostOn}
            tip={renamed()}
            ctx={props.ctx}
          >
            {body()}
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
        copy={props.pkg.cabalFile}
        copyIs="path"
        ghostOn={undefined}
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
function Other(props: { item: OtherItem; ctx: Ctx }): JSX.Element {
  // What the row links to and copies, and its markers.
  const row = () => {
    const f = props.item.file;
    return f.kind === "module"
      ? {
          target: { kind: "ref", ref: f.ref } as const,
          copy: f.name ?? f.path,
          copyIs:
            f.name === null ? ("path" as const) : ("module name" as const),
          marks: f,
        }
      : {
          target: { kind: "path", path: f.path } as const,
          copy: f.path,
          copyIs: "path" as const,
          marks: undefined,
        };
  };
  const renamed = () =>
    props.ctx.renameTip(row().marks?.change ?? null, props.item.file.path);
  return (
    <li>
      <Link
        target={row().target}
        path={props.item.file.path}
        copy={row().copy}
        copyIs={row().copyIs}
        ghostOn={props.item.ghostOn}
        tip={renamed()}
        ctx={props.ctx}
      >
        {props.item.file.path}
        <Marker text={props.item.file.kind === "boot" ? "boot" : undefined} />
        <Show when={row().marks}>
          {(m) => <Signs mod={m()} renamed={renamed()} />}
        </Show>
        <Show when={props.item.ghostOn}>{(on) => <GhostSide on={on()} />}</Show>
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
  // What Copy gives: the module name, or the path.
  copy: string;
  copyIs: "module name" | "path";
  // Only on that side: links there, never current, not in the link
  // index (its ref or path may clash with a real one).
  ghostOn: Side | undefined;
  // A longer text on hover and focus (a rename's).
  tip?: string | undefined;
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
  // Fixed for the row: For makes a new row for a new item.
  const ghost = (): boolean => props.ghostOn !== undefined;
  onMount(() => {
    if (link !== undefined && !ghost()) props.ctx.addLink(keys(), link);
  });
  onCleanup(() => {
    if (link !== undefined && !ghost()) props.ctx.removeLink(keys(), link);
  });
  return (
    <div class="tree-row">
      <a
        ref={(el) => (link = el)}
        class={ghost() ? "tree-file tree-ghost" : "tree-file"}
        href={props.ctx.href(props.ghostOn ?? props.ctx.side(), props.target)}
        data-copy={props.copy}
        data-tip={props.tip}
        aria-current={
          !ghost() && props.ctx.isCurrent(props.target, props.path)
            ? "page"
            : undefined
        }
      >
        {props.children}
      </a>
      {/* Mouse only: the keyboard has y. */}
      <CopyButton
        text={() => props.copy}
        label={`Copy ${props.copyIs} ${props.copy}`}
        mouseOnly
      />
    </div>
  );
}
