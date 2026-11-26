import { useLocation, useResolvedPath } from "@solidjs/router";
import { createMemo, type JSX, Match, Show, Switch } from "solid-js";
import { docsHref } from "../api/module";
import { DiffFilesNotes, openAllUpTo, useDiffFiles } from "../diff/store";
import { refFromPathname } from "../docs/urls";
import { hrefFor, pathFromPathname, withBase } from "../files/paths";
import { usePrefs } from "../prefs/store";
import { usePageSide } from "../session";
import { otherSide, type Side } from "../sides";
import { type Current, ModuleTree } from "./ModuleTree";
import { loadingModulesText, useModules } from "./store";
import { changedModules, isEmptyView, unionModules } from "./union";

// The Modules view of the sidebar. In diff mode, with ghosts from the
// other side's tree and the "changed only" filter (D-337).
export function ModulesPanel(): JSX.Element {
  const modules = useModules();
  const diffFiles = useDiffFiles();
  const prefs = usePrefs();
  const location = useLocation();
  const side = usePageSide();
  const current = createMemo((): Current | undefined => {
    const path = pathFromPathname(location.pathname, side());
    if (path !== undefined) return { kind: "path", path };
    const ref = refFromPathname(location.pathname, side());
    return ref === undefined ? undefined : { kind: "ref", ref };
  });
  const base = useResolvedPath(() => "/");
  const docs = (s: Side, ref: string): string =>
    withBase(base(), docsHref(s, ref));
  const src = (s: Side, path: string): string =>
    withBase(base(), hrefFor(s, path));
  const view = createMemo(() => {
    const t = modules.ok();
    return t === undefined
      ? undefined
      : unionModules(t, modules.other(), side());
  });
  // Only on (view, the box): never on a navigation. Not in browse mode,
  // and not when pairing failed: the markers are all null then.
  const filter = createMemo(() => {
    const v = view();
    return v === undefined ||
      otherSide(side()) === undefined ||
      diffFiles.failed() ||
      !prefs.changedOnly()
      ? undefined
      : changedModules(v);
  });
  const defaultOpen = () => {
    const f = filter();
    return f !== undefined && f.count <= openAllUpTo;
  };
  // Diff mode, the other side's tree (its ghosts) not in yet.
  const otherLoading = () =>
    otherSide(side()) !== undefined &&
    modules.other() === undefined &&
    !modules.otherFailed();
  const otherFailed = () =>
    modules.otherFailed() ? otherSide(side()) : undefined;
  return (
    <Switch>
      {/* A reload keeps the old tree until the new one comes. */}
      <Match when={modules.loading() && modules.ok() === undefined}>
        <p>{loadingModulesText}</p>
      </Match>
      <Match when={view()}>
        {(v) => (
          <>
            <Show when={otherFailed()}>
              {(o) => <p class="tree-note">Couldn't load {o()}'s modules.</p>}
            </Show>
            <DiffFilesNotes loading={false} />
            <Show
              when={!isEmptyView(v())}
              fallback={<p>No Haskell modules.</p>}
            >
              {/* Above the tree, not instead of it: the current module
                  still shows, and open groups are kept. */}
              <Show when={filter()?.count === 0}>
                <p class="tree-note">
                  {otherLoading() ? "Loading changes…" : "No changed modules."}
                </p>
              </Show>
              <ModuleTree
                view={v()}
                side={side()}
                bySource={modules.bySource()}
                byRef={modules.byRef()}
                current={current()}
                docsHref={docs}
                srcHref={src}
                renames={diffFiles.marks()?.marks}
                filter={filter()}
                defaultOpen={defaultOpen()}
              />
            </Show>
          </>
        )}
      </Match>
      <Match when={true}>
        <p>Couldn't load the module tree. See the terminal for details.</p>
      </Match>
    </Switch>
  );
}
