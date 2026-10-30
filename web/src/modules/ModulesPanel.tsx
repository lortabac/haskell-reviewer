import { useLocation, useResolvedPath } from "@solidjs/router";
import { createMemo, type JSX, Match, Show, Switch } from "solid-js";
import { hrefFor, pathFromPathname, withBase } from "../files/paths";
import { docsHref } from "../api/module";
import { refFromPathname } from "../docs/urls";
import { type Current, ModuleTree } from "./ModuleTree";
import { useModules } from "./store";

// The Modules view of the sidebar.
export function ModulesPanel(): JSX.Element {
  const modules = useModules();
  const location = useLocation();
  const current = createMemo((): Current | undefined => {
    const path = pathFromPathname(location.pathname);
    if (path !== undefined) return { kind: "path", path };
    const ref = refFromPathname(location.pathname);
    return ref === undefined ? undefined : { kind: "ref", ref };
  });
  const base = useResolvedPath(() => "/");
  const docs = (ref: string): string => withBase(base(), docsHref(ref));
  const src = (path: string): string => withBase(base(), hrefFor(path));
  const empty = () => {
    const t = modules.ok();
    return (
      t !== undefined &&
      t.packages.length === 0 &&
      t.broken.length === 0 &&
      t.other.length === 0
    );
  };
  return (
    <Switch>
      <Match when={modules.tree.loading}>
        <p>Loading modules…</p>
      </Match>
      <Match when={modules.ok()}>
        {(t) => (
          <Show when={!empty()} fallback={<p>No Haskell modules.</p>}>
            <ModuleTree
              tree={t()}
              bySource={modules.bySource()}
              byRef={modules.byRef()}
              current={current()}
              docsHref={docs}
              srcHref={src}
            />
          </Show>
        )}
      </Match>
      <Match when={true}>
        <p>Couldn't load the module tree. See the terminal for details.</p>
      </Match>
    </Switch>
  );
}
