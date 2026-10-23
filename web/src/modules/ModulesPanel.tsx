import { useLocation, useResolvedPath } from "@solidjs/router";
import { createMemo, type JSX, Match, Show, Switch } from "solid-js";
import { hrefFor, pathFromPathname, withBase } from "../files/paths";
import { ModuleTree } from "./ModuleTree";
import { useModules } from "./store";

// The Modules view of the sidebar.
export function ModulesPanel(): JSX.Element {
  const modules = useModules();
  const location = useLocation();
  const current = createMemo(() => pathFromPathname(location.pathname));
  const base = useResolvedPath(() => "/");
  const href = (path: string): string => withBase(base(), hrefFor(path));
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
              current={current()}
              href={href}
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
