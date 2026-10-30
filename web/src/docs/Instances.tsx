// Instance lists: closed "Instances (k)" under a type, class or family
// (D-191). Built only when open. The entries are in the home's scope,
// so an instance anchor lands in its home's list.
import { createMemo, For, type JSX, Show } from "solid-js";
import type { Handle } from "../api/module";
import { ScopeContext, usePage } from "./context";
import { Decl, type Shown } from "./Decl";

export function InstancesList(props: {
  home: Handle;
  list: Handle[];
  // The home's heading level.
  level: number;
}): JSX.Element {
  const page = usePage();
  const state = () => page.instancesOpen(props.home);
  const open = () => state()[0]();
  // Each once; only instances (a broken page could list others).
  const decls = createMemo(() => {
    const out: Shown[] = [];
    const seen = new Set<Handle>();
    for (const n of props.list) {
      if (seen.has(n)) continue;
      seen.add(n);
      const d = page.ix.byHandle.get(n);
      if (
        d !== undefined &&
        "kind" in d &&
        (d.kind === "instance" || d.kind === "faminst")
      ) {
        out.push(d);
      }
    }
    return out;
  });
  return (
    <Show when={decls().length > 0}>
      <details
        class="decl-instances"
        open={open()}
        onToggle={(e) => state()[1](e.currentTarget.open)}
      >
        <summary>Instances ({decls().length})</summary>
        <Show when={open()}>
          {/* eslint-disable-next-line solid/reactivity -- home never changes */}
          <ScopeContext.Provider value={`${props.home}/`}>
            <div class="decl-instance-list">
              <For each={decls()}>
                {(d) => (
                  <Decl decl={d} level={props.level + 1} home={props.home} />
                )}
              </For>
            </div>
          </ScopeContext.Provider>
        </Show>
      </details>
    </Show>
  );
}
