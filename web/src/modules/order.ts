// ] and [ in docs (D-350): the changed modules in tree order. Pure.
import type { Marker, ModuleTree } from "../api/modules";
import { type FileStep, stepIndex } from "../keys/fileSteps";
import { memoBy } from "../memo";
import { isChanged, unionModules } from "./union";

export const moduleStepTexts = {
  noMore: "No more changed modules.",
  none: "No changed modules.",
  failed: "Couldn't load the modules.",
};

export interface ModuleOrder {
  // Every module of the tree, so a page that isn't changed has a place
  // too.
  all: readonly { ref: string; name: string }[];
  // By ref: its place in all.
  at: ReadonlyMap<string, number>;
  // The places of the changed ones, ascending.
  changed: readonly number[];
}

// The tree's modules as the sidebar shows them (packages, components,
// modules, then loose modules), one per ref. No no-source modules: no
// page. A tree is one side's, and with no other tree unionModules adds
// no ghosts, so its side argument doesn't matter here.
export const moduleOrder = memoBy((tree: ModuleTree): ModuleOrder => {
  const view = unionModules(tree, undefined, "tree");
  const all: { ref: string; name: string }[] = [];
  const at = new Map<string, number>();
  const changed: number[] = [];
  const add = (ref: string, name: string, m: Marker) => {
    if (at.has(ref)) return;
    at.set(ref, all.length);
    if (isChanged(m)) changed.push(all.length);
    all.push({ ref, name });
  };
  for (const p of view.packages) {
    for (const c of p.components) {
      for (const { entry: m } of c.modules) {
        if (m.source.kind !== "no-source") add(m.ref, m.name, m);
      }
    }
  }
  for (const { file: f } of view.other) {
    if (f.kind === "module") add(f.ref, f.name ?? f.path, f);
  }
  return { all, at, changed };
});

// ] or [ from ref: the changed module there, or why not. A ref not in
// the tree counts as before the first.
export function moduleStep(
  order: ModuleOrder,
  ref: string,
  dir: 1 | -1,
  href: (ref: string) => string,
): FileStep {
  const n = order.changed.length;
  if (n === 0) return moduleStepTexts.none;
  const pos = order.at.get(ref) ?? -1;
  const i = stepIndex(n, (k) => (order.changed[k] ?? 0) - pos, dir);
  const m = i === undefined ? undefined : order.all[order.changed[i] ?? -1];
  if (i === undefined || m === undefined) return moduleStepTexts.noMore;
  return {
    href: href(m.ref),
    words: `${m.name}, changed module ${i + 1} of ${n}.`,
  };
}
