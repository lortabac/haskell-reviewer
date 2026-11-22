// What the module tree shows (D-337): this side's tree, plus ghosts for
// the modules only on the other side, and the "changed only" filter.
// Pure.
import type {
  BrokenPackage,
  Marker,
  ModuleEntry,
  ModuleTree,
  OtherFile,
} from "../api/modules";
import { compareCodePoints } from "../files/tree";
import { ghostChange, otherSide, type Side } from "../sides";
import { componentKey, otherKey, packageKey } from "./keys";

// ghostOn: the side it is only on (links there, never current).
// Undefined: on this side.
export interface ModuleItem {
  entry: ModuleEntry;
  ghostOn: Side | undefined;
}

export interface ComponentView {
  name: string;
  buildable: boolean;
  ghostOn: Side | undefined;
  // In code point order.
  modules: ModuleItem[];
}

export interface PackageView {
  name: string;
  version: string;
  cabalFile: string;
  ghostOn: Side | undefined;
  components: ComponentView[];
}

export interface OtherItem {
  file: OtherFile;
  ghostOn: Side | undefined;
}

export interface ModuleView {
  packages: PackageView[];
  broken: BrokenPackage[];
  // By path.
  other: OtherItem[];
  // The other group is the implicit package.
  implicit: boolean;
}

const byName = (a: ModuleItem, b: ModuleItem): number =>
  compareCodePoints(a.entry.name, b.entry.name);

const byPath = (a: OtherItem, b: OtherItem): number =>
  compareCodePoints(a.file.path, b.file.path);

// other: the other side's tree, if loaded. Its ghosts are its modules
// with the marker of an entry only there ("removed" seen from head):
// the server's markers decide, not an id compare here, so the two can't
// disagree. Ghost packages and components come after the real ones,
// ghost modules in name order with the rest.
export function unionModules(
  current: ModuleTree,
  other: ModuleTree | undefined,
  side: Side,
): ModuleView {
  const packages: PackageView[] = current.packages.map((p) => ({
    name: p.name,
    version: p.version,
    cabalFile: p.cabalFile,
    ghostOn: undefined,
    components: p.components.map((c) => ({
      name: c.name,
      buildable: c.buildable,
      ghostOn: undefined,
      modules: c.modules
        .map((m) => ({ entry: m, ghostOn: undefined }))
        .sort(byName),
    })),
  }));
  const otherItems: OtherItem[] = current.other.map((f) => ({
    file: f,
    ghostOn: undefined,
  }));
  const on = otherSide(side);
  const gone = on === undefined ? undefined : ghostChange(on);
  if (other !== undefined && on !== undefined && gone !== undefined) {
    const touched = new Set<ComponentView>();
    for (const p of other.packages) {
      for (const c of p.components) {
        const ghosts = c.modules.filter((m) => m.change === gone);
        if (ghosts.length === 0) continue;
        let pkg = packages.find((x) => x.cabalFile === p.cabalFile);
        if (pkg === undefined) {
          pkg = {
            name: p.name,
            version: p.version,
            cabalFile: p.cabalFile,
            ghostOn: on,
            components: [],
          };
          packages.push(pkg);
        }
        let comp = pkg.components.find((x) => x.name === c.name);
        if (comp === undefined) {
          comp = {
            name: c.name,
            buildable: c.buildable,
            ghostOn: on,
            modules: [],
          };
          pkg.components.push(comp);
        }
        for (const m of ghosts) comp.modules.push({ entry: m, ghostOn: on });
        touched.add(comp);
      }
    }
    for (const comp of touched) comp.modules.sort(byName);
    const paths = new Set(current.other.map((f) => f.path));
    let added = false;
    for (const f of other.other) {
      if (f.kind === "module" && f.change === gone && !paths.has(f.path)) {
        otherItems.push({ file: f, ghostOn: on });
        added = true;
      }
    }
    if (added) otherItems.sort(byPath);
  }
  return {
    packages,
    broken: current.broken,
    other: otherItems,
    implicit: current.notices.some((n) => n.kind === "implicit"),
  };
}

// Marked in diff mode: changed, or its settings are.
export function isChanged(m: Marker): boolean {
  return m.change !== null || m.settingsChanged;
}

// "Changed only" keeps it: a ghost, or a changed module.
export function keepModule(m: ModuleItem): boolean {
  return m.ghostOn !== undefined || isChanged(m.entry);
}

// Boot files and loose files with no marker go.
export function keepOther(o: OtherItem): boolean {
  return (
    o.ghostOn !== undefined || (o.file.kind === "module" && isChanged(o.file))
  );
}

// "Changed only" for the module tree: the groups with a kept row. The
// tree decides as it renders (with the current module kept too), so
// its rows stay the same objects and nothing mounts again on a
// navigation. Broken packages always show: they can't say what changed.
export interface ModuleFilter {
  // Keys of packageKey, componentKey and otherKey.
  groups: ReadonlySet<string>;
  // Rows kept: modules, loose files, broken packages.
  count: number;
}

export function changedModules(view: ModuleView): ModuleFilter {
  const groups = new Set<string>();
  let count = view.broken.length;
  for (const p of view.packages) {
    for (const c of p.components) {
      const n = c.modules.filter(keepModule).length;
      if (n === 0) continue;
      count += n;
      groups.add(packageKey(p.cabalFile));
      groups.add(componentKey(p.cabalFile, c.name));
    }
  }
  const n = view.other.filter(keepOther).length;
  if (n > 0) groups.add(otherKey);
  return { groups, count: count + n };
}

// Anything to show?
export function isEmptyView(view: ModuleView): boolean {
  return (
    view.packages.length === 0 &&
    view.broken.length === 0 &&
    view.other.length === 0
  );
}
