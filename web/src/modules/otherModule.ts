// The same module on the other side (D-349). Pure, but for
// useModuleSide.
//
// By id, with the other tree's ref (a name can be ambiguous there).
// Else, if its file was renamed, the module at the other path (a loose
// module's id is its path). Else it is on this side only.
import { useDiffFiles } from "../diff/store";
import { onlyOn } from "../diff/words";
import type { FileMark } from "../files/changes";
import {
  type SideGo,
  type SideTarget,
  useSideTarget,
} from "../keys/KeysProvider";
import { usePageSide } from "../session";
import { type DiffSessionSide, otherSide } from "../sides";
import { type RefInfo, useModules } from "./store";

export type ModuleTarget =
  { kind: "go"; ref: string } | { kind: "off"; why: string };

// The other side's tree: loading, failed, or its index (indexRefs).
export type OtherTree =
  | { kind: "loading" }
  | { kind: "failed" }
  | { kind: "ok"; refs: ReadonlyMap<string, RefInfo> };

export function loadingModules(side: DiffSessionSide): string {
  return `Loading ${side}'s modules…`;
}

// ref: as in the URL. here: this side's index; marks: this side's file
// marks, by path.
export function otherModule(o: {
  side: DiffSessionSide;
  ref: string;
  here: ReadonlyMap<string, RefInfo>;
  other: OtherTree;
  marks: ReadonlyMap<string, FileMark> | undefined;
}): ModuleTarget {
  const info = o.here.get(o.ref);
  // Not in this side's tree (yet): the same ref; the page there says
  // if it has none.
  if (info === undefined) return { kind: "go", ref: o.ref };
  switch (o.other.kind) {
    case "loading":
      return { kind: "off", why: loadingModules(otherSide(o.side)) };
    case "failed":
      // No tree: trust the marker, and link by id (D-182).
      return info.change === "added" || info.change === "removed"
        ? { kind: "off", why: onlyOn(o.side) }
        : { kind: "go", ref: info.id };
    case "ok":
      break;
  }
  const refs = o.other.refs;
  const same = refs.get(info.id);
  if (same !== undefined && same.id === info.id) {
    return { kind: "go", ref: same.ref };
  }
  const m = info.path === null ? undefined : o.marks?.get(info.path);
  if (m?.status === "renamed") {
    for (const r of refs.values()) {
      if (r.path === m.other) return { kind: "go", ref: r.ref };
    }
  }
  return { kind: "off", why: onlyOn(o.side) };
}

// The header item's target for a module page (docs, refs). top: the
// other side's page for its ref. at: the exact place, only on use.
export function useModuleSide(
  ref: () => string,
  top: (side: DiffSessionSide, ref: string) => string,
  at?: (side: DiffSessionSide, ref: string) => SideGo,
  leave?: () => void,
): void {
  const side = usePageSide();
  const modules = useModules();
  const diff = useDiffFiles();
  const other = (): OtherTree => {
    const refs = modules.otherByRef();
    if (refs !== undefined) return { kind: "ok", refs };
    return modules.otherFailed() ? { kind: "failed" } : { kind: "loading" };
  };
  useSideTarget((): SideTarget | undefined => {
    const s = side();
    if (s === "tree") return undefined;
    const o = otherSide(s);
    const t = otherModule({
      side: s,
      ref: ref(),
      here: modules.byRef(),
      other: other(),
      marks: diff.marks()?.marks,
    });
    if (t.kind === "off") return { kind: "off", side: o, why: t.why };
    return {
      kind: "go",
      side: o,
      top: top(o, t.ref),
      ...(at === undefined ? {} : { at: () => at(o, t.ref) }),
      ...(leave === undefined ? {} : { leave }),
    };
  });
}
