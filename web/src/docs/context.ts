// What every declaration on a module page needs: the page index, the
// source URL, anchors and the open state of instance lists.
import {
  type Accessor,
  createContext,
  createSignal,
  onCleanup,
  type Setter,
  useContext,
} from "solid-js";
import type { Handle } from "../api/module";
import type { PageIndex } from "./pageIndex";

export interface PageCtx {
  ix: PageIndex;
  // The file view URL at a line, if the module has a source.
  sourceHref: ((line: number) => string) | undefined;
  // Elements for anchors, by key (scope + handle, see anchorKey).
  anchors: Anchors;
  // Open state of the instance list of a type, class or family.
  instancesOpen: (home: Handle) => [Accessor<boolean>, Setter<boolean>];
}

export const PageContext = createContext<PageCtx>();

export function usePage(): PageCtx {
  const p = useContext(PageContext);
  if (p === undefined) throw new Error("usePage: no PageContext");
  return p;
}

// Where a declaration is drawn: "" in the main flow, "<home>/" in the
// instance list of home. An instance can be in several lists; the
// anchor goes to the one in its home's list.
export const ScopeContext = createContext<string>("");

export const anchorKey = (scope: string, n: Handle): string => scope + n;

// Elements by key. The first one wins: a handle can repeat (CPP).
// Ref callbacks run before the element is in the page, so no
// isConnected check: a removed one leaves through remove.
export class Anchors {
  private els = new Map<string, HTMLElement>();

  add(key: string, el: HTMLElement): void {
    if (!this.els.has(key)) this.els.set(key, el);
  }

  remove(key: string, el: HTMLElement): void {
    if (this.els.get(key) === el) this.els.delete(key);
  }

  get(key: string): HTMLElement | undefined {
    return this.els.get(key);
  }
}

// Open states, made on first use.
export function openStates(): PageCtx["instancesOpen"] {
  const states = new Map<Handle, [Accessor<boolean>, Setter<boolean>]>();
  return (n) => {
    let s = states.get(n);
    if (s === undefined) {
      const [get, set] = createSignal(false);
      s = [get, set];
      states.set(n, s);
    }
    return s;
  };
}

// A ref callback that makes el the anchor of handle n in the current
// scope. Call it in a component body: it removes el on cleanup.
export function useAnchor(n: Handle | undefined): (el: HTMLElement) => void {
  const page = usePage();
  const scope = useContext(ScopeContext);
  let mine: HTMLElement | undefined;
  onCleanup(() => {
    if (mine !== undefined && n !== undefined) {
      page.anchors.remove(anchorKey(scope, n), mine);
    }
  });
  return (el) => {
    if (n === undefined) return;
    mine = el;
    page.anchors.add(anchorKey(scope, n), el);
  };
}
