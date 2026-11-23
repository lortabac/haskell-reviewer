// Cursor and row helpers shared by the source view (CodeView) and the
// diff view (DiffGrid): roving tab stop, focus that waits for its row,
// row height changes, drawing a cell. See D-277, D-284, D-341.
import {
  createEffect,
  createRenderEffect,
  onCleanup,
  onMount,
  untrack,
} from "solid-js";
import { setVar } from "../ui/cssom";
import type { Spot, StopAt } from "./keys";
import { replaceKeepingSelection } from "./selection";

// Every field the same (cursors are flat records).
export function sameFields<T extends object>(a: T, b: T): boolean {
  return (Object.keys(a) as (keyof T)[]).every((k) => a[k] === b[k]);
}

// Roving tabindex: the cursor's stop (a link, a button) gets tabindex
// 0, the one before -1. The cell's own tabindex is the view's. Rows are
// drawn by hand, so this is set by hand. Returns apply(cell, at): false
// if at is a stop that isn't there (a redraw took it).
export function createRovingStop(
  stopsOf: (cell: Element) => StopAt[],
): (cell: Element | undefined, at: Spot) => boolean {
  let stopEl: HTMLElement | undefined;
  return (cell, at) => {
    let el: HTMLElement | undefined;
    // No cell (not mounted): false for a link, so the caller drops it.
    // The cursor's row is pinned, so this doesn't happen today.
    if (at.link && cell !== undefined) {
      el = stopsOf(cell).find((s) => s.start === at.col)?.el;
    }
    if (stopEl !== undefined && stopEl !== el) stopEl.tabIndex = -1;
    stopEl = el;
    if (el !== undefined) el.tabIndex = 0;
    return !at.link || el !== undefined;
  };
}

// A focus that waits for its row: the cursor's row is mounted by an
// effect, so inside an effect it may not be there yet. attempt: false
// if not yet. retry after the rows change. Focus that goes outside the
// box drops it, so it never takes focus back later.
export function createPendingFocus<P>(
  box: () => HTMLElement,
  attempt: (p: P) => boolean,
): {
  request: (p: P) => void;
  retry: () => void;
  clear: () => void;
  get: () => P | undefined;
} {
  let pending: P | undefined;
  const onAnyFocus = (e: FocusEvent) => {
    if (!(e.target instanceof Node) || !box().contains(e.target)) {
      pending = undefined;
    }
  };
  onMount(() => {
    document.addEventListener("focusin", onAnyFocus);
    onCleanup(() => document.removeEventListener("focusin", onAnyFocus));
  });
  return {
    request: (p) => {
      pending = undefined;
      if (!attempt(p)) pending = p;
    },
    retry: () => {
      const p = pending;
      if (p !== undefined && attempt(p)) pending = undefined;
    },
    clear: () => {
      pending = undefined;
    },
    get: () => pending,
  };
}

// The focus target from a focus event in a cell: the stop it is on, or
// the cell itself at cellCol. Undefined: something else.
export function focusSpot(
  target: HTMLElement,
  cell: Element,
  stops: readonly StopAt[],
  cellCol: number,
): Spot | undefined {
  if (target === cell) return { link: false, col: cellCol };
  const s = stops.find((x) => x.el === target);
  return s === undefined ? undefined : { link: true, col: s.start };
}

// A new row height (the probe's first measure, zoom, fonts): keep the
// same row at the top. The virtualiser only sees it on measure().
// after: once all is moved.
export function keepTopOnNewHeight(opts: {
  height: () => number;
  fallback: number;
  box: () => HTMLElement;
  rows: () => HTMLElement;
  v: { measure: () => void; getTotalSize: () => number };
  after?: () => void;
}): void {
  let last = opts.fallback;
  createEffect(() => {
    const h = opts.height();
    if (h === last) return;
    const box = opts.box();
    const top = box.scrollTop / last;
    last = h;
    opts.v.measure();
    // The new height first: the browser clamps scrollTop to the old one.
    setVar(opts.rows(), "--code-h", `${opts.v.getTotalSize()}px`);
    box.scrollTop = top * h;
    opts.after?.();
  });
}

// A cell's children, drawn by hand: a new render swaps them under a
// live selection, which must stay where it was. after(had): with the
// new children in, untracked; had: focus was in the cell before.
export function drawCell(
  cell: () => HTMLElement,
  nodes: () => Node[],
  after: (had: boolean) => void,
): void {
  createRenderEffect(() => {
    const ns = nodes();
    const el = cell();
    const active = document.activeElement;
    const had = active !== null && el.contains(active);
    replaceKeepingSelection(el, ns);
    untrack(() => after(had));
  });
}
