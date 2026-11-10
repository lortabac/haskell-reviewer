// Helpers for the virtualiser.
import { defaultRangeExtractor, type Range } from "@tanstack/solid-virtual";
import {
  createEffect,
  createRoot,
  createSignal,
  onCleanup,
  type Owner,
} from "solid-js";

// The rows to mount: the window around the screen, plus pinned lines
// (indexes from 0) anywhere in the file: the cursor's line and the
// selection's ends. Sorted, so DOM order is line order.
export function withPins(range: Range, pins: Iterable<number>): number[] {
  const rows = defaultRangeExtractor(range);
  const extra = [...pins].filter(
    (i) => Number.isInteger(i) && i >= 0 && i < range.count,
  );
  if (extra.length === 0) return rows;
  return [...new Set([...rows, ...extra])].sort((a, b) => a - b);
}

// Row height in px, from a hidden probe row, kept up to date (zoom,
// font changes). Until a probe is measured, or with no layout (tests):
// fallback. Call observe from onMount.
export function createRowHeight(fallback: number): {
  height: () => number;
  observe: (probe: HTMLElement) => void;
} {
  const [height, setHeight] = createSignal(fallback);
  const observe = (probe: HTMLElement) => {
    const measure = () => {
      const h = probe.getBoundingClientRect().height;
      setHeight(h > 0 ? h : fallback);
    };
    measure();
    if (typeof ResizeObserver === "undefined") return;
    const ro = new ResizeObserver(measure);
    ro.observe(probe);
    onCleanup(() => ro.disconnect());
  };
  return { height, observe };
}

// Keeps the rows of lines() (sorted) in the container, by hand: a new
// row goes in before the next mounted one, an old one is removed, and
// no row is ever moved. <For> can move or replace a row that stays,
// and that loses focus and moves a selection end inside it. Each row
// gets its own root under owner, so context works in render.
export function mountRows(
  container: () => HTMLElement,
  lines: () => number[],
  render: (line: number) => Node,
  owner: Owner | null,
): void {
  const mounted = new Map<number, { el: Node; dispose: () => void }>();
  createEffect(() => {
    const want = lines();
    const box = container();
    const keep = new Set(want);
    for (const [n, r] of mounted) {
      if (keep.has(n)) continue;
      box.removeChild(r.el);
      r.dispose();
      mounted.delete(n);
    }
    let next: Node | null = null;
    for (const n of [...want].reverse()) {
      let r = mounted.get(n);
      if (r === undefined) {
        r = createRoot((dispose) => ({ el: render(n), dispose }), owner);
        box.insertBefore(r.el, next);
        mounted.set(n, r);
      }
      next = r.el;
    }
  });
  onCleanup(() => {
    for (const r of mounted.values()) r.dispose();
  });
}
