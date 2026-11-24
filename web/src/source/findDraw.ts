// Draws find hits in a grid's mounted cells (T09): the paint of all
// hits, and an outline box on the current one. Used by the source view
// and the diff view. Lines here are the finder's (find.ts Matches).
import { onCleanup, onMount } from "solid-js";
import { setVar } from "../ui/cssom";
import { type FindHits, inLine } from "./find";
import { createPaint, maxRangesPerRow } from "./findPaint";
import { nodeAt, nodesAt } from "./selection";

export interface FindDrawOptions {
  // The scroll box: the boxes go in it, in content coordinates.
  box: () => HTMLElement;
  // The mounted code cells and their lines.
  cells: () => Iterable<{ cell: HTMLElement; line: number }>;
  find: () => FindHits | undefined;
  // Scroll sideways so the current hit (its cell, its rect) is seen.
  reveal: (cell: HTMLElement, rect: DOMRect) => void;
  // Where the cell's text can be seen (client x): the box is cut to it.
  clip?: ((cell: HTMLElement) => { left: number; right: number }) | undefined;
}

export interface FindDraw {
  // Draw again: rows came or redrew, or the hits changed.
  repaint: () => void;
  // The next repaint that draws the current hit scrolls to it.
  revealNext: () => void;
  // Only the outline again (the cell moved sideways).
  outline: () => void;
}

// The current hit: its range and cell.
interface Hit {
  r: Range;
  cell: HTMLElement;
}

export function createFindDraw(o: FindDrawOptions): FindDraw {
  const paint = createPaint();
  onCleanup(paint.clear);
  // The outline of the current hit: boxes in the scroll box, outside
  // the cells (so not in a copy), in content coordinates.
  const boxes = document.createElement("div");
  boxes.className = "code-find-boxes";
  boxes.setAttribute("aria-hidden", "true");
  let last: Hit | undefined;
  const outline = (hit: Hit | undefined) => {
    last = hit;
    boxes.replaceChildren();
    if (hit === undefined || typeof hit.r.getClientRects !== "function") {
      return;
    }
    // One box: rows don't wrap. The union of the rects with a width
    // (a hit that starts on a token border gives an empty one, and a
    // link gives its own rect too).
    let left = Infinity;
    let top = Infinity;
    let right = -Infinity;
    let bottom = -Infinity;
    for (const q of hit.r.getClientRects()) {
      if (q.width <= 0) continue;
      left = Math.min(left, q.left);
      top = Math.min(top, q.top);
      right = Math.max(right, q.right ?? q.left + q.width);
      bottom = Math.max(bottom, q.bottom ?? q.top + q.height);
    }
    if (left === Infinity) return;
    if (o.clip !== undefined) {
      const c = o.clip(hit.cell);
      left = Math.max(left, c.left);
      right = Math.min(right, c.right);
      if (right <= left) return;
    }
    const box = o.box();
    const b = box.getBoundingClientRect();
    const el = document.createElement("div");
    el.className = "code-find-box";
    const x = left - b.left - box.clientLeft + box.scrollLeft;
    const y = top - b.top - box.clientTop + box.scrollTop;
    setVar(el, "--box-x", `${x}px`);
    setVar(el, "--box-y", `${y}px`);
    setVar(el, "--box-w", `${right - left}px`);
    setVar(el, "--box-h", `${bottom - top}px`);
    boxes.append(el);
  };
  onMount(() => o.box().append(boxes));
  // The view moves to the current hit, once its row is drawn.
  let reveal = false;
  const repaint = () => {
    const f = o.find();
    if (f === undefined) {
      paint.clear();
      outline(undefined);
      return;
    }
    const m = f.matches;
    const ranges: Range[] = [];
    let cur: Hit | undefined;
    const range = (a: [Node, number], z: [Node, number]) => {
      const r = document.createRange();
      r.setStart(...a);
      r.setEnd(...z);
      return r;
    };
    for (const { cell, line } of o.cells()) {
      const [lo, hi] = inLine(m, line);
      const end = Math.min(hi, lo + maxRangesPerRow);
      const cols: number[] = [];
      for (let i = lo; i < end; i++) cols.push(m.from[i]!, m.to[i]!);
      const at = nodesAt(cell, cols);
      for (let i = lo; i < end; i++) {
        const r = range(at[2 * (i - lo)]!, at[2 * (i - lo) + 1]!);
        if (i === f.current) cur = { r, cell };
        else ranges.push(r);
      }
      // The current one is always drawn, past the cap too.
      if (f.current >= end && f.current < hi) {
        cur = {
          r: range(
            nodeAt(cell, m.from[f.current]!),
            nodeAt(cell, m.to[f.current]!),
          ),
          cell,
        };
      }
    }
    paint.set(ranges, cur?.r);
    outline(cur);
    if (reveal && cur !== undefined) {
      reveal = false;
      o.reveal(cur.cell, cur.r.getBoundingClientRect());
      // It may have moved the view: the boxes again.
      outline(cur);
    }
  };
  return {
    repaint,
    revealNext: () => (reveal = true),
    outline: () => outline(last),
  };
}
