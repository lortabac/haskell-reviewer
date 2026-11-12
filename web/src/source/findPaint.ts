// Draws the hits with the CSS Custom Highlight API (T09): no DOM
// change, so copy, links and focus don't notice. One registry per
// document, shared by all views; each view owns its ranges.

// More ranges than this in one row: the rest aren't drawn.
export const maxRangesPerRow = 5000;

const all = "code-find";
const current = "code-find-current";

export function canPaint(): boolean {
  return (
    typeof CSS !== "undefined" &&
    "highlights" in CSS &&
    typeof Highlight !== "undefined"
  );
}

export interface Paint {
  // Replace this view's ranges. current: the one to stress.
  set: (ranges: readonly Range[], current: Range | undefined) => void;
  clear: () => void;
}

function shared(name: string, priority: number): Highlight {
  let h = CSS.highlights.get(name);
  if (h === undefined) {
    h = new Highlight();
    h.priority = priority;
    CSS.highlights.set(name, h);
  }
  return h;
}

export function createPaint(): Paint {
  let mine: Range[] = [];
  let mineCur: Range | undefined;
  const clear = () => {
    if (!canPaint()) return;
    const a = CSS.highlights.get(all);
    const c = CSS.highlights.get(current);
    for (const r of mine) a?.delete(r);
    if (mineCur !== undefined) c?.delete(mineCur);
    mine = [];
    mineCur = undefined;
    if (a?.size === 0) CSS.highlights.delete(all);
    if (c?.size === 0) CSS.highlights.delete(current);
  };
  const set = (ranges: readonly Range[], cur: Range | undefined) => {
    if (!canPaint()) return;
    clear();
    if (ranges.length === 0 && cur === undefined) return;
    const a = shared(all, 0);
    for (const r of ranges) a.add(r);
    mine = [...ranges];
    if (cur !== undefined) {
      shared(current, 1).add(cur);
      mineCur = cur;
    }
  };
  return { set, clear };
}
