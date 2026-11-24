// Sideways scroll of the diff grid. The grid only calls a SideScroll:
// scroll by, and the x range where a cell's text can be seen.
//
// Split (D-344): one offset for both sides. The box scrolls natively
// (its scrollbar, wheel, trackpad), the halves stay put (sticky) and
// the code cells move by --diff-x. All of it is here, in the
// .diff-split-view CSS and in splitRow's .diff-clip box, so it can be
// swapped.
import { onCleanup } from "solid-js";
import { setVar } from "../ui/cssom";

export interface SideScroll {
  by: (dx: number) => void;
  // Where text in el's cell is in view: right of the sticky gutter,
  // left of the cell's right edge (client px).
  bounds: (el: Element) => { left: number; right: number };
}

// Unified: the box scrolls; the gutter is the sign cell.
export function plainScroll(box: HTMLElement): SideScroll {
  return {
    by: (dx) => {
      if (dx !== 0) box.scrollLeft += dx;
    },
    bounds: (el) => {
      const b = box.getBoundingClientRect();
      const sign = el.closest(".code-row")?.querySelector(".diff-sign");
      return {
        left: sign?.getBoundingClientRect().right ?? b.left,
        right: b.left + box.clientWidth,
      };
    },
  };
}

// Split: --diff-x follows the box's offset, also right after a scroll
// by hand (the scroll event comes a frame later).
export function sharedScroll(box: HTMLElement, rows: HTMLElement): SideScroll {
  let last = -1;
  const sync = () => {
    const x = box.scrollLeft;
    if (x === last) return;
    last = x;
    setVar(rows, "--diff-x", `${x}px`);
  };
  box.addEventListener("scroll", sync, { passive: true });
  onCleanup(() => box.removeEventListener("scroll", sync));
  return {
    by: (dx) => {
      if (dx === 0) return;
      box.scrollLeft += dx;
      sync();
    },
    bounds: (el) => {
      const half = el.closest(".diff-half");
      const b = box.getBoundingClientRect();
      return {
        left:
          half?.querySelector(".code-num")?.getBoundingClientRect().right ??
          b.left,
        right:
          half?.querySelector(".diff-clip")?.getBoundingClientRect().right ??
          b.left + box.clientWidth,
      };
    },
  };
}
