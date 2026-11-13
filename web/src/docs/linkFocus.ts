// Focus on a link in code or docs, kept when the link is drawn again
// (a refetch while partial, D-293). Before the change: note the box
// and the column of the focused link. After: if that link is gone,
// focus the link at the same column, else (it became plain text) the
// declaration around it.
import { focusKeepingSelection } from "../source/focus";
import { colIn } from "../source/selection";

const boxes = "code.code, code.sig, .doc";

export function linkFocus(root: () => HTMLElement | undefined): {
  before: () => void;
  after: () => void;
} {
  let spot: { a: Element; box: Element; col: number } | undefined;
  return {
    before: () => {
      spot = undefined;
      const a = document.activeElement;
      const r = root();
      if (!(a instanceof HTMLAnchorElement) || r === undefined) return;
      if (!r.contains(a)) return;
      const box = a.closest(boxes);
      if (box === null) return;
      spot = { a, box, col: colIn(box, a, 0) };
    },
    after: () => {
      const s = spot;
      spot = undefined;
      if (s === undefined || s.a.isConnected || !s.box.isConnected) return;
      // Someone else took focus meanwhile: leave it.
      const now = document.activeElement;
      if (now !== null && now !== document.body) return;
      const link = [...s.box.querySelectorAll<HTMLElement>("a[href]")].find(
        (x) => colIn(s.box, x, 0) === s.col,
      );
      if (link !== undefined) {
        focusKeepingSelection(link);
        return;
      }
      const anchor =
        s.box.closest<HTMLElement>('[data-handle][tabindex="-1"]') ??
        document.querySelector<HTMLElement>("#main h1");
      anchor?.focus({ preventScroll: true });
    },
  };
}
