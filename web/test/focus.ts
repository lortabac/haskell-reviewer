import { onTestFinished } from "vitest";

// Make any focus change drop the selection. WebKit only does it when
// the new focus doesn't hold the selection's start; this fake always
// does. Browsers send that selectionchange later (a task), happy-dom
// at once: it's hidden here. Only for the running test.
export function webkitFocus(): void {
  const stop = (e: Event) => e.stopImmediatePropagation();
  const drop = () => {
    document.addEventListener("selectionchange", stop, true);
    getSelection()?.removeAllRanges();
    document.removeEventListener("selectionchange", stop, true);
  };
  document.addEventListener("focusin", drop, true);
  onTestFinished(() => document.removeEventListener("focusin", drop, true));
}
