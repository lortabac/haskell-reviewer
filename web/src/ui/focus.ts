// Small helpers shared by the views: loading delay and focus checks.
import { createEffect, createSignal, onCleanup } from "solid-js";

// True once loading() has been true for ms milliseconds, so fast loads
// don't flash a loading text.
export function createDelay(loading: () => boolean, ms: number): () => boolean {
  const [slow, setSlow] = createSignal(false);
  createEffect(() => {
    setSlow(false);
    if (!loading()) return;
    const t = setTimeout(() => setSlow(true), ms);
    onCleanup(() => clearTimeout(t));
  });
  return slow;
}

// Focus is on something inside the main view (not main itself).
export function focusInMain(): boolean {
  const el = document.activeElement;
  return el !== null && el.id !== "main" && el.closest("#main") !== null;
}

// Focus went nowhere: body, or an element that is gone.
export function focusLost(): boolean {
  const el = document.activeElement;
  return el === null || el === document.body || !el.isConnected;
}
