// Does the side-by-side diff fit (SPEC §5.4, D-344)? It needs 60rem
// of page width. Once it fell back on a page, it needs 62rem to come
// back: a page scrollbar coming and going can't make it flap.
import { createSignal, onCleanup, onMount } from "solid-js";

export const splitMin = 60;
export const splitBack = 62;

// prev: the last answer on this page, if any.
export function fits(
  prev: boolean | undefined,
  width: number,
  rem: number,
): boolean {
  return width >= (prev === false ? splitBack : splitMin) * rem;
}

// Watch el's width and the rem size (probe: an element 1rem wide, so
// text-only zoom counts too). With no layout (tests): it fits.
export function createFits(
  el: () => HTMLElement,
  probe: () => HTMLElement,
): () => boolean {
  const [ok, setOk] = createSignal(guess());
  let prev: boolean | undefined;
  const measure = () => {
    const rem = probe().getBoundingClientRect().width;
    if (rem <= 0) return;
    prev = fits(prev, el().getBoundingClientRect().width, rem);
    setOk(prev);
  };
  onMount(() => {
    measure();
    if (typeof ResizeObserver === "undefined") return;
    // In a frame, not in the callback: no "ResizeObserver loop" error
    // when the new layout changes the size again.
    let frame = 0;
    const ro = new ResizeObserver(() => {
      cancelAnimationFrame(frame);
      frame = requestAnimationFrame(measure);
    });
    ro.observe(el());
    ro.observe(probe());
    onCleanup(() => {
      ro.disconnect();
      cancelAnimationFrame(frame);
    });
  });
  return ok;
}

// Before the page is measured: false when even the window is too
// narrow, so a phone builds no split grid first.
function guess(): boolean {
  const root = document.documentElement;
  const rem = parseFloat(getComputedStyle(root).fontSize);
  const w = root.clientWidth;
  return !(w > 0 && rem > 0) || w >= splitMin * rem;
}
