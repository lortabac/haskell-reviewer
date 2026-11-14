// "Copy location" is pressed in the header, which has the focus by
// then. This remembers the item (matching sel) that had it in root.
// It counts while focus is in the header; focus anywhere else (the
// sidebar, say) forgets it.
import { onCleanup } from "solid-js";

export interface LastFocus {
  // The item with focus now, or the one it left for the header.
  get: () => HTMLElement | undefined;
}

// Call in a component: listens on document until it is gone.
export function lastFocus(
  root: () => Element | undefined,
  sel: string,
): LastFocus {
  let last: HTMLElement | undefined;
  const onFocusIn = (e: FocusEvent) => {
    const t = e.target;
    if (!(t instanceof Element)) return;
    const r = root();
    if (r !== undefined && r.contains(t)) {
      last = t.closest<HTMLElement>(sel) ?? undefined;
    } else if (t.closest("header") === null) {
      last = undefined;
    }
  };
  document.addEventListener("focusin", onFocusIn);
  onCleanup(() => document.removeEventListener("focusin", onFocusIn));
  return {
    get: () => {
      const r = root();
      const a = document.activeElement;
      if (r === undefined || a === null) return undefined;
      if (r.contains(a)) return a.closest<HTMLElement>(sel) ?? undefined;
      if (a.closest("header") !== null && last?.isConnected === true) {
        return last;
      }
      return undefined;
    },
  };
}
