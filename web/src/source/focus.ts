// Focus without losing the selection. WebKit clears the selection when
// the newly focused element doesn't hold its start (a range over lines
// 2-4, then focus on line 6). Chromium and Firefox keep it.

// Focus el, no scroll. If the browser dropped the selection, put the
// same ends back (same direction, so copy.ts still sees select-all).
export function focusKeepingSelection(el: HTMLElement): void {
  const sel = getSelection();
  const a = sel?.anchorNode ?? null;
  const f = sel?.focusNode ?? null;
  const ao = sel?.anchorOffset ?? 0;
  const fo = sel?.focusOffset ?? 0;
  el.focus({ preventScroll: true });
  if (sel === null || sel.rangeCount > 0 || a === null || f === null) return;
  if (!a.isConnected || !f.isConnected) return;
  // Rows may have gone meanwhile (WebKit): offsets past the end throw.
  sel.setBaseAndExtent(a, Math.min(ao, size(a)), f, Math.min(fo, size(f)));
}

// The largest offset in a node.
function size(n: Node): number {
  return n instanceof CharacterData ? n.length : n.childNodes.length;
}
