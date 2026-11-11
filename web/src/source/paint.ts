// A line's pieces as DOM nodes: plain text as text, a class as
// <span class="hl-<class>">, control glyphs as <span class="code-ctrl">.
// Text only (textContent), never markup. No Solid: a row can have
// thousands of them, and nothing in them changes.
import type { StyledPiece } from "./display";

export function lineNodes(ps: readonly StyledPiece[]): Node[] {
  return ps.map((p) => {
    if (!p.ctrl && p.cls === null) return document.createTextNode(p.text);
    const s = document.createElement("span");
    s.className = p.ctrl ? "code-ctrl" : `hl-${p.cls}`;
    s.textContent = p.text;
    return s;
  });
}
