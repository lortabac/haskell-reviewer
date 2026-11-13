// Moving by declaration in the source view, with the outline (D-298).
import type { OutlineEntry } from "../api/source";

// The line of the next (dir 1) or previous (dir -1) entry from line,
// by the entry's name line. Undefined: none that way.
export function outlineStep(
  entries: readonly OutlineEntry[],
  line: number,
  dir: 1 | -1,
): number | undefined {
  let best: number | undefined;
  for (const e of entries) {
    const l = e[3];
    if (dir === 1 && l > line && (best === undefined || l < best)) best = l;
    if (dir === -1 && l < line && (best === undefined || l > best)) best = l;
  }
  return best;
}

// The entry a line is in: the smallest one that holds it. Else the
// one that ends last above it, so a line between two declarations
// goes to the one above.
export function declAtLine(
  entries: readonly OutlineEntry[],
  line: number,
): OutlineEntry | undefined {
  let inside: OutlineEntry | undefined;
  let above: OutlineEntry | undefined;
  for (const e of entries) {
    const [, , , , first, last] = e;
    if (first <= line && line <= last) {
      if (inside === undefined || last - first < inside[5] - inside[4]) {
        inside = e;
      }
    }
    // The one that ends last above the line; the outer one on a tie.
    if (
      last < line &&
      (above === undefined ||
        last > above[5] ||
        (last === above[5] && first < above[4]))
    ) {
      above = e;
    }
  }
  return inside ?? above;
}
