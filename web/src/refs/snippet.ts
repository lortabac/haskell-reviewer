// The line of a reference, cut in three: before the token, the token,
// after it. Pure.
import type { Ref } from "../api/refs";
import { utf16Cols } from "../source/columns";

export interface Snippet {
  // The server cut the start of the line: show "…".
  cut: boolean;
  before: string;
  hit: string;
  after: string;
}

export function snippetParts(r: Ref): Snippet {
  const [l1, c1, l2, c2] = r.span;
  // Offsets in code points from the line's first one. A token that
  // goes over a line end is cut at the end of this line.
  const from = Math.max(0, c1 - r.lineCol);
  const [a, b] = utf16Cols(r.line, [
    from + 1,
    l2 === l1 ? Math.max(from + 1, c2 - r.lineCol + 1) : Infinity,
  ]) as [number, number];
  let before = r.line.slice(0, a);
  // A whole line loses its indent; a cut one keeps what it has.
  if (r.lineCol === 1) before = before.trimStart();
  return {
    cut: r.lineCol > 1,
    before,
    hit: r.line.slice(a, b),
    after: r.line.slice(b),
  };
}
