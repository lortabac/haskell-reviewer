// A line on one side to the same place on the other side, through the
// diff rows (D-349). Pure.
//
// - Equal row: the other side's line there (exact).
// - In a change block with lines on both sides: the i-th line here
//   goes to the i-th line there, as split.ts splitOf pairs them (the
//   test checks it against splitOf); past the end of the other side's
//   lines, its last one (paired).
// - In a block with lines on one side only: the nearest equal row,
//   the one just above or just below the block; a tie goes up
//   (nearest).
import type { DiffSessionSide } from "../sides";
import { equalRow, type Rows } from "./rows";

export interface Mapped {
  line: number;
  how: "exact" | "paired" | "nearest";
}

// Undefined: no such line here, or the other side has no lines.
export function mapLine(
  rows: Rows,
  from: DiffSessionSide,
  line: number,
): Mapped | undefined {
  const here = from === "base" ? rows.base : rows.head;
  const there = from === "base" ? rows.head : rows.base;
  const lineRow = from === "base" ? rows.baseRow : rows.headRow;
  const count = from === "base" ? rows.baseCount : rows.headCount;
  const otherCount = from === "base" ? rows.headCount : rows.baseCount;
  if (line < 1 || line > count || otherCount === 0) return undefined;
  const r = lineRow[line] ?? 0;
  const { kind } = rows;
  if (kind[r] === equalRow) return { line: there[r] ?? 0, how: "exact" };
  // The block [s, e) around r.
  let s = r;
  while (s > 0 && kind[s - 1] !== equalRow) s--;
  let e = r + 1;
  while (e < rows.count && kind[e] !== equalRow) e++;
  // The other side's first line in the block, and how many it has.
  let first = 0;
  let n = 0;
  let i = 0;
  for (let k = s; k < e; k++) {
    const t = there[k] ?? 0;
    if (t > 0) {
      if (first === 0) first = t;
      n++;
    }
    // This side's lines before r in the block.
    if (k < r && (here[k] ?? 0) > 0) i++;
  }
  if (n > 0) return { line: first + Math.min(i, n - 1), how: "paired" };
  // One side only: the equal row above (s - 1) or below (e).
  const up = s > 0 ? r - (s - 1) : Infinity;
  const down = e < rows.count ? e - r : Infinity;
  const to = up <= down ? s - 1 : e;
  return { line: there[to] ?? 0, how: "nearest" };
}
