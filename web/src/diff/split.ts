// The rows of the side-by-side diff (D-344), over the unified rows
// of rows.ts. Pure, typed arrays.
//
// A split row has a base cell and a head cell. An equal row is one
// split row with the same unified row on both sides. In a change
// block (a run of removed and added rows) the i-th removed row pairs
// with the i-th added one; the longer side's extra rows get an empty
// cell on the other side.
//
// shown stays over unified rows. Gaps hold equal rows only, so a gap
// of split rows is the same run of unified rows.
import type { DiffSessionSide } from "../sides";
import { deleteRow, equalRow, insertRow, type Rows } from "./rows";

// The two layouts of the diff view.
export type DiffLayout = "unified" | "split";

export interface Split {
  count: number;
  // The unified row in each cell; -1: empty.
  base: Int32Array;
  head: Int32Array;
  // The split row of each unified row.
  at: Uint32Array;
}

export function splitOf(rows: Rows): Split {
  const { kind, count } = rows;
  // At most one split row per unified row.
  const base = new Int32Array(count);
  const head = new Int32Array(count);
  const at = new Uint32Array(count);
  let d = 0;
  let i = 0;
  while (i < count) {
    if (kind[i] === equalRow) {
      base[d] = i;
      head[d] = i;
      at[i] = d;
      d++;
      i++;
      continue;
    }
    let j = i;
    while (j < count && kind[j] !== equalRow) j++;
    // Block [i, j): removed rows to base, added to head, in order.
    let b = d;
    let h = d;
    for (let r = i; r < j; r++) {
      if (kind[r] === deleteRow) {
        base[b] = r;
        at[r] = b++;
      } else {
        head[h] = r;
        at[r] = h++;
      }
    }
    const end = Math.max(b, h);
    base.fill(-1, b, end);
    head.fill(-1, h, end);
    d = end;
    i = j;
  }
  return { count: d, base: base.slice(0, d), head: head.slice(0, d), at };
}

// shown over split rows: a row is shown if a cell's row is. Changed
// rows always are, so this only matters for equal rows.
export function splitShown(split: Split, shown: Uint8Array): Uint8Array {
  const out = new Uint8Array(split.count);
  for (let d = 0; d < split.count; d++) {
    const b = split.base[d] ?? -1;
    const h = split.head[d] ?? -1;
    out[d] = (b >= 0 && shown[b] === 1) || (h >= 0 && shown[h] === 1) ? 1 : 0;
  }
  return out;
}

// The rows a grid shows, unified or split. Display rows are this
// space's rows; keys and the visible list of rows.ts work on them.
export interface RowSpace {
  split: boolean;
  count: number;
  // shown over unified rows to shown over display rows.
  shown: (unified: Uint8Array) => Uint8Array;
  // The unified row in a display row's cell on side; -1: empty. In
  // unified, a row has one cell: side doesn't matter.
  rowOf: (d: number, side: DiffSessionSide) => number;
  // The first and last unified rows of a display row. A gap's display
  // rows are equal rows: first is the unified row.
  first: (d: number) => number;
  last: (d: number) => number;
  // The display row of a unified row.
  displayOf: (r: number) => number;
}

export function unifiedSpace(rows: Rows): RowSpace {
  return {
    split: false,
    count: rows.count,
    shown: (u) => u,
    rowOf: (d) => d,
    first: (d) => d,
    last: (d) => d,
    displayOf: (r) => r,
  };
}

export function splitSpace(split: Split): RowSpace {
  const b = (d: number) => split.base[d] ?? -1;
  const h = (d: number) => split.head[d] ?? -1;
  // An empty cell (-1) counts for neither.
  const lo = (d: number) =>
    b(d) < 0 ? h(d) : h(d) < 0 ? b(d) : Math.min(b(d), h(d));
  return {
    split: true,
    count: split.count,
    shown: (u) => splitShown(split, u),
    rowOf: (d, side) => (side === "base" ? b(d) : h(d)),
    first: lo,
    last: (d) => Math.max(b(d), h(d)),
    displayOf: (r) => split.at[r] ?? 0,
  };
}

// The side of a unified row: removed rows base, the rest head. A
// display row's side (unified, and where a change lands in split) is
// its first row's.
export function sideOfRow(rows: Rows, r: number): DiffSessionSide {
  return rows.kind[r] === deleteRow ? "base" : "head";
}

// The line of unified row r on side; 0: none.
export function lineOn(rows: Rows, r: number, side: DiffSessionSide): number {
  return (side === "base" ? rows.base[r] : rows.head[r]) ?? 0;
}

// What a reader hears for a split cell's number (G23). An equal head
// cell says what the unified row says.
export function splitCellLabel(
  rows: Rows,
  r: number,
  side: DiffSessionSide,
): string {
  const n = lineOn(rows, r, side);
  switch (rows.kind[r]) {
    case insertRow:
      return `added line ${n}`;
    case deleteRow:
      return `removed line ${n}`;
    default:
      return side === "base"
        ? `base line ${n}, unchanged`
        : `line ${n}, unchanged`;
  }
}

export const noLine: Record<DiffSessionSide, string> = {
  base: "No base line",
  head: "No head line",
};
