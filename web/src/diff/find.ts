// Find in the diff view (T14a, D-346). Pure. find.ts searches each
// side's file; here the hits are put in one list in display order,
// with "virtual lines", one per cell, so find.ts Matches, inLine,
// nearest and step work as they are:
// - unified: row r is line r + 1. Its cell shows base's text on a
//   removed row, else head's (equal rows: once, on head);
// - split: row d's base cell is line 2d + 1, its head cell 2d + 2.
//   Both cells count, so an equal row can give two hits.
import type { DiffSessionSide } from "../sides";
import type { Finder, FindHits, Matches } from "../source/find";
import { equalRow, rowLabel, type Rows } from "./rows";
import { lineOn, sideOfRow, type Split, splitCellLabel } from "./split";

// Where the lines are counted: the rows, and their Split in the split
// layout (undefined: unified).
export interface CellSpace {
  rows: Rows;
  split: Split | undefined;
}

export interface DiffFinder extends Finder, CellSpace {}

// Hits and the finder that made them: a grid draws them only for its
// own rows and layout.
export interface DiffHits extends FindHits {
  finder: DiffFinder;
}

// The finder counts lines as this view does: same rows, same layout.
export function isFor(f: CellSpace, view: CellSpace): boolean {
  return f.rows === view.rows && f.split === view.split;
}

// A cell's line, for a unified row on a side. -1: no such cell (no
// line on that side; unified: the base side of an equal row).
export function cellLine(
  f: CellSpace,
  row: number,
  side: DiffSessionSide,
): number {
  if (lineOn(f.rows, row, side) === 0) return -1;
  if (f.split === undefined) {
    if (side === "base" && f.rows.kind[row] === equalRow) return -1;
    return row + 1;
  }
  return 2 * (f.split.at[row] ?? 0) + (side === "head" ? 1 : 0) + 1;
}

// The cell of a line: its unified row and side. row -1: an empty cell.
export function cellAt(
  f: CellSpace,
  line: number,
): { row: number; side: DiffSessionSide } {
  if (f.split === undefined) {
    const row = line - 1;
    return { row, side: sideOfRow(f.rows, row) };
  }
  const d = (line - 1) >> 1;
  const side: DiffSessionSide = (line - 1) % 2 === 1 ? "head" : "base";
  const row = (side === "base" ? f.split.base[d] : f.split.head[d]) ?? -1;
  return { row, side };
}

// base and head: one finder per side's file (createFinder). split:
// the rows' Split in the split layout, else undefined.
export function diffFinder(
  rows: Rows,
  base: Finder,
  head: Finder,
  split: Split | undefined,
): DiffFinder {
  const f = { rows, split };
  // A side's hits on their cells' lines. Lines go up with the rows, so
  // the list stays in order.
  const mapped = (m: Matches, side: DiffSessionSide): Matches => {
    const map = side === "base" ? rows.baseRow : rows.headRow;
    const line = new Uint32Array(m.count);
    const from = new Uint32Array(m.count);
    const to = new Uint32Array(m.count);
    let n = 0;
    for (let i = 0; i < m.count; i++) {
      const v = cellLine(f, map[m.line[i]!] ?? 0, side);
      if (v < 0) continue;
      line[n] = v;
      from[n] = m.from[i]!;
      to[n] = m.to[i]!;
      n++;
    }
    return {
      count: n,
      line: line.subarray(0, n),
      from: from.subarray(0, n),
      to: to.subarray(0, n),
    };
  };
  const search = (q: string): Matches => {
    const a = mapped(base.search(q), "base");
    const b = mapped(head.search(q), "head");
    // Merge: a cell is on one side, so equal lines come from one list.
    const count = a.count + b.count;
    const line = new Uint32Array(count);
    const from = new Uint32Array(count);
    const to = new Uint32Array(count);
    let i = 0;
    let j = 0;
    for (let k = 0; k < count; k++) {
      const takeA = j >= b.count || (i < a.count && a.line[i]! < b.line[j]!);
      const s = takeA ? a : b;
      const x = takeA ? i++ : j++;
      line[k] = s.line[x]!;
      from[k] = s.from[x]!;
      to[k] = s.to[x]!;
    }
    return { count, line, from, to };
  };
  return { search, rows, split };
}

// For the live region: "Match 3 of 12, removed line 40".
export function diffStatus(f: DiffFinder, m: Matches, i: number): string {
  if (m.count === 0 || i < 0) return "No matches";
  const { row, side } = cellAt(f, m.line[i]!);
  const where =
    f.split === undefined
      ? rowLabel(f.rows, row)
      : splitCellLabel(f.rows, row, side);
  return `Match ${i + 1} of ${m.count}, ${where}`;
}
