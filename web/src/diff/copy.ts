// Copy in the diff grid (SPEC §4.5, D-347). Only rows near the screen
// are in the DOM, so the text comes from the two files, never the
// DOM: each end of the selection maps to a row and a column, then the
// text between them is cut from the files (source/selection.ts
// textBetween: CRLF and surrogate pairs kept, no BOM). Signs, numbers
// and gap words never get in. Hidden gap lines do.
//
// Unified: each row's own text (removed rows base, the rest head).
// Split: only the anchor's side.
import { type DiffSessionSide, otherSide } from "../sides";
import { type LineIndex, wholeText } from "../source/lines";
import {
  colIn,
  copiedWith,
  outside,
  type Point,
  textBetween,
} from "../source/selection";
import type { CellSpace } from "./find";
import { indexOfKey, isGapKey, rowOfKey, type Visible } from "./rows";
import { lineOn, sideOfRow, type Split } from "./split";

// One side's text and its lines.
export interface SideText {
  text: string;
  index: LineIndex;
}

// Where a selection end is: before the grid, after it, or in an item
// (key: rows.ts). side: the cell's or the half's (a split row's start
// is base's, its end head's); undefined in a gap or outside a unified
// row's cell. col: UTF-16 from 0; Infinity: the line's end. With no
// side, col 0 is the row's start and Infinity its end. whole: inside
// a gap row's cell (the whole gap).
export type DiffPoint =
  | { kind: "before" }
  | { kind: "after" }
  | {
      kind: "at";
      key: number;
      side: DiffSessionSide | undefined;
      col: number;
      whole?: boolean;
    };

const before: DiffPoint = { kind: "before" };
const after: DiffPoint = { kind: "after" };

// What copy needs to know about the grid: its rows and layout, ...
export interface CopyModel extends CellSpace {
  base: SideText;
  head: SideText;
  // The visible items (a gap's length).
  vis: Visible;
}

// The DOM shape this reads (DiffGrid.tsx): view > .code-rows >
// .code-row[data-key] > unified: .code-num, .diff-sign, .code-cell;
// split: .diff-split > .diff-half.diff-<side> > .code-num, .diff-clip >
// .code-cell; gap: .diff-gap-cell. Rows are in display order.
export function diffPoint(
  view: Element,
  node: Node,
  offset: number,
): DiffPoint {
  const out = outside(view, node, offset);
  if (out !== undefined) return out === "before" ? before : after;
  const el = node instanceof Element ? node : node.parentElement;
  if (el === null) return before;
  if (el.classList.contains("code-rows")) {
    const row = el.children[offset];
    if (row !== undefined) return rowEdge(row, false);
    const last = el.children[el.children.length - 1];
    return last === undefined ? before : rowEdge(last, true);
  }
  const row = el.closest(".code-row");
  if (row === null) return before;
  const cell = el.closest(".code-cell");
  if (cell !== null) {
    if (cell.classList.contains("diff-gap-cell")) {
      return { ...at(row, undefined, 0), whole: true };
    }
    return at(row, sideOf(cell), colIn(cell, node, offset));
  }
  // Between the halves: offset 0 is base's start, 1 head's.
  if (node === el && el.classList.contains("diff-split")) {
    if (offset >= el.childNodes.length) return at(row, "head", Infinity);
    return at(row, offset === 0 ? "base" : "head", 0);
  }
  // Elsewhere in a half or a unified row (a gutter, a box around the
  // cell): the line's start if before its cell, else its end. A split
  // or gap row's own ends: the row's start or end.
  const half = el.closest(".diff-half");
  const box = half ?? row;
  const own = box.querySelector(".code-cell");
  if (half === null && (own === null || own.closest(".diff-half") !== null)) {
    return rowEdge(row, offset > 0);
  }
  const side = half === null ? undefined : halfSide(half);
  return at(
    row,
    side,
    own === null || beforeNode(node, offset, own) ? 0 : Infinity,
  );
}

// A row's start or end. Split: base's start, head's end; a gap or a
// unified row: no side.
function rowEdge(row: Element, end: boolean): DiffPoint {
  if (row.firstElementChild?.classList.contains("diff-split") !== true) {
    return at(row, undefined, end ? Infinity : 0);
  }
  return end ? at(row, "head", Infinity) : at(row, "base", 0);
}

// The point is before node n (or right at its start).
function beforeNode(node: Node, offset: number, n: Node): boolean {
  const r = document.createRange();
  r.setStartBefore(n);
  r.collapse(true);
  return r.comparePoint(node, offset) <= 0;
}

function at(
  row: Element,
  side: DiffSessionSide | undefined,
  col: number,
): Extract<DiffPoint, { kind: "at" }> {
  return { kind: "at", key: Number(row.getAttribute("data-key")), side, col };
}

// A code cell's side (data-side); undefined: a gap cell.
export function sideOf(cell: Element): DiffSessionSide | undefined {
  const s = cell.getAttribute("data-side");
  return s === "base" || s === "head" ? s : undefined;
}

// The side of a split half (.diff-half).
export function halfSide(half: Element): DiffSessionSide {
  return half.classList.contains("diff-base") ? "base" : "head";
}

// The side of the code cell a node is in, if any (not a gap's).
export function cellSideAt(
  view: Element,
  node: Node,
): DiffSessionSide | undefined {
  const el = node instanceof Element ? node : node.parentElement;
  const cell = el?.closest(".code-cell");
  return cell == null || !view.contains(cell) ? undefined : sideOf(cell);
}

// The items of the selection's anchor and focus that are in the view.
// The grid keeps them mounted: an end in a row that goes would move.
export function selectionKeys(view: Element, sel: Selection | null): number[] {
  if (sel === null || sel.anchorNode === null || sel.focusNode === null) {
    return [];
  }
  const out: number[] = [];
  for (const [node, off] of [
    [sel.anchorNode, sel.anchorOffset],
    [sel.focusNode, sel.focusOffset],
  ] as const) {
    if (!view.contains(node)) continue;
    const p = diffPoint(view, node, off);
    if (p.kind === "at") out.push(p.key);
  }
  return out;
}

// The display rows of an item: one, or a gap's.
function span(m: CopyModel, key: number): [number, number] {
  const d = rowOfKey(key);
  if (!isGapKey(key)) return [d, d];
  const i = indexOfKey(m.vis, key);
  const len = i < 0 ? 1 : (m.vis.len[i] ?? 1);
  return [d, d + len - 1];
}

const textOf = (m: CopyModel, side: DiffSessionSide) =>
  side === "base" ? m.base : m.head;

// The break after line n, as in the file: \r\n, \n, "" at a file's
// end with none (or a lone \r there). Exported for tests.
export function breakAfter(t: SideText, n: number): string {
  const end = t.index.ends[n - 1] ?? t.text.length;
  const next = n < t.index.count ? (t.index.starts[n] ?? end) : t.text.length;
  return t.text.slice(end, next);
}

// A place in unified rows; "end": past the last row's break.
type Place = { row: number; col: number } | "end";

function unifiedPlace(m: CopyModel, p: DiffPoint, later: boolean): Place {
  if (p.kind === "before") return { row: 0, col: 0 };
  if (p.kind === "after") return "end";
  const [lo, hi] = span(m, p.key);
  if (!isGapKey(p.key)) return { row: lo, col: p.col };
  // Inside a gap: all of it. At its row's start or end: that point.
  if (p.whole === true) {
    return later ? { row: hi, col: Infinity } : { row: lo, col: 0 };
  }
  return p.col === 0 ? { row: lo, col: 0 } : { row: hi, col: Infinity };
}

// Unified rows from a to b, each in its own file. A run of rows on one
// side is a run of that file's lines: one cut each, joined by the
// break after the run's last line ("\n" if it has none).
function unifiedText(m: CopyModel, a: Place, b: Place): string {
  const { rows } = m;
  if (rows.count === 0 || a === "end") return "";
  const last = b === "end" ? rows.count - 1 : b.row;
  const parts: string[] = [];
  for (let r = a.row; r <= last;) {
    const side = sideOfRow(rows, r);
    let e = r;
    while (e < last && sideOfRow(rows, e + 1) === side) e++;
    const t = textOf(m, side);
    const n = lineOn(rows, e, side);
    const from: Point = {
      kind: "at",
      line: lineOn(rows, r, side),
      col: r === a.row ? a.col : 0,
    };
    const endCol = e === last && b !== "end" ? b.col : Infinity;
    parts.push(
      textBetween(t.text, t.index, from, { kind: "at", line: n, col: endCol }),
    );
    if (e < last) {
      const br = breakAfter(t, n);
      parts.push(br === "\n" || br === "\r\n" ? br : "\n");
    } else if (b === "end") parts.push(breakAfter(t, n));
    r = e + 1;
  }
  return parts.join("");
}

// The line of side in display row d of a split; 0: an empty cell.
function splitLine(
  m: CopyModel,
  s: Split,
  d: number,
  side: DiffSessionSide,
): number {
  const r = (side === "base" ? s.base[d] : s.head[d]) ?? -1;
  return r < 0 ? 0 : lineOn(m.rows, r, side);
}

// A split end on side, as a line and column: in side's cell (or its
// half: the line's start or end), its column. Else it snaps to side:
// an earlier end takes the next line of side after the point, at its
// start; a later end the last one before it, at its end. In row d, DOM
// order is base's half, then head's; a gap row holds all its lines,
// and its own start or end is before or after all of them.
// undefined: no line that way.
function splitPlace(
  m: CopyModel,
  s: Split,
  p: DiffPoint,
  side: DiffSessionSide,
  later: boolean,
): { line: number; col: number } | undefined {
  const count = textOf(m, side).index.count;
  if (count === 0) return undefined;
  if (p.kind === "before") return later ? undefined : { line: 1, col: 0 };
  if (p.kind === "after")
    return later ? { line: count, col: Infinity } : undefined;
  // The rows to look in.
  let [lo, hi] = span(m, p.key);
  if (isGapKey(p.key)) {
    if (p.whole !== true) {
      if (p.col === 0) hi = lo - 1;
      else lo = hi + 1;
    }
  } else if (p.side === side) {
    const n = splitLine(m, s, lo, side);
    if (n > 0) return { line: n, col: p.col };
  } else {
    // The other half: after base's, before head's.
    const after = p.side === undefined ? p.col > 0 : p.side === "head";
    if (after) lo++;
    else hi--;
  }
  if (!later) {
    for (let d = lo; d < s.count; d++) {
      const n = splitLine(m, s, d, side);
      if (n > 0) return { line: n, col: 0 };
    }
    return undefined;
  }
  for (let d = hi; d >= 0; d--) {
    const n = splitLine(m, s, d, side);
    if (n > 0) return { line: n, col: Infinity };
  }
  return undefined;
}

function splitText(
  m: CopyModel,
  s: Split,
  a: DiffPoint,
  b: DiffPoint,
  side: DiffSessionSide,
): string {
  const x = splitPlace(m, s, a, side, false);
  const y = splitPlace(m, s, b, side, true);
  if (x === undefined || y === undefined) return "";
  if (x.line > y.line || (x.line === y.line && x.col > y.col)) return "";
  const t = textOf(m, side);
  // after: the file's end, with its last break.
  const to: Point =
    b.kind === "after" ? { kind: "after" } : { kind: "at", ...y };
  const from: Point =
    a.kind === "before" ? { kind: "before" } : { kind: "at", ...x };
  return textBetween(t.text, t.index, from, to);
}

// The text from a to b (a first). side: the split side to copy.
export function diffCopyText(
  m: CopyModel,
  a: DiffPoint,
  b: DiffPoint,
  side: DiffSessionSide,
): string {
  if (m.split !== undefined) return splitText(m, m.split, a, b, side);
  return unifiedText(m, unifiedPlace(m, a, false), unifiedPlace(m, b, true));
}

// Select-all: every row (unified), or side's whole file (split). No
// BOM.
export function diffAllText(m: CopyModel, side: DiffSessionSide): string {
  if (m.split !== undefined) {
    const t = textOf(m, side);
    return wholeText(t.text, t.index);
  }
  return unifiedText(m, { row: 0, col: 0 }, "end");
}

// What a copy of the selection gives, or undefined to leave it to the
// browser (no range meets the grid, or all are empty). Ranges (Firefox
// can have several) are joined with "\n"; one that doesn't meet the
// grid gives its own text. side: as diffCopyText.
export function diffCopied(
  view: Element,
  m: CopyModel,
  sel: Selection,
  side: DiffSessionSide,
): string | undefined {
  return copiedWith(
    sel,
    (n, o) => diffPoint(view, n, o),
    (a, b) => diffCopyText(m, a, b, side),
  );
}

// Lines from and to (both in) on one side.
export interface LineRange {
  side: DiffSessionSide;
  from: number;
  to: number;
}

// The lines a selection from a to b covers, for Copy location. Split:
// side's lines (ends snapped as for copy). Unified: the lines on side
// of the rows in the range, else the other side's. An end at the
// start of a later line doesn't count that line (D-299). undefined:
// none.
export function selectedRange(
  m: CopyModel,
  a: DiffPoint,
  b: DiffPoint,
  side: DiffSessionSide,
): LineRange | undefined {
  if (a.kind !== "at" && a.kind === b.kind) return undefined;
  if (m.split !== undefined) {
    const x = splitPlace(m, m.split, a, side, false);
    const y = splitPlace(m, m.split, b, side, true);
    if (x === undefined || y === undefined || x.line > y.line) return undefined;
    const to = y.col === 0 && y.line > x.line ? y.line - 1 : y.line;
    return { side, from: x.line, to };
  }
  const { rows } = m;
  if (rows.count === 0) return undefined;
  const x = unifiedPlace(m, a, false);
  const y = unifiedPlace(m, b, true);
  if (x === "end") return undefined;
  let hi = y === "end" ? rows.count - 1 : y.row;
  if (y !== "end" && y.col === 0 && hi > x.row) hi--;
  for (const s of [side, otherSide(side)]) {
    let from = 0;
    let to = 0;
    for (let r = x.row; r <= hi; r++) {
      const n = lineOn(rows, r, s);
      if (n === 0) continue;
      if (from === 0) from = n;
      to = n;
    }
    if (from > 0) return { side: s, from, to };
  }
  return undefined;
}
