// Copy for the virtual source view (SPEC §5.7). Only the rows on
// screen are in the DOM, so the copied text comes from the file: each
// end of the selection is mapped to a line and column, then the file's
// text between them is copied. Line numbers and page chrome never get
// in.
import { type LineIndex, offsetOf } from "./lines";

// Where a selection end is: before the view, after it, or at a column
// (UTF-16, from 0) of a line (from 1). A column past the line's end
// means its end.
export type Point =
  | { kind: "before" }
  | { kind: "after" }
  | { kind: "at"; line: number; col: number };

const before: Point = { kind: "before" };
const after: Point = { kind: "after" };

// The DOM shape this reads (CodeView.tsx): view > .code-rows >
// .code-row[data-line] > (.code-num, .code-cell). Rows are in line
// order.
export function domPoint(view: Element, node: Node, offset: number): Point {
  if (!view.contains(node)) {
    // Compare with the point just before the view.
    const r = document.createRange();
    r.setStartBefore(view);
    r.collapse(true);
    return r.comparePoint(node, offset) <= 0 ? before : after;
  }
  if (node === view) return offset === 0 ? before : after;
  const el = node instanceof Element ? node : node.parentElement;
  const cell = el?.closest(".code-cell");
  if (cell !== null && cell !== undefined) {
    return { kind: "at", line: lineOf(cell), col: colIn(cell, node, offset) };
  }
  if (el?.classList.contains("code-rows") === true) {
    const rows = el.children;
    const row = rows[offset];
    if (row !== undefined) return { kind: "at", line: lineOf(row), col: 0 };
    const last = rows[rows.length - 1];
    return last === undefined ? before : endOf(lineOf(last));
  }
  const row = el?.closest(".code-row");
  if (row !== null && row !== undefined) {
    // At the row's end: the line's end. In the gutter or between the
    // cells: its start.
    if (node === row && offset >= row.childNodes.length) {
      return endOf(lineOf(row));
    }
    return { kind: "at", line: lineOf(row), col: 0 };
  }
  return before;
}

// The column (UTF-16) of a point inside a cell: the length of the text
// before it.
export function colIn(cell: Node, node: Node, offset: number): number {
  const r = document.createRange();
  r.setStart(cell, 0);
  r.setEnd(node, offset);
  return r.toString().length;
}

// nodeAt for many columns at once, sorted up: one walk of the text.
export function nodesAt(cell: Node, cols: readonly number[]): [Node, number][] {
  const out: [Node, number][] = [];
  const w = document.createTreeWalker(cell, NodeFilter.SHOW_TEXT);
  let t = w.nextNode() as Text | null;
  let start = 0;
  let last: Text | undefined;
  for (const col of cols) {
    // At a border between two nodes: the end of the first.
    while (t !== null && col > start + t.length) {
      start += t.length;
      last = t;
      t = w.nextNode() as Text | null;
    }
    if (t !== null) out.push([t, Math.max(0, col - start)]);
    else if (last !== undefined) out.push([last, last.length]);
    else out.push([cell, cell.childNodes.length]);
  }
  return out;
}

// The other way: the text node and offset at col in cell. At a border
// between two nodes, the end of the first. Past the end: the end.
export function nodeAt(cell: Node, col: number): [Node, number] {
  const w = document.createTreeWalker(cell, NodeFilter.SHOW_TEXT);
  let left = Math.max(0, col);
  let last: Text | undefined;
  for (let t = w.nextNode(); t !== null; t = w.nextNode()) {
    const len = (t as Text).length;
    if (left <= len) return [t, left];
    left -= len;
    last = t as Text;
  }
  return last === undefined
    ? [cell, cell.childNodes.length]
    : [last, last.length];
}

function endOf(line: number): Point {
  return { kind: "at", line, col: Infinity };
}

function lineOf(el: Element): number {
  return Number(el.closest(".code-row")?.getAttribute("data-line") ?? 1);
}

// The file's text between two points, in either order. "before" is the
// file's start (after a BOM), "after" its end. CRLF is kept. A point
// inside a surrogate pair takes the whole character.
export function textBetween(
  text: string,
  index: LineIndex,
  a: Point,
  b: Point,
): string {
  const x = offset(text, index, a);
  const y = offset(text, index, b);
  let from = Math.min(x, y);
  let to = Math.max(x, y);
  if (from > 0 && low(text, from) && high(text, from - 1)) from--;
  if (to < text.length && low(text, to) && high(text, to - 1)) to++;
  return text.slice(from, to);
}

function offset(text: string, index: LineIndex, p: Point): number {
  switch (p.kind) {
    case "before":
      return index.bom ? 1 : 0;
    case "after":
      return text.length;
    case "at":
      return offsetOf(index, p.line, p.col);
  }
}

function high(text: string, i: number): boolean {
  const c = text.charCodeAt(i);
  return c >= 0xd800 && c <= 0xdbff;
}

function low(text: string, i: number): boolean {
  const c = text.charCodeAt(i);
  return c >= 0xdc00 && c <= 0xdfff;
}

// What a copy of the selection should give, or undefined to leave the
// copy to the browser (no range meets the view, or all are empty). A
// range from before the view to after it gives the whole file. Ranges
// (Firefox can have several) are joined with "\n"; one that doesn't
// meet the view gives its own text.
export function copiedText(
  view: Element,
  text: string,
  index: LineIndex,
  sel: Selection,
): string | undefined {
  const parts: string[] = [];
  let ours = false;
  for (let i = 0; i < sel.rangeCount; i++) {
    const r = sel.getRangeAt(i);
    const a = domPoint(view, r.startContainer, r.startOffset);
    const b = domPoint(view, r.endContainer, r.endOffset);
    if (a.kind === "at" || b.kind === "at" || a.kind !== b.kind) {
      ours = true;
      parts.push(textBetween(text, index, a, b));
    } else {
      parts.push(r.toString());
    }
  }
  if (!ours || parts.every((p) => p === "")) return undefined;
  return parts.join("\n");
}

// The lines of the selection's anchor and focus that are in the view.
// CodeView keeps them mounted: an end in a removed row would move.
export function selectionLines(view: Element, sel: Selection | null): number[] {
  if (sel === null || sel.anchorNode === null || sel.focusNode === null) {
    return [];
  }
  const out: number[] = [];
  for (const [node, off] of [
    [sel.anchorNode, sel.anchorOffset],
    [sel.focusNode, sel.focusOffset],
  ] as const) {
    if (!view.contains(node)) continue;
    const p = domPoint(view, node, off);
    if (p.kind === "at") out.push(p.line);
  }
  return out;
}

// The largest offset in a node.
export function size(n: Node): number {
  return n instanceof CharacterData ? n.length : n.childNodes.length;
}

// Swap a cell's children (a new highlight) and keep the selection.
// Ends inside the cell go back to the same columns. Ends elsewhere go
// back as they were: WebKit moves them too (select-all's ends on the
// rowgroup become the cell's), and copy.ts tells select-all by them.
export function replaceKeepingSelection(cell: Element, nodes: Node[]): void {
  const sel = getSelection();
  const a = sel?.anchorNode ?? null;
  const f = sel?.focusNode ?? null;
  if (sel === null || a === null || f === null) {
    cell.replaceChildren(...nodes);
    return;
  }
  const ao = sel.anchorOffset;
  const fo = sel.focusOffset;
  const aCol = cell.contains(a) ? colIn(cell, a, ao) : undefined;
  const fCol = cell.contains(f) ? colIn(cell, f, fo) : undefined;
  cell.replaceChildren(...nodes);
  const [an, ano] = aCol === undefined ? [a, ao] : nodeAt(cell, aCol);
  const [fn, fno] = fCol === undefined ? [f, fo] : nodeAt(cell, fCol);
  if (
    sel.anchorNode === an &&
    sel.anchorOffset === ano &&
    sel.focusNode === fn &&
    sel.focusOffset === fno
  ) {
    return;
  }
  if (!an.isConnected || !fn.isConnected) return;
  sel.setBaseAndExtent(
    an,
    Math.min(ano, size(an)),
    fn,
    Math.min(fno, size(fn)),
  );
}

// The text point under (x, y) in the window, if any.
export function pointAt(x: number, y: number): [Node, number] | undefined {
  if (typeof document.caretPositionFromPoint === "function") {
    const p = document.caretPositionFromPoint(x, y);
    return p === null ? undefined : [p.offsetNode, p.offset];
  }
  // WebKit before 18.2.
  const d = document as Document & {
    caretRangeFromPoint?: (x: number, y: number) => Range | null;
  };
  const r = d.caretRangeFromPoint?.(x, y) ?? null;
  return r === null ? undefined : [r.startContainer, r.startOffset];
}
