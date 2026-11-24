// The rows of a unified diff, from api/diff's runs. Pure, typed
// arrays: a file can have 100k rows. See D-341.
//
// Row: one line of the diff (equal, removed or added), by index from 0
// in the full list. Shown: a bitmap over the rows; changed rows are
// always shown. Visible: the shown rows plus one gap row for each run
// of hidden ones, in order.
import type { DiffRun } from "../api/diff";

export const equalRow = 0;
export const deleteRow = 1;
export const insertRow = 2;

export interface Rows {
  count: number;
  // equalRow, deleteRow or insertRow.
  kind: Uint8Array;
  // The row's line on each side, from 1; 0: none.
  base: Uint32Array;
  head: Uint32Array;
  baseCount: number;
  headCount: number;
  // The row of each line, by line (index 0 unused).
  baseRow: Uint32Array;
  headRow: Uint32Array;
}

export function rowsOf(runs: readonly DiffRun[]): Rows {
  let count = 0;
  let baseCount = 0;
  let headCount = 0;
  for (const [op, n] of runs) {
    count += n;
    if (op !== "insert") baseCount += n;
    if (op !== "delete") headCount += n;
  }
  const kind = new Uint8Array(count);
  const base = new Uint32Array(count);
  const head = new Uint32Array(count);
  const baseRow = new Uint32Array(baseCount + 1);
  const headRow = new Uint32Array(headCount + 1);
  let r = 0;
  let b = 0;
  let h = 0;
  for (const [op, n] of runs) {
    const k =
      op === "equal" ? equalRow : op === "delete" ? deleteRow : insertRow;
    for (let i = 0; i < n; i++, r++) {
      kind[r] = k;
      if (k !== insertRow) {
        base[r] = ++b;
        baseRow[b] = r;
      }
      if (k !== deleteRow) {
        head[r] = ++h;
        headRow[h] = r;
      }
    }
  }
  return { count, kind, base, head, baseCount, headCount, baseRow, headRow };
}

// The runs fit the texts: equal + delete is base's line count, equal +
// insert head's.
export function countsMatch(
  rows: Rows,
  baseLines: number,
  headLines: number,
): boolean {
  return rows.baseCount === baseLines && rows.headCount === headLines;
}

// Context lines at each change (G5, git's -U3).
export const context = 3;

// The default: changed rows, and up to context equal rows on each side
// of a change. A file with no change shows nothing (all in one gap).
export function defaultShown(rows: Rows, ctx = context): Uint8Array {
  const shown = new Uint8Array(rows.count);
  const { kind, count } = rows;
  let i = 0;
  while (i < count) {
    if (kind[i] !== equalRow) {
      shown[i] = 1;
      i++;
      continue;
    }
    let j = i;
    while (j < count && kind[j] === equalRow) j++;
    // Equal run [i, j). After a change: its first ctx; before one: its
    // last ctx.
    if (i > 0) shown.fill(1, i, Math.min(j, i + ctx));
    if (j < count) shown.fill(1, Math.max(i, j - ctx), j);
    i = j;
  }
  return shown;
}

export function allShown(rows: Rows): Uint8Array {
  return new Uint8Array(rows.count).fill(1);
}

// The shown and gap rows, in order. Item i covers rows start[i] up to
// start[i] + len[i]; len is 1 for a line row.
export interface Visible {
  length: number;
  start: Uint32Array;
  len: Uint32Array;
  gap: Uint8Array;
}

export function visibleOf(shown: Uint8Array): Visible {
  const n = shown.length;
  let items = 0;
  for (let i = 0; i < n; i++) {
    if (shown[i] === 1 || i === 0 || shown[i - 1] === 1) items++;
  }
  const start = new Uint32Array(items);
  const len = new Uint32Array(items);
  const gap = new Uint8Array(items);
  let k = 0;
  let i = 0;
  while (i < n) {
    if (shown[i] === 1) {
      start[k] = i;
      len[k] = 1;
      k++;
      i++;
      continue;
    }
    let j = i;
    while (j < n && shown[j] !== 1) j++;
    start[k] = i;
    len[k] = j - i;
    gap[k] = 1;
    k++;
    i = j;
  }
  return { length: items, start, len, gap };
}

// An item's key: its row for a line row, -(1 + its first row) for a
// gap. A line row keeps its key when a gap opens. Only this module
// knows the encoding.
export function gapKey(start: number): number {
  return -(1 + start);
}

export function isGapKey(key: number): boolean {
  return key < 0;
}

export function keyAt(v: Visible, i: number): number {
  const s = v.start[i] ?? 0;
  return v.gap[i] === 1 ? gapKey(s) : s;
}

// The first row of a key.
export function rowOfKey(key: number): number {
  return isGapKey(key) ? -(key + 1) : key;
}

// The item that has row r: its line row, or the gap it is hidden in.
// -1 when there are no items.
export function indexOfRow(v: Visible, r: number): number {
  let lo = 0;
  let hi = v.length - 1;
  if (hi < 0) return -1;
  // The last item with start <= r.
  while (lo < hi) {
    const mid = (lo + hi + 1) >> 1;
    if ((v.start[mid] ?? 0) <= r) lo = mid;
    else hi = mid - 1;
  }
  return lo;
}

// The item of a key, exactly; -1 if it isn't there.
export function indexOfKey(v: Visible, key: number): number {
  const i = indexOfRow(v, rowOfKey(key));
  return i >= 0 && keyAt(v, i) === key ? i : -1;
}

// Lines added by one click on a gap.
export const step = 20;

// How a gap opens: down shows its top (below the rows above it), up
// its bottom, all the whole gap.
export type Expand = "down" | "up" | "all";

// A copy of shown with part of the gap [start, start + len) shown.
export function expand(
  shown: Uint8Array,
  start: number,
  len: number,
  how: Expand,
  by = step,
): Uint8Array {
  const out = shown.slice();
  const end = start + len;
  if (how === "all" || len <= by) out.fill(1, start, end);
  else if (how === "down") out.fill(1, start, start + by);
  else out.fill(1, end - by, end);
  return out;
}

// A copy of shown with the rows around r shown (a hash on a hidden
// line).
export function reveal(
  shown: Uint8Array,
  r: number,
  around = context,
): Uint8Array {
  const out = shown.slice();
  out.fill(1, Math.max(0, r - around), Math.min(shown.length, r + around + 1));
  return out;
}

// Which buttons a gap has. One button ("all") when it is step lines
// or fewer. down needs rows above, up rows below; a gap with neither
// (an unchanged file) gets down.
export function gapButtons(rows: Rows, start: number, len: number): Expand[] {
  if (len <= step) return ["all"];
  const above = start > 0;
  const below = start + len < rows.count;
  const out: Expand[] = [];
  if (above || !below) out.push("down");
  if (below) out.push("up");
  out.push("all");
  return out;
}

// What a reader hears for a row (G23).
export function rowLabel(rows: Rows, r: number): string {
  const b = rows.base[r] ?? 0;
  const h = rows.head[r] ?? 0;
  switch (rows.kind[r]) {
    case insertRow:
      return `added line ${h}`;
    case deleteRow:
      return `removed line ${b}`;
    default:
      return b === h
        ? `line ${h}, unchanged`
        : `line ${h}, unchanged, base line ${b}`;
  }
}

// Gaps hold equal rows only: their head lines.
function gapLines(rows: Rows, start: number, len: number): [number, number] {
  return [rows.head[start] ?? 0, rows.head[start + len - 1] ?? 0];
}

function num(n: number): string {
  return n.toLocaleString("en-US");
}

function lines(n: number): string {
  return n === 1 ? "1 line" : `${num(n)} lines`;
}

export function gapLabel(rows: Rows, start: number, len: number): string {
  const [a, b] = gapLines(rows, start, len);
  return a === b
    ? `${lines(len)} hidden, ${a}`
    : `${lines(len)} hidden, ${a} to ${b}`;
}

// A gap button: its short text and its name. The name starts with
// the text (WCAG 2.5.3) and says the lines it shows. down and up get
// an arrow from CSS.
export function buttonText(
  rows: Rows,
  start: number,
  len: number,
  how: Expand,
): { text: string; name: string } {
  const [a, b] = gapLines(rows, start, len);
  const range = (x: number, y: number) => (x === y ? `${x}` : `${x} to ${y}`);
  if (len <= step) {
    return { text: `Show ${len}`, name: `Show ${lines(len)}, ${range(a, b)}` };
  }
  if (how === "all") {
    return {
      text: `All ${num(len)}`,
      name: `All ${lines(len)}, ${range(a, b)}`,
    };
  }
  const [x, y] = how === "down" ? [a, a + step - 1] : [b - step + 1, b];
  return { text: `${step}`, name: `${step} lines, ${range(x, y)}` };
}

// The change blocks (runs of removed and added rows), in order, as
// [first row, row after the last].
export function blocks(rows: Rows): [number, number][] {
  const out: [number, number][] = [];
  const { kind, count } = rows;
  let i = 0;
  while (i < count) {
    if (kind[i] === equalRow) {
      i++;
      continue;
    }
    let j = i;
    while (j < count && kind[j] !== equalRow) j++;
    out.push([i, j]);
    i = j;
  }
  return out;
}

// The first row of each change block, in order.
export function blockStarts(rows: Rows): number[] {
  return blocks(rows).map(([s]) => s);
}

// The first changed row of each hunk: blocks joined when at most
// 2 × ctx equal rows lie between them (git's -U3), whatever is open.
export function hunkStarts(rows: Rows, ctx = context): number[] {
  const out: number[] = [];
  const { kind, count } = rows;
  let lastChange = -1;
  for (let i = 0; i < count; i++) {
    if (kind[i] === equalRow) continue;
    if (lastChange < 0 || i - lastChange - 1 > 2 * ctx) out.push(i);
    lastChange = i;
  }
  return out;
}

// The next start after row r (dir 1), or the last one before it
// (dir -1). undefined: none that way.
export function stepTo(
  starts: readonly number[],
  r: number,
  dir: 1 | -1,
): number | undefined {
  if (dir === 1) return starts.find((s) => s > r);
  // No findLast in ES2022.
  return [...starts].reverse().find((s) => s < r);
}
