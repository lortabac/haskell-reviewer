// A file's lines, by the server's rule (sourceLines in
// HaskellReviewer.Core.SourceText), so #L<n> and the server's line
// numbers agree. Lines split on "\n". A leading BOM isn't text. A "\r"
// right before a "\n" (or at the very end) isn't part of the line.
// Lone "\r", U+2028/9, NEL, VT and FF don't break. A final "\n" ends
// the last line, so there's no empty line after it, and "" has no
// lines. fixtures/lines/cases.json checks both sides.

export interface LineIndex {
  count: number;
  // UTF-16 offsets into the text. Line n (from 1) is
  // text.slice(starts[n - 1], ends[n - 1]), without its break.
  starts: Uint32Array;
  ends: Uint32Array;
  bom: boolean;
  // Display columns of the widest line: tabs go to the next stop of 8,
  // wide (CJK) characters count 2.
  maxCols: number;
}

export function lineIndex(text: string): LineIndex {
  const bom = text.charCodeAt(0) === 0xfeff;
  const from = bom ? 1 : 0;
  const starts: number[] = [];
  const ends: number[] = [];
  let maxCols = 0;
  let start = from;
  while (start < text.length) {
    const nl = text.indexOf("\n", start);
    const stop = nl === -1 ? text.length : nl;
    const end =
      stop > start && text.charCodeAt(stop - 1) === 13 ? stop - 1 : stop;
    starts.push(start);
    ends.push(end);
    maxCols = Math.max(maxCols, columns(text, start, end));
    if (nl === -1) break;
    start = nl + 1;
  }
  return {
    count: starts.length,
    starts: Uint32Array.from(starts),
    ends: Uint32Array.from(ends),
    bom,
    maxCols,
  };
}

// Line n, from 1, without its break. Out of range: "".
export function lineText(text: string, index: LineIndex, n: number): string {
  if (n < 1 || n > index.count) return "";
  return text.slice(index.starts[n - 1], index.ends[n - 1]);
}

// The offset in the text of column col (UTF-16, from 0) of line n.
// Both are clamped into the file.
export function offsetOf(index: LineIndex, n: number, col: number): number {
  if (index.count === 0) return index.bom ? 1 : 0;
  const i = Math.min(Math.max(n, 1), index.count) - 1;
  const start = index.starts[i] ?? 0;
  const end = index.ends[i] ?? 0;
  return Math.min(Math.max(start + col, start), end);
}

function columns(text: string, from: number, to: number): number {
  let col = 0;
  for (let i = from; i < to; i++) {
    const c = text.codePointAt(i) ?? 0;
    if (c > 0xffff) i++;
    if (c === 9) col = col - (col % 8) + 8;
    else col += wide(c) ? 2 : 1;
  }
  return col;
}

// East Asian wide and fullwidth, roughly. Only for the scroll width:
// a bit off is fine.
function wide(c: number): boolean {
  return (
    (c >= 0x1100 && c <= 0x115f) ||
    (c >= 0x2e80 && c <= 0xa4cf) ||
    (c >= 0xac00 && c <= 0xd7a3) ||
    (c >= 0xf900 && c <= 0xfaff) ||
    (c >= 0xfe30 && c <= 0xfe4f) ||
    (c >= 0xff00 && c <= 0xff60) ||
    (c >= 0xffe0 && c <= 0xffe6) ||
    (c >= 0x1f300 && c <= 0x1f64f) ||
    (c >= 0x1f900 && c <= 0x1f9ff) ||
    (c >= 0x20000 && c <= 0x3fffd)
  );
}
