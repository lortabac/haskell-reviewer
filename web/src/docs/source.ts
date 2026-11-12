// A declaration's source (SPEC §4.3), as rows to show: its parts in
// source order, joined, with line numbers, and "⋯" rows for the lines
// left out. Plain text: .lhs ">" and CPP lines as written.
import type { SourcePart, Span } from "../api/module";
import type { Seg } from "../source/highlight";

// A part with its text (shared ones resolved).
export interface TextPart {
  span: Span;
  text: string;
}

// Which part of a row's text is which part of the file: from code point
// `at` of the row (from 0), `len` code points, they are line `line` of
// the file from column `col` (from 1). Pad, " … " and text after an
// overlap are in no span.
export interface RowSpan {
  at: number;
  line: number;
  col: number;
  len: number;
}

export type SourceRow =
  | { kind: "line"; line: number; text: string; spans: RowSpan[] }
  // Lines from..to (both included) are left out.
  | { kind: "gap"; from: number; to: number };

// Resolve shared parts with resolve (PageIndex.sentPart). A part it
// can't find is dropped: the page is broken, show the rest.
export function textParts(
  parts: SourcePart[],
  resolve: (p: {
    span: Span;
    sharedWith: number;
  }) => { text: string } | undefined,
): TextPart[] {
  const out: TextPart[] = [];
  for (const p of parts) {
    const text = "text" in p ? p.text : resolve(p)?.text;
    if (text !== undefined) out.push({ span: p.span, text });
  }
  return out;
}

// The first line of code (not docs), for "go to source".
export function firstCodeLine(parts: SourcePart[]): number | undefined {
  let best: Span | undefined;
  for (const p of parts) {
    if (p.role === "doc") continue;
    if (best === undefined || before(p.span, best)) best = p.span;
  }
  return best?.[0];
}

const before = (a: Span, b: Span) =>
  a[0] < b[0] || (a[0] === b[0] && a[1] < b[1]);

// Joins parts: sorted by start; the first line padded to its column.
// Next part on the same line: right after if it touches, else " … ".
// On the next line: a newline and its column's indent. Further down:
// a gap row. A span that ends at column 1 ends on the line before.
export function sourceRows(parts: TextPart[]): SourceRow[] {
  const sorted = [...parts].sort((a, b) =>
    before(a.span, b.span) ? -1 : before(b.span, a.span) ? 1 : 0,
  );
  const rows: SourceRow[] = [];
  let cur: (SourceRow & { kind: "line" }) | undefined;
  // Where the last part ended: line and column.
  let endLine = 0;
  let endCol = 0;
  // Adds pre (no place in the file), then text, which starts at column
  // col of line: col undefined: text has no place either. A lone "\r"
  // in the middle of a line shifts the columns after it (not handled).
  const add = (pre: string, text: string, line: number, col?: number) => {
    const lines = (pre + text).replace(/\r/g, "").split("\n");
    const skip = [...pre.replace(/\r/g, "")].length;
    lines.forEach((t, i) => {
      if (i === 0 && cur !== undefined) {
        const at = [...cur.text].length;
        cur.text += t;
        note(cur, at, t, i, skip, line, col);
        return;
      }
      cur = { kind: "line", line: line + i, text: t, spans: [] };
      rows.push(cur);
      note(cur, 0, t, i, skip, line, col);
    });
  };
  // t was added to row at code point at; it is line i of the added text.
  const note = (
    row: SourceRow & { kind: "line" },
    at: number,
    t: string,
    i: number,
    skip: number,
    line: number,
    col: number | undefined,
  ) => {
    if (col === undefined) return;
    // Only the first line has the pre.
    const cut = i === 0 ? Math.min(skip, [...t].length) : 0;
    const len = [...t].length - cut;
    if (len <= 0) return;
    row.spans.push({
      at: at + cut,
      line: line + i,
      col: i === 0 ? col : 1,
      len,
    });
  };
  for (const p of sorted) {
    const [sl, sc, el, ec] = p.span;
    let text = p.text;
    // Ends at column 1: the text ends with the line break before it.
    let last = el;
    let lastCol = ec;
    if (ec === 1 && el > sl && text.endsWith("\n")) {
      text = text.replace(/\r?\n$/, "");
      last = el - 1;
      lastCol = Number.MAX_SAFE_INTEGER;
    }
    const pad = " ".repeat(Math.max(0, sc - 1));
    if (cur === undefined) {
      add(pad, text, sl, sc);
    } else if (sl === endLine) {
      add(sc <= endCol ? "" : " … ", text, sl, sc);
    } else if (sl === endLine + 1) {
      cur = undefined;
      add(pad, text, sl, sc);
    } else if (sl > endLine) {
      rows.push({ kind: "gap", from: endLine + 1, to: sl - 1 });
      cur = undefined;
      add(pad, text, sl, sc);
    } else {
      // Starts before the last end (overlap): go on from there. The
      // text is not where its lines say: no spans.
      add("", text, endLine);
    }
    if (last > endLine || (last === endLine && lastCol > endCol)) {
      endLine = last;
      endCol = lastCol;
    }
  }
  return rows;
}

// The segs of a row: runs (per file line, as in highlightOf) cut to
// the row's spans and moved to the row's columns (code points from 1).
export function rowSegs(
  row: SourceRow & { kind: "line" },
  runs: readonly (readonly Seg[])[],
): Seg[] {
  const out: Seg[] = [];
  for (const sp of row.spans) {
    const end = sp.col + sp.len;
    const shift = sp.at + 1 - sp.col;
    for (const s of runs[sp.line] ?? []) {
      const from = Math.max(s.from, sp.col);
      const to = Math.min(s.to, end);
      if (from < to) out.push({ ...s, from: from + shift, to: to + shift });
    }
  }
  return out;
}
