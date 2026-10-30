// A declaration's source (SPEC §4.3), as rows to show: its parts in
// source order, joined, with line numbers, and "⋯" rows for the lines
// left out. Plain text: .lhs ">" and CPP lines as written.
import type { SourcePart, Span } from "../api/module";

// A part with its text (shared ones resolved).
export interface TextPart {
  span: Span;
  text: string;
}

export type SourceRow =
  | { kind: "line"; line: number; text: string }
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
  let cur: { kind: "line"; line: number; text: string } | undefined;
  // Where the last part ended: line and column.
  let endLine = 0;
  let endCol = 0;
  const add = (text: string, line: number) => {
    const lines = text.replace(/\r/g, "").split("\n");
    lines.forEach((t, i) => {
      if (i === 0 && cur !== undefined) {
        cur.text += t;
        return;
      }
      cur = { kind: "line", line: line + i, text: t };
      rows.push(cur);
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
      add(pad + text, sl);
    } else if (sl === endLine) {
      add(sc <= endCol ? text : " … " + text, sl);
    } else if (sl === endLine + 1) {
      cur = undefined;
      add(pad + text, sl);
    } else if (sl > endLine) {
      rows.push({ kind: "gap", from: endLine + 1, to: sl - 1 });
      cur = undefined;
      add(pad + text, sl);
    } else {
      // Starts before the last end (overlap): go on from there.
      add(text, endLine);
    }
    if (last > endLine || (last === endLine && lastCol > endCol)) {
      endLine = last;
      endCol = lastCol;
    }
  }
  return rows;
}
