import { utf16Cols } from "./columns";
import type { Seg } from "./highlight";

// How a line is drawn. Control characters would be invisible, break
// the row or reorder the text (bidi), so they show as a glyph, one
// UTF-16 unit for one: a column in the DOM is a column in the text,
// which copy needs. Tab stays a tab.

export interface Piece {
  text: string;
  ctrl: boolean;
}

// C0 but tab, DEL, C1, U+2028/9 and the bidi controls and marks
// (LRM, RLM, ALM). Zero-width characters stay as they are.
const ctrlRun =
  // eslint-disable-next-line no-control-regex -- matching them is the point
  /([\x00-\x08\x0a-\x1f\x7f-\x9f\u061c\u200e\u200f\u2028\u2029\u202a-\u202e\u2066-\u2069]+)/;

// The glyph for one control character (from ctrlRun).
export function glyph(c: number): string {
  if (c < 0x20) return String.fromCharCode(0x2400 + c);
  if (c === 0x7f) return "\u2421";
  if (c === 0x85 || c === 0x2028 || c === 0x2029) return "\u2424";
  if (c >= 0x80 && c <= 0x9f) return "\u2426";
  return "\u2194";
}

// The line in runs of plain text and control glyphs.
export function pieces(line: string): Piece[] {
  // split with a group: odd items are the control runs.
  return line
    .split(ctrlRun)
    .map((text, i) =>
      i % 2 === 0
        ? { text, ctrl: false }
        : {
            text: [...text].map((ch) => glyph(ch.charCodeAt(0))).join(""),
            ctrl: true,
          },
    )
    .filter((p) => p.text !== "");
}

// A piece of a highlighted line: cls and link as in Seg.
export interface StyledPiece {
  text: string;
  cls: string | null;
  link: number;
  ctrl: boolean;
}

// More pieces than this on a line: plain (D-279). Each is a DOM node.
export const maxPieces = 5000;

// The line cut by its segs, control glyphs as in pieces(). Same text
// as pieces(line), whatever the segs (overlaps and past the end are
// cut).
export function styledPieces(
  line: string,
  segs: readonly Seg[],
): StyledPiece[] {
  if (segs.length > maxPieces) return plainPieces(line);
  const cps: number[] = [];
  let last = 1;
  for (const s of segs) {
    last = Math.max(last, s.from);
    cps.push(last);
    last = Math.max(last, s.to);
    cps.push(last);
  }
  const cols = utf16Cols(line, cps);
  const out: StyledPiece[] = [];
  const add = (from: number, to: number, cls: string | null, link: number) => {
    for (const p of pieces(line.slice(from, to))) {
      out.push({ text: p.text, cls, link, ctrl: p.ctrl });
    }
  };
  let pos = 0;
  segs.forEach((s, k) => {
    const a = cols[2 * k] ?? pos;
    const b = cols[2 * k + 1] ?? a;
    if (a > pos) add(pos, a, null, 0);
    if (b > a) add(a, b, s.cls, s.link);
    pos = Math.max(pos, b);
  });
  if (pos < line.length) add(pos, line.length, null, 0);
  return out.length > maxPieces ? plainPieces(line) : out;
}

export function plainPieces(line: string): StyledPiece[] {
  return pieces(line).map((p) => ({ ...p, cls: null, link: 0 }));
}
