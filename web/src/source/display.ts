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
