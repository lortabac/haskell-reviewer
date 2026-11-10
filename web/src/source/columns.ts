// Server columns to ours. The server counts code points from 1 (tab
// is 1, end exclusive: Note [Span convention] in Core/Span.hs); the DOM
// and lines.ts count UTF-16 units from 0. An astral character is 1
// code point but 2 units.

// The UTF-16 column (from 0) of each code point column (from 1) in
// cps, which must go up. One scan of the line. Past the end: the
// line's length. Below 1: 0.
export function utf16Cols(line: string, cps: ArrayLike<number>): number[] {
  const out: number[] = [];
  let i = 0;
  let cp = 1;
  for (let k = 0; k < cps.length; k++) {
    const want = cps[k] ?? 1;
    while (cp < want && i < line.length) {
      i += isPair(line, i) ? 2 : 1;
      cp++;
    }
    out.push(i);
  }
  return out;
}

// A surrogate pair starts at i.
function isPair(s: string, i: number): boolean {
  const c = s.charCodeAt(i);
  if (c < 0xd800 || c > 0xdbff) return false;
  const d = s.charCodeAt(i + 1);
  return d >= 0xdc00 && d <= 0xdfff;
}
