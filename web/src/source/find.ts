// Find in file (T09, G11): literal, smart case, the whole text. Pure:
// no DOM. Lines and columns as in lines.ts (UTF-16, columns from 0).
import { type LineIndex } from "./lines";

// The hits, in order. Parallel arrays: hit i is on line[i] (from 1),
// from col from[i] up to col to[i].
export interface Matches {
  count: number;
  line: Uint32Array;
  from: Uint32Array;
  to: Uint32Array;
}

export const noMatches: Matches = {
  count: 0,
  line: new Uint32Array(0),
  from: new Uint32Array(0),
  to: new Uint32Array(0),
};

// Lower case, one code point at a time: no final-sigma rule, and the
// UTF-16 length stays (offsets must stay valid): a char whose lower
// case has another length (U+0130) stays as it is. Never locale.
function fold(s: string): string {
  // Only these two differ from a plain toLowerCase of the whole text.
  if (!s.includes("\u03a3") && !s.includes("\u0130")) return s.toLowerCase();
  let out = "";
  for (const ch of s) {
    const l = ch.toLowerCase();
    out += l.length === ch.length ? l : ch;
  }
  return out;
}

export interface Finder {
  search: (query: string) => Matches;
}

export function createFinder(text: string, index: LineIndex): Finder {
  let folded: string | undefined;
  const search = (q: string): Matches => {
    // A hit never spans a line break (and "" has no hit).
    if (q === "" || q.includes("\n") || index.count === 0) return noMatches;
    // Smart case: any capital makes it exact.
    const exact = q !== q.toLowerCase();
    const hay = exact ? text : (folded ??= fold(text));
    const needle = exact ? q : fold(q);
    let cap = 64;
    let line: Uint32Array = new Uint32Array(cap);
    let from: Uint32Array = new Uint32Array(cap);
    let to: Uint32Array = new Uint32Array(cap);
    let count = 0;
    let li = 0;
    let pos = 0;
    for (;;) {
      const i = hay.indexOf(needle, pos);
      if (i === -1) break;
      while (li + 1 < index.count && index.starts[li + 1]! <= i) li++;
      const start = index.starts[li]!;
      // Not in the line (BOM, the "\r" of CRLF): try one on.
      if (i < start || i + needle.length > index.ends[li]!) {
        pos = i + 1;
        continue;
      }
      if (count === cap) {
        cap *= 2;
        line = grow(line, cap);
        from = grow(from, cap);
        to = grow(to, cap);
      }
      line[count] = li + 1;
      from[count] = i - start;
      to[count] = i - start + needle.length;
      count++;
      // No overlap.
      pos = i + needle.length;
    }
    return {
      count,
      line: line.slice(0, count),
      from: from.slice(0, count),
      to: to.slice(0, count),
    };
  };
  return { search };
}

function grow(a: Uint32Array, n: number): Uint32Array<ArrayBuffer> {
  const b = new Uint32Array(n);
  b.set(a);
  return b;
}

// The hits of line n: [lo, hi).
export function inLine(m: Matches, n: number): [number, number] {
  return [firstAt(m, n, 0), firstAt(m, n + 1, 0)];
}

// The first hit at or after (line, col); m.count if none.
function firstAt(m: Matches, line: number, col: number): number {
  let lo = 0;
  let hi = m.count;
  while (lo < hi) {
    const mid = (lo + hi) >> 1;
    const l = m.line[mid]!;
    if (l < line || (l === line && m.from[mid]! < col)) lo = mid + 1;
    else hi = mid;
  }
  return lo;
}

// The first hit at or after (line, col), wrapping; -1 if none.
export function nearest(m: Matches, line: number, col: number): number {
  if (m.count === 0) return -1;
  const i = firstAt(m, line, col);
  return i === m.count ? 0 : i;
}

// The next (d = 1) or previous (d = -1) hit, wrapping.
export function step(m: Matches, i: number, d: 1 | -1): number {
  if (m.count === 0) return -1;
  return (i + d + m.count) % m.count;
}

// For the live region.
export function status(m: Matches, i: number): string {
  if (m.count === 0 || i < 0) return "No matches";
  return `Match ${i + 1} of ${m.count}, line ${m.line[i]}`;
}
