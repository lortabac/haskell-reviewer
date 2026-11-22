// Text search hits in pieces and groups, and their words. Pure.
import type { TextHit, TextSearch } from "../api/textSearch";
import { utf16Cols } from "../source/columns";
import { plural } from "../ui/format";

export interface TextSnippet {
  // The start of the line is not shown: show "…".
  cut: boolean;
  // The line in runs, hit or not.
  parts: { text: string; hit: boolean }[];
}

// lead: start at most this many code points before the first hit (the
// one-line rows of the box), else the whole text.
export function textSnippet(h: TextHit, lead?: number): TextSnippet {
  const n = [...h.text].length;
  // Columns of h.text, from 1. A hit past the server's cut is dropped
  // or cut.
  const spans: [number, number][] = [];
  for (const [a, b] of h.matches) {
    const from = Math.max(1, a - h.lineCol + 1);
    const to = Math.min(n + 1, b - h.lineCol + 1);
    if (from < to) spans.push([from, to]);
  }
  const first = spans[0];
  // A whole line loses its indent (white space is one UTF-16 unit, so
  // this is in code points too); a cut one keeps what it has.
  const indent =
    h.lineCol === 1 ? h.text.length - h.text.trimStart().length : 0;
  let start = 1 + indent;
  if (lead !== undefined && first !== undefined) {
    start = Math.max(start, first[0] - lead);
  }
  if (first !== undefined) start = Math.min(start, first[0]);
  const cols = utf16Cols(h.text, [start, ...spans.flat()]);
  const s0 = cols[0] ?? 0;
  const parts: { text: string; hit: boolean }[] = [];
  let at = s0;
  for (let i = 0; i < spans.length; i++) {
    const a = cols[1 + 2 * i] ?? at;
    const b = cols[2 + 2 * i] ?? a;
    if (a > at) parts.push({ text: h.text.slice(at, a), hit: false });
    parts.push({ text: h.text.slice(a, b), hit: true });
    at = b;
  }
  if (at < h.text.length) parts.push({ text: h.text.slice(at), hit: false });
  const cut = h.lineCol > 1 || start > 1 + indent;
  return { cut, parts };
}

// Where a hit link lands in the source view: its line, and the first
// hit's column and length in code points (FileView marks it).
export function hitState(h: TextHit): {
  codeJump: true;
  cp: number;
  len: number;
} {
  const [a, b] = h.matches[0] ?? [h.lineCol, h.lineCol];
  return { codeJump: true, cp: a, len: b - a };
}

// A key for a hit that is stable across fetches.
export function hitKey(h: TextHit): string {
  return `${h.path}:${h.line}`;
}

export interface HitGroup {
  path: string;
  hits: TextHit[];
}

// Runs of the same path, in server order (path, then line).
export function groupHits(results: readonly TextHit[]): HitGroup[] {
  const out: HitGroup[] = [];
  for (const h of results) {
    const g = out[out.length - 1];
    if (g?.path === h.path) g.hits.push(h);
    else out.push({ path: h.path, hits: [h] });
  }
  return out;
}

const count = (n: number): string => n.toLocaleString("en-US");

// "12 matching lines in 3 files." or, past the cap, "Showing the first
// 1,000 of 4,145 matching lines."
export function textSummary(
  t: Pick<TextSearch, "total" | "files" | "results">,
): string {
  if (t.total === 0) return "No matches.";
  if (t.results.length < t.total) {
    return (
      `Showing the first ${count(t.results.length)} of ` +
      `${plural(t.total, "matching line", "matching lines")}.`
    );
  }
  return (
    `${plural(t.total, "matching line", "matching lines")} in ` +
    `${plural(t.files, "file", "files")}.`
  );
}

// "3 files not searched: 2 binary, 1 too large."; "" for none.
export function skippedText(s: TextSearch["skipped"]): string {
  const n = s.binary + s.tooLarge + s.unreadable + s.overBudget;
  if (n === 0) return "";
  const why = [
    s.binary > 0 ? `${count(s.binary)} binary` : "",
    s.tooLarge > 0 ? `${count(s.tooLarge)} too large` : "",
    s.unreadable > 0 ? `${count(s.unreadable)} unreadable` : "",
    s.overBudget > 0 ? `${count(s.overBudget)} over the memory limit` : "",
  ].filter((w) => w !== "");
  return `${plural(n, "file", "files")} not searched: ${why.join(", ")}.`;
}

export const stoppedText = "Search stopped after 1 s: results are incomplete.";
