// Where a source line goes after a reload (D-356). Pure.
//
// A line is kept by the outline entry it is in (declAtLine): the
// entry's id, its index among the entries with that id, and the
// offset from its first line. In the new outline: the same entry's
// first line plus the offset, clamped to the entry. A line below an
// entry (between two) keeps its offset from that entry's last line.
// No entry, or the entry is gone: the same line. Always clamped to
// the file.
import type { OutlineEntry } from "../api/source";
import { declAtLine } from "../keys/outline";

export interface SourcePlace {
  line: number;
  decl?: {
    id: string;
    // Its index among the entries with that id.
    nth: number;
    // Counted from the entry's first line (inside it) or its last
    // (below it).
    from: "first" | "last";
    offset: number;
  };
}

export function placeOf(
  outline: readonly OutlineEntry[],
  line: number,
): SourcePlace {
  const e = declAtLine(outline, line);
  if (e === undefined) return { line };
  const [, id, , , first, last] = e;
  let nth = 0;
  for (const o of outline) {
    if (o === e) break;
    if (o[1] === id) nth++;
  }
  const from = line <= last ? "first" : "last";
  const offset = line - (from === "first" ? first : last);
  return { line, decl: { id, nth, from, offset } };
}

// n in 1..count (1 for an empty file).
export const clampLine = (n: number, count: number): number =>
  Math.max(1, Math.min(Math.max(count, 1), n));

// The line of place in a file of count lines. outline undefined: not
// here yet.
export function lineOf(
  place: SourcePlace,
  outline: readonly OutlineEntry[] | undefined,
  count: number,
): number {
  const d = place.decl;
  const e =
    d === undefined || outline === undefined
      ? undefined
      : nthEntry(outline, d.id, d.nth);
  if (d === undefined || e === undefined) return clampLine(place.line, count);
  const [, , , , first, last] = e;
  const line =
    d.from === "first"
      ? Math.max(first, Math.min(last, first + d.offset))
      : last + d.offset;
  return clampLine(line, count);
}

function nthEntry(
  outline: readonly OutlineEntry[],
  id: string,
  nth: number,
): OutlineEntry | undefined {
  let k = 0;
  for (const e of outline) {
    if (e[1] !== id) continue;
    if (k === nth) return e;
    k++;
  }
  return undefined;
}

// Both steps at once (the tests).
export function keepSourceLine(
  oldOutline: readonly OutlineEntry[],
  newOutline: readonly OutlineEntry[] | undefined,
  count: number,
  line: number,
): number {
  return lineOf(placeOf(oldOutline, line), newOutline, count);
}

// A place read back from history state. Undefined: not one.
export function sourcePlaceFrom(v: unknown): SourcePlace | undefined {
  if (typeof v !== "object" || v === null) return undefined;
  const { line, decl } = v as { line?: unknown; decl?: unknown };
  if (typeof line !== "number" || !Number.isInteger(line)) return undefined;
  if (decl === undefined) return { line };
  if (typeof decl !== "object" || decl === null) return undefined;
  const { id, nth, from, offset } = decl as Record<string, unknown>;
  if (
    typeof id !== "string" ||
    typeof nth !== "number" ||
    (from !== "first" && from !== "last") ||
    typeof offset !== "number"
  ) {
    return undefined;
  }
  return { line, decl: { id, nth, from, offset } };
}

// What a source page keeps over a reload, in the landing's history
// state: the cursor, the marked lines (the hash) and the top line on
// screen.
export interface SourceKeep {
  cursor: SourcePlace;
  col: number;
  marks?: [SourcePlace, SourcePlace];
  // The top line on screen, and how many more lines were on screen.
  top?: { place: SourcePlace; below: number };
}

export function sourceKeepFrom(v: unknown): SourceKeep | undefined {
  if (typeof v !== "object" || v === null) return undefined;
  const k = v as Record<string, unknown>;
  const cursor = sourcePlaceFrom(k.cursor);
  if (cursor === undefined || typeof k.col !== "number") return undefined;
  const out: SourceKeep = { cursor, col: k.col };
  if (Array.isArray(k.marks)) {
    const a = sourcePlaceFrom(k.marks[0]);
    const b = sourcePlaceFrom(k.marks[1]);
    if (a !== undefined && b !== undefined) out.marks = [a, b];
  }
  if (typeof k.top === "object" && k.top !== null) {
    const { place, below } = k.top as Record<string, unknown>;
    const p = sourcePlaceFrom(place);
    if (p !== undefined && typeof below === "number") {
      out.top = { place: p, below };
    }
  }
  return out;
}
