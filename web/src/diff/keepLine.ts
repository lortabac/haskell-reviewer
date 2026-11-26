// Where a diff line goes after a reload (D-356). Pure.
//
// Base is a fixed commit, so a base line stays. A head line is kept
// through base: mapLine (D-349) takes it to a base line, and we keep
// how far it is from that base line's own head line. In the new rows:
// that base line's head line plus the offset, clamped to head. No
// base line to go through (an added file): the same line, clamped.
// Head has no lines any more (the file was removed): that base line.
import type { DiffSessionSide } from "../sides";
import { clampLine } from "../source/keepLine";
import { mapLine } from "./mapLine";
import type { Rows } from "./rows";

export interface DiffPlace {
  side: DiffSessionSide;
  line: number;
  // Head only: the base line it goes through, and the offset.
  base?: { line: number; offset: number };
}

export function diffPlaceOf(
  rows: Rows,
  side: DiffSessionSide,
  line: number,
): DiffPlace {
  if (side === "base") return { side, line };
  const b = mapLine(rows, "head", line);
  if (b === undefined) return { side, line };
  const back = mapLine(rows, "base", b.line);
  if (back === undefined) return { side, line };
  return { side, line, base: { line: b.line, offset: line - back.line } };
}

export function diffLineOf(
  place: DiffPlace,
  rows: Rows,
): { side: DiffSessionSide; line: number } {
  if (place.side === "base") {
    return { side: "base", line: clampLine(place.line, rows.baseCount) };
  }
  const b = place.base;
  if (b === undefined) {
    return { side: "head", line: clampLine(place.line, rows.headCount) };
  }
  if (rows.headCount === 0) {
    return { side: "base", line: clampLine(b.line, rows.baseCount) };
  }
  const h = mapLine(rows, "base", b.line);
  const line = h === undefined ? place.line : h.line + b.offset;
  return { side: "head", line: clampLine(line, rows.headCount) };
}

// Both steps at once (the tests).
export function keepDiffLine(
  oldRows: Rows,
  newRows: Rows,
  side: DiffSessionSide,
  line: number,
): { side: DiffSessionSide; line: number } {
  return diffLineOf(diffPlaceOf(oldRows, side, line), newRows);
}

// A place read back from history state. Undefined: not one.
export function diffPlaceFrom(v: unknown): DiffPlace | undefined {
  if (typeof v !== "object" || v === null) return undefined;
  const { side, line, base } = v as Record<string, unknown>;
  if (side !== "base" && side !== "head") return undefined;
  if (typeof line !== "number" || !Number.isInteger(line)) return undefined;
  if (base === undefined) return { side, line };
  if (typeof base !== "object" || base === null) return undefined;
  const { line: bl, offset } = base as Record<string, unknown>;
  if (typeof bl !== "number" || typeof offset !== "number") return undefined;
  return { side, line, base: { line: bl, offset } };
}
