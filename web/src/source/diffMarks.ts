// Change markers for one side's lines, from the diff rows (D-348).
// Pure. A change block is a run of changed rows (rows.ts). Each block
// has d removed and i added rows.
//
// Head: a block with added rows marks them (added if d = 0, else
// modified). A block with only removed rows marks the line before it
// "removed below" (line 1 "removed above" at the file start). Base is
// the same with the roles swapped.
import { blocks, deleteRow, insertRow, type Rows } from "../diff/rows";
import type { DiffSessionSide } from "../sides";
import { changeWords } from "../ui/ChangeSign";

// Values of SideChanges.own, and the bits of SideChanges.edge.
export const addedLine = 1;
export const removedLine = 2;
export const modifiedLine = 3;

export const edgeBelow = 1;
export const edgeAbove = 2;

export interface SideChanges {
  // By line (index 0 unused): 0, added, removed or modified.
  own: Uint8Array;
  // By line: bits below and above, for the lines the other side lost
  // (head) or gained (base) there.
  edge: Uint8Array;
  // One line a block, ascending: its first own line, else its edge.
  starts: number[];
}

export function sideChanges(rows: Rows, side: DiffSessionSide): SideChanges {
  const lines = side === "head" ? rows.head : rows.base;
  const n = side === "head" ? rows.headCount : rows.baseCount;
  const mine = side === "head" ? insertRow : deleteRow;
  const own = new Uint8Array(n + 1);
  const edge = new Uint8Array(n + 1);
  const starts: number[] = [];
  const { kind } = rows;
  for (const [s, e] of blocks(rows)) {
    let m = 0;
    for (let r = s; r < e; r++) if (kind[r] === mine) m++;
    // [s, e) has m of my rows and e - s - m of the other's.
    let at = 0;
    if (m > 0) {
      const v = m === e - s ? addedLine : modifiedLine;
      const own1 = side === "head" ? v : v === addedLine ? removedLine : v;
      for (let r = s; r < e; r++) {
        if (kind[r] !== mine) continue;
        if (at === 0) at = lines[r] ?? 0;
        own[lines[r] ?? 0] = own1;
      }
    } else if (s > 0) {
      at = lines[s - 1] ?? 0;
      edge[at] = (edge[at] ?? 0) | edgeBelow;
    } else if (n > 0) {
      at = 1;
      edge[1] = (edge[1] ?? 0) | edgeAbove;
    }
    if (at > 0 && starts[starts.length - 1] !== at) starts.push(at);
  }
  return { own, edge, starts };
}

// What a reader hears for a line, if it has a change.
export function changeWord(
  c: SideChanges,
  line: number,
  side: DiffSessionSide,
): string | undefined {
  const o = c.own[line] ?? 0;
  if (o === addedLine) return changeWords.added;
  if (o === removedLine) return changeWords.removed;
  if (o === modifiedLine) return changeWords.modified;
  const e = c.edge[line] ?? 0;
  if (e === 0) return undefined;
  const what = side === "head" ? "removed" : "added";
  const where =
    e === (edgeBelow | edgeAbove)
      ? "above and below"
      : e === edgeBelow
        ? "below"
        : "above";
  return `lines ${what} ${where}`;
}

// "added line 5", "line 7, lines removed below": for the status line.
export function changeStatus(
  c: SideChanges,
  line: number,
  side: DiffSessionSide,
): string {
  const w = changeWord(c, line, side);
  if (w === undefined) return `line ${line}`;
  return (c.own[line] ?? 0) > 0 ? `${w} line ${line}` : `line ${line}, ${w}`;
}
