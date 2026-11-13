// The hits of a local name: its binder and every use (T12). Pure.
import type { Target } from "../api/source";
import { utf16Cols } from "./columns";
import type { Matches } from "./find";
import type { Seg } from "./highlight";

// binder: line and code point column. The hits in file order, as
// find's Matches (UTF-16 columns), and which one is the binder (-1: no
// seg starts there, so the name is gone).
export function localHits(
  runs: readonly (readonly Seg[])[],
  targets: readonly Target[],
  binder: { line: number; cp: number },
  lineText: (n: number) => string,
): { matches: Matches; binderAt: number } {
  const mine = new Set<number>();
  targets.forEach((t, i) => {
    if (t[0] === "local" && t[1] === binder.line && t[2] === binder.cp) {
      mine.add(i + 1);
    }
  });
  const lines: number[] = [];
  const froms: number[] = [];
  const tos: number[] = [];
  let binderAt = -1;
  for (let n = 1; n < runs.length; n++) {
    const found: Seg[] = [];
    for (const s of runs[n] ?? []) {
      const isBinder = n === binder.line && s.from === binder.cp;
      if (isBinder || mine.has(s.link)) found.push(s);
    }
    if (found.length === 0) continue;
    found.sort((a, b) => a.from - b.from);
    const text = lineText(n);
    const cols = utf16Cols(
      text,
      found.flatMap((s) => [s.from, s.to]),
    );
    found.forEach((s, k) => {
      if (n === binder.line && s.from === binder.cp) binderAt = lines.length;
      lines.push(n);
      froms.push(cols[2 * k]!);
      tos.push(cols[2 * k + 1]!);
    });
  }
  return {
    matches: {
      count: lines.length,
      line: Uint32Array.from(lines),
      from: Uint32Array.from(froms),
      to: Uint32Array.from(tos),
    },
    binderAt,
  };
}
