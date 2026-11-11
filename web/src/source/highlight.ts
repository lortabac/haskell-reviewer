// api/source's runs and regions cut per line, once per view. Pure: no
// DOM here (decor.ts makes the nodes).
import {
  displayClasses,
  expandRuns,
  type LexedSource,
  type OutlineEntry,
  regionKinds,
  type Target,
} from "../api/source";
import type { LinkInfo } from "./links";

// A run's part on one line. Code point columns from 1, end exclusive;
// to is Infinity when it goes on to the next line. cls: a display
// class name. link: 0 none, else 1 + a target index (T08).
export interface Seg {
  from: number;
  to: number;
  cls: string | null;
  link: number;
}

// Segs of line n at index n (0 unused). Runs over lines are cut; an end
// at (L + 1, 1) gives nothing on L + 1; past the last line is dropped.
export function lineRuns(runs: readonly number[], count: number): Seg[][] {
  const out: Seg[][] = Array.from({ length: count + 1 }, () => []);
  for (const r of expandRuns(runs)) {
    const cls = displayClasses[r.cls] ?? null;
    const last = Math.min(r.endLine, count);
    for (let l = Math.max(1, r.startLine); l <= last; l++) {
      const from = l === r.startLine ? r.startCol : 1;
      const to = l === r.endLine ? r.endCol : Infinity;
      if (from < to) out[l]?.push({ from, to, cls, link: r.link });
    }
  }
  return out;
}

export type LineKind = "code" | "directive" | "excluded";

// What each line is for CPP, at index n. Excluded wins over a
// directive. Prose is code here: it comes as comment runs (D-232).
export function lineKinds(
  regions: readonly number[],
  count: number,
): LineKind[] {
  const out: LineKind[] = Array.from({ length: count + 1 }, () => "code");
  for (let i = 0; i + 2 < regions.length; i += 3) {
    const kind = regionKinds[regions[i] ?? 0];
    if (kind !== "directives" && kind !== "excluded") continue;
    const to = Math.min(count, regions[i + 2] ?? 0);
    for (let l = Math.max(1, regions[i + 1] ?? 1); l <= to; l++) {
      if (kind === "excluded") out[l] = "excluded";
      else if (out[l] === "code") out[l] = "directive";
    }
  }
  return out;
}

// The first outline entry on each line.
export function outlineByLine(
  outline: readonly OutlineEntry[],
): Map<number, OutlineEntry> {
  const out = new Map<number, OutlineEntry>();
  for (const e of outline) if (!out.has(e[3])) out.set(e[3], e);
  return out;
}

// A local binder is no link (D-283): the seg that starts where its own
// local target points gets link 0.
export function dropBinderLinks(
  runs: Seg[][],
  targets: readonly Target[],
): void {
  targets.forEach((t, i) => {
    if (t[0] !== "local") return;
    for (const s of runs[t[1]] ?? []) {
      if (s.link === i + 1 && s.from === t[2]) s.link = 0;
    }
  });
}

// All a view needs from a lexed source.
export interface Highlight {
  runs: Seg[][];
  // By link word − 1; undefined: not a link.
  links: readonly (LinkInfo | undefined)[];
  kinds: LineKind[];
  // Entries on a line of the file only (no dead "line N" links).
  outline: OutlineEntry[];
  byLine: Map<number, OutlineEntry>;
}

export function highlightOf(
  s: LexedSource,
  count: number,
  links: readonly (LinkInfo | undefined)[] = [],
): Highlight {
  const outline = s.outline.filter((e) => e[3] >= 1 && e[3] <= count);
  const runs = lineRuns(s.runs, count);
  dropBinderLinks(runs, s.targets);
  return {
    runs,
    links,
    kinds: lineKinds(s.regions, count),
    outline,
    byLine: outlineByLine(outline),
  };
}
