// Refs in groups and words. Pure.
import type { Ref, Refs } from "../api/refs";

// A ref with a key that holds still while the list is asked again.
export type Item = Ref & { key: string };

export interface Group {
  key: string;
  // Module name and ref, file.
  module: string;
  ref: string;
  path: string | null;
  items: Item[];
}

// Runs of the same (ref, path), in server order (home first, then
// module, path, line).
export function groupRefs(results: readonly Ref[]): Group[] {
  const out: Group[] = [];
  for (const r of results) {
    const g = out[out.length - 1];
    const item = { ...r, key: refKey(r) };
    if (g !== undefined && g.ref === r.ref && g.path === r.path) {
      g.items.push(item);
    } else {
      out.push({
        key: `${r.ref}|${r.path ?? ""}`,
        module: r.module,
        ref: r.ref,
        path: r.path,
        items: [item],
      });
    }
  }
  return out;
}

const plural = (n: number, one: string, many: string) =>
  `${n.toLocaleString("en-US")} ${n === 1 ? one : many}`;

// "10 references in 3 modules." or, past the cap, "Showing the first
// 1,000 of 4,145 references."
export function summary(refs: Pick<Refs, "total" | "results">): string {
  const { total, results } = refs;
  if (total === 0) return "No references found.";
  if (results.length < total) {
    return (
      `Showing the first ${results.length.toLocaleString("en-US")} of ` +
      `${total.toLocaleString("en-US")} references.`
    );
  }
  const modules = new Set(results.map((r) => r.ref)).size;
  return `${plural(total, "reference", "references")} in ${plural(
    modules,
    "module",
    "modules",
  )}.`;
}

// "2 are guesses." or "1 is a guess."; "" for none. capped: only the
// shown ones are counted, and it says so.
export function guessCount(results: readonly Ref[], capped = false): string {
  const n = results.filter((r) => r.heuristic).length;
  if (n === 0) return "";
  const k = n.toLocaleString("en-US");
  if (capped)
    return n === 1 ? "1 of these is a guess." : `${k} of these are guesses.`;
  return n === 1 ? "1 is a guess." : `${k} are guesses.`;
}

function refKey(r: Ref): string {
  return `${r.ref}|${r.path ?? ""}|${r.span.join(",")}|${r.kind}`;
}
