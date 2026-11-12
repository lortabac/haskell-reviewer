// The highlighting of a module page's inline sources (T10b): one
// api/source fetch for the whole page, made on the first "Show
// source". Plain text until it comes, and for good if it fails or is
// for another snapshot (createSource drops those).
import { createMemo, createSignal } from "solid-js";
import { expandRuns, type LexedSource } from "../api/source";
import { createSource } from "../source/createSource";
import { type Highlight, highlightOf } from "../source/highlight";
import { pageLinkInfos } from "../source/links";

// The last lexed answers (complete ones), so a page seen again doesn't
// ask again. Newest last.
const keep = 4;
const cache = new Map<string, LexedSource>();

function cacheGet(key: string): LexedSource | undefined {
  const s = cache.get(key);
  if (s !== undefined) {
    cache.delete(key);
    cache.set(key, s);
  }
  return s;
}

function cachePut(key: string, s: LexedSource): void {
  cache.delete(key);
  cache.set(key, s);
  for (const k of cache.keys()) {
    if (cache.size <= keep) break;
    cache.delete(k);
  }
}

// For tests.
export function clearSourceCache(): void {
  cache.clear();
}

// The last line any run or region reaches: all highlightOf needs of
// the text (the page has no whole file).
export function lastLine(s: LexedSource): number {
  let n = 0;
  for (const r of expandRuns(s.runs)) n = Math.max(n, r.endLine);
  for (let i = 0; i + 2 < s.regions.length; i += 3) {
    n = Math.max(n, s.regions[i + 2] ?? 0);
  }
  return n;
}

export interface PageSource {
  // The first "Show source" calls it.
  want: () => void;
  // Undefined until the answer is there (and ok).
  highlight: () => Highlight | undefined;
}

export function createPageSource(page: {
  // As the server names the module.
  ref: string;
  typedRef: string;
  path: string | null;
  generation: number;
  base: () => string | undefined;
}): PageSource {
  const [wanted, setWanted] = createSignal(false);
  const path = page.path;
  const key = `${page.generation}\0${path}`;
  const fresh = createSource(
    () => path ?? "",
    () => page.generation,
    () => wanted() && path !== null && cacheGet(key) === undefined,
    // Not the memo: a done answer equal to a partial one must count.
    (s) => {
      if (!s.partial) cachePut(key, s);
    },
  );
  const highlight = createMemo((): Highlight | undefined => {
    if (!wanted() || path === null) return undefined;
    const s = fresh() ?? cacheGet(key);
    if (s === undefined) return undefined;
    const links = pageLinkInfos(page, s.targets, page.base());
    return highlightOf(s, lastLine(s), links);
  });
  return { want: () => setWanted(true), highlight };
}
