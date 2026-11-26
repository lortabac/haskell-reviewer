// The server search (D-207), debounced and kept fresh while the index
// is still filling in (D-209, D-210). One of these per SearchBox: two
// boxes on one page (the header's and a notice's) each run their own.
import { createEffect, createMemo, on, untrack, useContext } from "solid-js";
import { getJson } from "../api/client";
import { decodeSearch, searchUrl, type SearchResult } from "../api/search";
import { useGeneration } from "../events/generation";
import { IndexingContext } from "../events/indexing";
import { refetchStep } from "../events/partialRefetch";
import { usePageSide } from "../session";
import { createFetch } from "./createFetch";

export { debounceMs } from "./createFetch";
export { refetchThrottleMs } from "../events/partialRefetch";

export interface SearchState {
  results: () => SearchResult[];
  matches: () => number;
  partial: () => boolean;
  loading: () => boolean;
  // "Search failed." territory (SearchBox): a 4xx/5xx, a network
  // error, or (in practice never: the page's side exists) 404.
  error: () => string | undefined;
  // The query results/matches/partial actually answer, the moment they
  // settle; undefined before the first answer. SearchBox uses this
  // (not query() + loading()) to know exactly when to announce, and to
  // tell a fresh answer from a stale one before acting on Enter:
  // query() changes right away on a keystroke, well before the
  // debounced fetch even starts, which is too early for either.
  answeredQuery: () => string | undefined;
  // Like answeredQuery, but also set on an error (SearchBox announces
  // either): the query a fetch last settled for, success or not.
  settledQuery: () => string | undefined;
  // Fires the debounced fetch now instead of waiting out the rest of
  // debounceMs (Enter: don't act on the previous query's results).
  // A no-op if there's nothing debounced (already fetching, or empty).
  flush: () => void;
}

// query: the text to search for, read reactively. Empty clears the
// results without fetching. visible: whether the box's results are
// actually shown (the header's popup open, or the page box, which is
// the list) — a partial refresh is pointless (and a quiet live region
// still ticks a clock) for a list nobody can see.
export function createSearch(
  query: () => string,
  visible: () => boolean,
): SearchState {
  const indexingCtx = useContext(IndexingContext);
  // The page's side, or the default one on pages with none (G15).
  const side = usePageSide();
  const f = createFetch(
    query,
    side,
    (q, s, signal) => getJson(searchUrl(s, q), decodeSearch, { signal }),
    (v) => v.query,
  );
  // A memo: a new answer that is still partial must not count as a
  // change (the effect below would refetch again at once at done).
  const partial = createMemo(() => f.value()?.partial ?? false);

  // Refresh while the last answer is partial and actually shown: on
  // every indexing change for the same snapshot, but throttled, plus
  // once more as soon as it's done. An event older than the answer's
  // own snapshot is just stale, ignored; one newer (a Reload) means
  // our answer is for a gone snapshot, so it refetches at once, same
  // as done. The snapshot is the answer's (Search.generation): an
  // event for another one is not about this answer.
  createEffect(() => {
    const progress = indexingCtx.of(side());
    if (progress === undefined || !partial() || !visible()) return;
    const snapshot = untrack(f.value)?.generation;
    const step = refetchStep(progress, snapshot, f.lastFetchAt(), Date.now());
    if (step === "older" || step === "throttled") return;
    // Not tracked: a change in the query is the debounce's job.
    f.run(untrack(query));
  });

  // Catching up: the box can go from hidden to shown (the header
  // popup reopens) without any indexing event happening right then,
  // so the effect above alone would leave a stale partial answer on
  // screen until the next one. Once, not throttled: reopening is a
  // deliberate, infrequent action.
  createEffect(
    on(
      visible,
      (shown, wasShown) => {
        if (shown && !wasShown && partial()) f.run(untrack(query));
      },
      { defer: true },
    ),
  );

  // A reload (D-354): an answer of an older generation is stale, partial
  // or not. Asked again once shown (the header popup can be closed).
  const gen = useGeneration();
  const stale = createMemo(() => {
    const a = f.value()?.generation;
    const c = gen.current();
    return a !== undefined && c !== undefined && a < c;
  });
  createEffect(() => {
    if (stale() && visible()) f.run(untrack(query));
  });

  return {
    results: () => f.value()?.results ?? [],
    matches: () => f.value()?.matches ?? 0,
    partial,
    loading: f.loading,
    error: f.error,
    answeredQuery: f.answeredQuery,
    settledQuery: f.settledQuery,
    flush: f.flush,
  };
}
