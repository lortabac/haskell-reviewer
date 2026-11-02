// The server search (D-207), debounced and kept fresh while the index
// is still filling in (D-209, D-210). One of these per SearchBox: two
// boxes on one page (the header's and a notice's) each run their own.
import {
  batch,
  createEffect,
  createSignal,
  on,
  onCleanup,
  untrack,
  useContext,
} from "solid-js";
import { getJson } from "../api/client";
import { decodeSearch, searchUrl, type SearchResult } from "../api/search";
import { IndexingContext } from "../events/indexing";

// Trailing debounce: a keystroke waits this long for a quieter moment
// before it fetches (plan §4.3). The very first query (e.g. a page
// prefilled from the URL) fetches at once instead.
export const debounceMs = 120;

// While the last answer was partial, at most one refetch per this many
// ms, plus one more as soon as indexing is done (never throttled).
export const refetchThrottleMs = 1000;

export interface SearchState {
  results: () => SearchResult[];
  matches: () => number;
  partial: () => boolean;
  loading: () => boolean;
  // "Search failed." territory (SearchBox): a 4xx/5xx, a network
  // error, or (in practice never, side `tree` always exists) 404.
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
  const [results, setResults] = createSignal<SearchResult[]>([]);
  const [matches, setMatches] = createSignal(0);
  const [partial, setPartial] = createSignal(false);
  const [loading, setLoading] = createSignal(false);
  const [error, setError] = createSignal<string>();
  const [answeredQuery, setAnsweredQuery] = createSignal<string>();
  const [settledQuery, setSettledQuery] = createSignal<string>();

  // Bumped on every fetch, so a late answer for an earlier one (abort
  // raced, or just slow) is dropped even if it somehow still arrives.
  let generation = 0;
  let controller: AbortController | undefined;
  // The snapshot generation of the last landed answer (Search.generation,
  // not ours above): an indexing event for a different one (a reload)
  // is ignored, not just a stale answer for the same project.
  let answerSnapshot: number | undefined;
  // Set whenever a fetch starts (debounced, prefilled or a partial
  // refetch): the throttle below counts from here, so an indexing
  // event right after the search's own first answer doesn't refetch
  // again at once.
  let lastFetchAt = 0;

  const run = (q: string): void => {
    controller?.abort();
    controller = undefined;
    if (q === "") {
      // No fetch for an empty query: nothing to search for, and
      // nothing to clear noisily either.
      generation++;
      batch(() => {
        setResults([]);
        setMatches(0);
        setPartial(false);
        setLoading(false);
        setError(undefined);
        setAnsweredQuery("");
        setSettledQuery("");
      });
      return;
    }
    const mine = ++generation;
    lastFetchAt = Date.now();
    const ac = new AbortController();
    controller = ac;
    setLoading(true);
    void getJson(searchUrl(q), decodeSearch, { signal: ac.signal }).then(
      (r) => {
        // A newer run (or a cleared query) already took over.
        if (mine !== generation) return;
        // One commit: a reader (SearchBox's "announce once" and
        // "Enter" logic) must never see loading turn false before the
        // new results, matches and partial land (Promise callbacks
        // aren't batched on their own).
        batch(() => {
          setLoading(false);
          if (r.kind === "ok") {
            // The server echoes the query it answered: a race that
            // slips past the generation check (same q refetched
            // twice) still can't show the wrong answer for a
            // different one.
            if (r.value.query !== q) return;
            setResults(r.value.results);
            setMatches(r.value.matches);
            setPartial(r.value.partial);
            setError(undefined);
            setAnsweredQuery(q);
            setSettledQuery(q);
            answerSnapshot = r.value.generation;
          } else {
            // C4: never leave a stale list under an error.
            setResults([]);
            setMatches(0);
            setPartial(false);
            setError(r.kind === "not-found" ? "not found" : r.message);
            setSettledQuery(q);
          }
        });
      },
    );
  };

  let debounceTimer: ReturnType<typeof setTimeout> | undefined;
  let first = true;
  createEffect(() => {
    const q = query();
    if (debounceTimer !== undefined) clearTimeout(debounceTimer);
    debounceTimer = undefined;
    if (first) {
      // A prefilled query (not-found page, decl notice) fetches right
      // away: there was no keystroke to debounce.
      first = false;
      run(q);
      return;
    }
    if (q === "") {
      run(q);
      return;
    }
    debounceTimer = setTimeout(() => run(q), debounceMs);
  });

  const flush = (): void => {
    if (debounceTimer === undefined) return;
    clearTimeout(debounceTimer);
    debounceTimer = undefined;
    run(query());
  };

  // Refresh while the last answer is partial and actually shown: on
  // every indexing change for the same snapshot, but throttled, plus
  // once more as soon as it's done. An event older than the answer's
  // own snapshot (can't happen in practice, but the server makes no
  // promise about event order) is just stale, ignored; one newer (a
  // Reload) means our answer is for a gone snapshot, so it refetches
  // at once, same as done.
  createEffect(() => {
    const progress = indexingCtx.indexing();
    if (progress === undefined || !partial() || !visible()) return;
    if (answerSnapshot !== undefined && progress.generation < answerSnapshot) {
      return;
    }
    const newerSnapshot =
      answerSnapshot !== undefined && progress.generation > answerSnapshot;
    const done = progress.ready + progress.failed >= progress.total;
    if (
      !newerSnapshot &&
      !done &&
      Date.now() - lastFetchAt < refetchThrottleMs
    ) {
      return;
    }
    // Not tracked: a change in the query is the debounce effect's
    // job, not this one's.
    run(untrack(query));
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
        if (shown && !wasShown && partial()) run(untrack(query));
      },
      { defer: true },
    ),
  );

  onCleanup(() => {
    if (debounceTimer !== undefined) clearTimeout(debounceTimer);
    controller?.abort();
  });

  return {
    results,
    matches,
    partial,
    loading,
    error,
    answeredQuery,
    settledQuery,
    flush,
  };
}
