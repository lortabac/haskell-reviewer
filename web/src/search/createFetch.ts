// The fetch part of a search box search (D-209): a trailing debounce,
// abort, and a sequence number plus the echoed query so a stale answer
// is dropped. Shared by the name search and the text search.
import {
  batch,
  createEffect,
  createSignal,
  onCleanup,
  untrack,
} from "solid-js";
import type { Result } from "../api/client";
import type { Side } from "../sides";

// Trailing debounce: a keystroke waits this long for a quieter moment
// before it fetches (plan §4.3). The very first query (e.g. a page
// prefilled from the URL) fetches at once instead.
export const debounceMs = 120;

export interface FetchState<T> {
  // The last good answer; undefined before one, for "", or after an
  // error (C4: never a stale list under an error).
  value: () => T | undefined;
  loading: () => boolean;
  error: () => string | undefined;
  // The query value() answers, once it lands ("" for an empty query).
  answeredQuery: () => string | undefined;
  // Like answeredQuery, but also set on an error.
  settledQuery: () => string | undefined;
  // Fetch now what is waiting on the debounce. No-op if nothing is.
  flush: () => void;
  // Fetch q now (a refresh), dropping what is in flight.
  run: (q: string) => void;
  // When the last fetch started (Date.now()).
  lastFetchAt: () => number;
  // The next change of the query fetches at once, no debounce.
  atOnce: () => void;
}

// query: read reactively; "" clears without a fetch. side: the side to
// search, read reactively; a change asks again at once. get: one
// fetch. echo: the query an answer says it is for.
//
// Answers are kept with their side (D-332): after a side change,
// value, answeredQuery and settledQuery are undefined until that
// side's answer lands, so nothing of the other side shows.
export function createFetch<T>(
  query: () => string,
  side: () => Side,
  get: (q: string, side: Side, signal: AbortSignal) => Promise<Result<T>>,
  echo: (v: T) => string,
): FetchState<T> {
  // Each with the side it is for. Always set together, in one batch.
  const [stored, setStored] = createSignal<{ side: Side; v: T }>();
  const [loading, setLoading] = createSignal(false);
  const [error, setError] = createSignal<string>();
  const [answered, setAnswered] = createSignal<{
    side: Side;
    q: string;
  }>();
  const [settled, setSettled] = createSignal<{ side: Side; q: string }>();
  const value = (): T | undefined => {
    const x = stored();
    return x?.side === side() ? x.v : undefined;
  };
  const answeredQuery = (): string | undefined => {
    const x = answered();
    return x?.side === side() ? x.q : undefined;
  };
  const settledQuery = (): string | undefined => {
    const x = settled();
    return x?.side === side() ? x.q : undefined;
  };

  // Bumped on every fetch, so a late answer for an earlier one is
  // dropped even if the abort raced.
  let generation = 0;
  let controller: AbortController | undefined;
  let lastFetchAt = 0;

  const run = (q: string): void => {
    controller?.abort();
    controller = undefined;
    const s = untrack(side);
    if (q === "") {
      generation++;
      batch(() => {
        setStored(undefined);
        setLoading(false);
        setError(undefined);
        setAnswered({ side: s, q: "" });
        setSettled({ side: s, q: "" });
      });
      return;
    }
    const mine = ++generation;
    lastFetchAt = Date.now();
    const ac = new AbortController();
    controller = ac;
    setLoading(true);
    void get(q, s, ac.signal).then((r) => {
      if (mine !== generation) return;
      // One commit: a reader must never see loading turn false before
      // the answer lands.
      batch(() => {
        setLoading(false);
        if (r.kind === "ok") {
          // Same q asked twice can slip past the generation check; the
          // echo still can't show the answer of another query.
          if (echo(r.value) !== q) return;
          setStored({ side: s, v: r.value });
          setError(undefined);
          setAnswered({ side: s, q });
          setSettled({ side: s, q });
        } else {
          setStored(undefined);
          setError(r.kind === "not-found" ? "not found" : r.message);
          setSettled({ side: s, q });
        }
      });
    });
  };

  let debounceTimer: ReturnType<typeof setTimeout> | undefined;
  let first = true;
  let now = false;
  let lastSide: Side | undefined;
  createEffect(() => {
    const q = query();
    const s = side();
    const sideChanged = lastSide !== undefined && s !== lastSide;
    lastSide = s;
    if (debounceTimer !== undefined) clearTimeout(debounceTimer);
    debounceTimer = undefined;
    if (first || now || sideChanged || q === "") {
      // A prefilled query has no keystroke to debounce; nor has a
      // side change.
      first = false;
      now = false;
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

  onCleanup(() => {
    if (debounceTimer !== undefined) clearTimeout(debounceTimer);
    controller?.abort();
  });

  return {
    value,
    loading,
    error,
    answeredQuery,
    settledQuery,
    flush,
    run,
    lastFetchAt: () => lastFetchAt,
    atOnce: () => {
      now = true;
    },
  };
}
