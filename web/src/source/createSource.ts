import {
  batch,
  createEffect,
  createMemo,
  createSignal,
  onCleanup,
  untrack,
  useContext,
} from "solid-js";
import { getCbor } from "../api/client";
import { decodeSourceView, type LexedSource, sameLexed } from "../api/source";
import { IndexingContext } from "../events/indexing";
import { refetchStep } from "../events/partialRefetch";
import { apiSourceUrl } from "../files/paths";

// Highlighting, at the same time as the text. Never waited for: the
// text shows plain first (the server can take a while when indexing).
// Any failure: plain, no message. While the answer is partial, asked
// again on indexing events (D-286): at most once a second, plus once
// when indexing is done. A failed or plain answer keeps the last good
// one; an equal one changes nothing (no redraw). An answer or event
// for another snapshot is dropped (stale until a reload).
// generation: the text's, undefined until known. enabled: nothing is
// asked until it is true (the docs page waits for a first "Show
// source").
export function createSource(
  path: () => string,
  generation: () => number | undefined,
  enabled: () => boolean = () => true,
  // Every lexed answer for this text, even an equal one (the memo
  // below hides those).
  onAnswer?: (s: LexedSource) => void,
): () => LexedSource | undefined {
  const indexing = useContext(IndexingContext);
  const [latest, setLatest] = createSignal<LexedSource>();
  const [partial, setPartial] = createSignal(false);
  // Bumped on each fetch: a late answer of an earlier one is dropped.
  let seq = 0;
  let ac: AbortController | undefined;
  let lastAt = 0;
  let answerGen: number | undefined;
  // The refetch at done is done once, until partial changes.
  let doneAsked = false;
  const fetchSource = () => {
    ac?.abort();
    const mine = ++seq;
    const signal = (ac = new AbortController()).signal;
    lastAt = Date.now();
    void getCbor(apiSourceUrl(untrack(path)), decodeSourceView, {
      signal,
    }).then((r) => {
      if (mine !== seq || r.kind !== "ok") return;
      const s = r.value;
      const g = untrack(generation);
      if (g !== undefined && s.generation !== g) return;
      answerGen = s.generation;
      if (s.partial !== untrack(partial)) doneAsked = false;
      batch(() => {
        setPartial(s.partial);
        if (s.highlight === "lexed") {
          setLatest(s);
          onAnswer?.(s);
        }
      });
    });
  };
  let started = false;
  const start = () => {
    if (started) return;
    started = true;
    fetchSource();
  };
  if (untrack(enabled)) start();
  else {
    createEffect(() => {
      if (enabled()) untrack(start);
    });
  }
  onCleanup(() => {
    seq++;
    ac?.abort();
  });
  createEffect(() => {
    const p = indexing.indexing();
    if (p === undefined || !partial()) return;
    const step = refetchStep(p, answerGen, lastAt, Date.now());
    if (step === "due") fetchSource();
    else if (step === "done" && !doneAsked) {
      doneAsked = true;
      fetchSource();
    }
  });
  // Only a lexed answer for this very text (same generation).
  return createMemo(
    (): LexedSource | undefined => {
      const g = generation();
      const s = latest();
      return g !== undefined && s !== undefined && s.generation === g
        ? s
        : undefined;
    },
    undefined,
    { equals: sameLexed },
  );
}
