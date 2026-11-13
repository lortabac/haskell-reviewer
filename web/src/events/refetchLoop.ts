// Ask again while an answer is partial (D-286): on indexing events, at
// most once a second, plus once at done; nothing for another snapshot.
// Shared by the source view and the docs page.
import {
  createEffect,
  createSignal,
  onCleanup,
  untrack,
  useContext,
} from "solid-js";
import { IndexingContext } from "./indexing";
import { refetchStep } from "./partialRefetch";

// What an answer said, for the loop.
export interface Answered {
  generation: number;
  partial: boolean;
}

// get: one fetch. take: gets only the answer of the last fetch; says
// its generation and partial, undefined to drop it (the loop goes on
// as before), "stop" to stop for good. first: a first answer got some
// other way (no fetch for it here). Returns: fetch now.
export function createRefetchLoop<R>(
  get: (signal: AbortSignal) => Promise<R>,
  take: (r: R) => Answered | "stop" | undefined,
  first?: Answered,
): () => void {
  const indexing = useContext(IndexingContext);
  // Bumped on each fetch: a late answer of an earlier one is dropped.
  let seq = 0;
  let ac: AbortController | undefined;
  let lastAt = first === undefined ? 0 : Date.now();
  let answerGen = first?.generation;
  // A signal: the effect below looks again when it turns on.
  const [partial, setPartial] = createSignal(first?.partial ?? false);
  let stopped = false;
  // The refetch at done is done once, until partial changes.
  let doneAsked = false;
  const fetchNow = () => {
    if (stopped) return;
    ac?.abort();
    const mine = ++seq;
    const signal = (ac = new AbortController()).signal;
    lastAt = Date.now();
    void get(signal).then((r) => {
      if (mine !== seq || stopped) return;
      // Not in a batch: take may draw and look at the page after.
      const a = take(r);
      if (a === undefined) return;
      if (a === "stop") {
        stopped = true;
        return;
      }
      answerGen = a.generation;
      if (a.partial !== untrack(partial)) doneAsked = false;
      setPartial(a.partial);
    });
  };
  onCleanup(() => {
    seq++;
    ac?.abort();
  });
  createEffect(() => {
    const p = indexing.indexing();
    if (p === undefined || !partial() || stopped) return;
    const step = refetchStep(p, answerGen, lastAt, Date.now());
    if (step === "due") fetchNow();
    else if (step === "done" && !doneAsked) {
      doneAsked = true;
      fetchNow();
    }
  });
  return fetchNow;
}
