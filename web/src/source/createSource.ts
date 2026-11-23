import {
  type Accessor,
  createEffect,
  createMemo,
  createSignal,
  on,
  untrack,
} from "solid-js";
import { getCbor } from "../api/client";
import { decodeSourceView, type LexedSource, sameLexed } from "../api/source";
import { createRefetchLoop } from "../events/refetchLoop";
import { apiSourceUrl } from "../files/paths";
import { usePageSide } from "../session";
import type { Side } from "../sides";

// Highlighting, at the same time as the text. Never waited for: the
// text shows plain first (the server can take a while when indexing).
// Any failure: plain, no message. While the answer is partial, asked
// again on indexing events (D-286): at most once a second, plus once
// when indexing is done. A failed or plain answer keeps the last good
// one; an equal one changes nothing (no redraw). An answer or event
// for another snapshot is dropped (stale until a reload).
// generation: the text's, undefined until known. Options: enabled,
// nothing is asked until it is true (the docs page waits for a first
// "Show source"); onAnswer, every lexed answer for this text, even an
// equal one (the memo below hides those); side, the page's unless
// given (the diff page has two).
export function createSource(
  path: () => string,
  generation: () => number | undefined,
  opts: {
    enabled?: () => boolean;
    onAnswer?: (s: LexedSource) => void;
    side?: Accessor<Side>;
  } = {},
): () => LexedSource | undefined {
  const pageSide = usePageSide();
  const side = opts.side ?? pageSide;
  const enabled = opts.enabled ?? (() => true);
  const onAnswer = opts.onAnswer;
  const [latest, setLatest] = createSignal<LexedSource>();
  const fetchSource = createRefetchLoop(
    (signal) =>
      getCbor(apiSourceUrl(side(), untrack(path)), decodeSourceView, {
        signal,
      }),
    (r) => {
      if (r.kind !== "ok") return undefined;
      const s = r.value;
      const g = untrack(generation);
      if (g !== undefined && s.generation !== g) return undefined;
      if (s.highlight === "lexed") {
        setLatest(s);
        onAnswer?.(s);
      }
      return s;
    },
    { side },
  );
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
  // A new text (another generation, the diff page refetches one): ask
  // again. Not for the first one.
  createEffect(
    on(
      generation,
      (g, prev) => {
        if (started && prev !== undefined && g !== undefined && g !== prev) {
          fetchSource();
        }
      },
      { defer: true },
    ),
  );
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
