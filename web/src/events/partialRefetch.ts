// When to ask again for a partial answer, on an indexing event (D-210,
// D-286). Search and the source view both use it.
import type { Indexing } from "../api/events";

// While the last answer was partial, at most one refetch per this many
// ms, plus one more as soon as indexing is done (never throttled).
export const refetchThrottleMs = 1000;

// older: an event for an older snapshot than the answer's (ignore).
// newer: a newer snapshot (a reload). done: indexing is done. due:
// refetch now. throttled: too soon after the last fetch.
export type RefetchStep = "older" | "newer" | "done" | "throttled" | "due";

// answerGen: the snapshot generation of the last answer, undefined if
// none yet. lastAt: when the last fetch started (ms).
export function refetchStep(
  progress: Indexing,
  answerGen: number | undefined,
  lastAt: number,
  now: number,
): RefetchStep {
  if (answerGen !== undefined && progress.generation < answerGen) {
    return "older";
  }
  if (answerGen !== undefined && progress.generation > answerGen) {
    return "newer";
  }
  if (progress.ready + progress.failed >= progress.total) return "done";
  return now - lastAt < refetchThrottleMs ? "throttled" : "due";
}
