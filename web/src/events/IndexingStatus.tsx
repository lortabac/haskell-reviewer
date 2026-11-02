// Indexing progress in the header (SPEC §10.2): plain text, not a
// progress bar. The "n of total" line is shown only once it's been
// running for a second (createDelay), so a fast index never flashes
// it; a failure line ("k module(s) couldn't be indexed") shows as
// soon as the run is done, delay or not (D-211, human choice 1A), and
// stays while this generation is current. The completion sentence is
// announced once per run that was actually shown, in an always-mounted
// sr-only status region (the SearchBox/D-209 pattern: kept apart from
// the visible text, so the fast-changing count never reaches a screen
// reader tick by tick). A new generation mid-run (rare: a reindex
// starting before the last one finished) doesn't restart the 1 s
// delay on purpose — `loading` only changes value at done/not-done
// boundaries, not on every tick or generation bump.
import {
  createEffect,
  createMemo,
  createSignal,
  type JSX,
  useContext,
} from "solid-js";
import { createDelay } from "../ui/focus";
import { ConnectionContext } from "./ConnectionNotice";
import { IndexingContext } from "./indexing";

function plural(n: number, word: string): string {
  return n === 1 ? word : `${word}s`;
}

export function IndexingStatus(): JSX.Element {
  const conn = useContext(ConnectionContext);
  const idx = useContext(IndexingContext);
  const ended = (): boolean => conn.status() === "ended";

  const done = createMemo(() => {
    const ix = idx.indexing();
    return ix !== undefined && ix.ready + ix.failed >= ix.total;
  });
  // A memo, not a plain function: it only changes value at the start
  // and the end of a run, so a progress tick partway through never
  // restarts the delay below (createDelay's effect tracks whatever it
  // reads, and a memo only notifies when its own value changes).
  const loading = createMemo(() => idx.indexing() !== undefined && !done());
  const slow = createDelay(loading, 1000);

  // The generation that got far enough to show the "n of total" line,
  // and the one already announced done: both keyed by generation, so
  // a later run (a reindex) starts over on its own, with no flag to
  // reset by hand. shownGeneration only gates the announcement now
  // (C2): the failure line itself shows for any done run, shown or not.
  let shownGeneration: number | undefined;
  let announcedGeneration: number | undefined;
  createEffect(() => {
    if (slow()) shownGeneration = idx.indexing()?.generation;
  });

  // Once done, a persistent line whenever something failed (human
  // choice 1A, SPEC §10.2) — even a run that finished inside the first
  // second, so a small project's failures are never silently hidden
  // (C2: this used to require shownGeneration too).
  const persistent = createMemo(() => {
    const ix = idx.indexing();
    if (ix === undefined || !done() || ended()) return false;
    return ix.failed > 0;
  });

  const text = (): string => {
    if (ended()) return "";
    const ix = idx.indexing();
    if (ix === undefined) return "";
    if (loading()) {
      if (!slow()) return "";
      const n = ix.ready + ix.failed;
      const suffix = ix.failed > 0 ? ` (${ix.failed} failed)` : "";
      return `Indexing ${n} of ${ix.total} ${plural(ix.total, "module")}${suffix}`;
    }
    if (persistent()) {
      return `${ix.failed} ${plural(ix.failed, "module")} couldn't be indexed`;
    }
    return "";
  };

  const [announced, setAnnounced] = createSignal("");
  // A run starting (loading turns true) clears the last sentence, so
  // a second shown run that ends the same way ("Indexing done.") still
  // changes the DOM and gets re-announced (C1: setting the same string
  // twice is not a mutation a screen reader notices).
  createEffect(() => {
    if (loading()) setAnnounced("");
  });
  createEffect(() => {
    const ix = idx.indexing();
    if (ix === undefined || !done() || ended()) return;
    if (shownGeneration !== ix.generation) return; // never shown: say nothing
    if (announcedGeneration === ix.generation) return; // said already
    announcedGeneration = ix.generation;
    setAnnounced(
      ix.failed === 0
        ? "Indexing done."
        : `Indexing done; ${ix.failed} ${plural(ix.failed, "module")} couldn't be indexed.`,
    );
  });

  return (
    <>
      <p class="indexing-live sr-only" role="status">
        {announced()}
      </p>
      <p class="indexing-status">{text()}</p>
    </>
  );
}
