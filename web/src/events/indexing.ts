// The indexing progress, as the tab sees it from the event stream
// (D-210), per side (D-333). Search, refs and the refetch loop follow
// the page's side; IndexingStatus shows the sum as the header's
// progress line (SPEC §10.2, D-211).
import { createContext, createSignal } from "solid-js";
import type { Indexing } from "../api/events";
import { sides, type Side } from "../sides";

// Progress summed over sides: Indexing with no side.
export type Progress = Omit<Indexing, "side">;

export interface IndexingState {
  // The latest event of a side. Undefined: none seen yet on this
  // stream (just connecting, or a reconnect that hasn't resent one).
  of(side: Side): Indexing | undefined;
  // Every side seen at the newest generation, summed: done only when
  // each one is. Undefined: no side seen.
  summed(): Progress | undefined;
}

const idle: IndexingState = { of: () => undefined, summed: () => undefined };

export const IndexingContext = createContext<IndexingState>(idle);

// The sum of these events, as IndexingState.summed. Only the sides at
// the newest generation count: after a reload, an old side's state is
// moot (D-354).
export function sumIndexing(seen: readonly Indexing[]): Progress | undefined {
  if (seen.length === 0) return undefined;
  const newest = Math.max(...seen.map((ix) => ix.generation));
  const sum = { generation: newest, total: 0, ready: 0, failed: 0 };
  for (const ix of seen) {
    if (ix.generation !== newest) continue;
    sum.total += ix.total;
    sum.ready += ix.ready;
    sum.failed += ix.failed;
  }
  return sum;
}

// One signal per side, so an event on base doesn't wake head's
// readers. set: give it an event.
export function createIndexingState(): {
  state: IndexingState;
  set: (ix: Indexing) => void;
} {
  const one = () => {
    const [get, set] = createSignal<Indexing>();
    return { get, set };
  };
  const bySide: Record<Side, ReturnType<typeof one>> = {
    tree: one(),
    base: one(),
    head: one(),
  };
  const of = (s: Side) => bySide[s].get();
  return {
    state: {
      of,
      summed: () => sumIndexing(sides.flatMap((s) => of(s) ?? [])),
    },
    set: (ix) => bySide[ix.side].set(ix),
  };
}
