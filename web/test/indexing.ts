import type { Accessor } from "solid-js";
import type { Indexing } from "../src/api/events";
import { type IndexingState, sumIndexing } from "../src/events/indexing";

// The state for one stream of events, whatever their side: each side
// sees the latest event if it is its own.
export function singleIndexing(
  latest: Accessor<Indexing | undefined>,
): IndexingState {
  return {
    of: (side) => {
      const ix = latest();
      return ix?.side === side ? ix : undefined;
    },
    summed: () => {
      const ix = latest();
      return ix === undefined ? undefined : sumIndexing([ix]);
    },
  };
}
