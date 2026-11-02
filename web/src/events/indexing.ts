// The indexing progress, as the tab sees it from the event stream
// (D-210). SearchBox uses it to refresh a partial search; IndexingStatus
// renders it as the header's progress line (SPEC §10.2, D-211).
import { type Accessor, createContext } from "solid-js";
import type { Indexing } from "../api/events";

export interface IndexingState {
  // Undefined: no "indexing" event seen yet on this stream (just
  // connecting, or a reconnect that hasn't resent one).
  indexing: Accessor<Indexing | undefined>;
}

const idle: IndexingState = { indexing: () => undefined };

export const IndexingContext = createContext<IndexingState>(idle);
