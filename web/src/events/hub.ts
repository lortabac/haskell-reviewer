import {
  decodeGeneration,
  decodeHello,
  decodeIndexing,
  parseEventData,
} from "../api/events";
import type { Side } from "../sides";
import type { Status, WorkerMessage } from "./protocol";

// What we use of an event: the data, for named events.
export interface EventLike {
  readonly data?: unknown;
}

// What we use of EventSource. Tests pass a fake.
export interface SourceLike {
  readonly readyState: number;
  addEventListener(type: string, listener: (event: EventLike) => void): void;
  close(): void;
}

export type SourceCtor = new (url: string) => SourceLike;

// Someone that gets the events: a tab's MessagePort, or the tab itself.
export interface Listener {
  post(msg: WorkerMessage): void;
}

export interface Hub {
  add(listener: Listener): void;
}

// EventSource.CLOSED. A constant, so fakes need no statics.
const CLOSED = 2;

// How long we try to reconnect before we call the session over.
export const giveUpAfterMs = 30_000;

// One event stream, passed on to every listener. The browser reconnects
// by itself after an error. The session is over when the server says no
// (readyState CLOSED, e.g. 404 from a new run with a new token), or when
// we can't get back in within giveUpAfterMs: once the server has exited,
// connections are refused and the browser would retry forever.
//
// Listeners are never removed: a MessagePort says nothing when its tab
// goes away. Posting to a dead port does nothing, and a session has few
// tabs.
export function createHub(url: string, Source: SourceCtor): Hub {
  const listeners = new Set<Listener>();
  let status: Status = "connecting";
  // The latest indexing event of each side (D-210, D-333), replayed to
  // a listener that joins late (a new tab, or a SearchBox mounting
  // after it).
  const indexing = new Map<Side, WorkerMessage & { type: "indexing" }>();
  // The newest generation seen in any event (D-354), replayed before
  // the indexing states. 0: none yet. Older indexing states are moot:
  // dropped, never passed on.
  let newest = 0;
  // A generation event came: replay newest (else the indexing states
  // say it already).
  let sawGeneration = false;
  // A newer generation: drop the indexing states of older ones.
  const raiseNewest = (g: number): void => {
    newest = g;
    for (const [side, msg] of indexing) {
      if (msg.indexing.generation < g) indexing.delete(side);
    }
  };

  const broadcast = (msg: WorkerMessage): void => {
    for (const l of listeners) l.post(msg);
  };
  const setStatus = (next: Status): void => {
    if (next === status) return;
    status = next;
    broadcast({ type: "status", status });
  };

  let giveUp: ReturnType<typeof setTimeout> | undefined;
  const stopTimer = (): void => {
    clearTimeout(giveUp);
    giveUp = undefined;
  };
  const end = (): void => {
    stopTimer();
    source.close();
    setStatus("ended");
  };

  const source = new Source(url);
  source.addEventListener("open", () => {
    if (status === "ended") return;
    stopTimer();
    setStatus("open");
  });
  source.addEventListener("hello", (event) => {
    // A bad payload is a bug on one side: say so, don't pass it on.
    try {
      decodeHello(parseEventData(event.data));
    } catch (err) {
      console.error(err);
      return;
    }
    broadcast({ type: "hello" });
  });
  source.addEventListener("indexing", (event) => {
    let decoded;
    try {
      decoded = decodeIndexing(parseEventData(event.data));
    } catch (err) {
      console.error(err);
      return;
    }
    if (decoded.generation < newest) return;
    if (decoded.generation > newest) raiseNewest(decoded.generation);
    const msg = { type: "indexing", indexing: decoded } as const;
    indexing.set(decoded.side, msg);
    broadcast(msg);
  });
  source.addEventListener("generation", (event) => {
    let g;
    try {
      g = decodeGeneration(parseEventData(event.data)).generation;
    } catch (err) {
      console.error(err);
      return;
    }
    if (g <= newest) return;
    sawGeneration = true;
    raiseNewest(g);
    broadcast({ type: "generation", generation: g });
  });
  source.addEventListener("error", () => {
    if (status === "ended") return;
    if (source.readyState === CLOSED) {
      end();
      return;
    }
    setStatus("reconnecting");
    // The browser fires error at each failed try: start the timer once.
    giveUp ??= setTimeout(end, giveUpAfterMs);
  });

  return {
    add(listener) {
      listeners.add(listener);
      listener.post({ type: "status", status });
      // A replay is no news: it says no reload happened (D-354).
      if (sawGeneration) {
        listener.post({ type: "generation", generation: newest, replay: true });
      }
      for (const msg of indexing.values()) listener.post(msg);
    },
  };
}
