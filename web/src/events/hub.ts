import type { Status, WorkerMessage } from "./protocol";

// What we use of EventSource. Tests pass a fake.
export interface SourceLike {
  readonly readyState: number;
  addEventListener(type: string, listener: () => void): void;
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
  source.addEventListener("hello", () => broadcast({ type: "hello" }));
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
    },
  };
}
