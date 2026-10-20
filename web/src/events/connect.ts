import { basePath } from "../base";
import { createHub, type SourceCtor } from "./hub";
import { decodeWorkerMessage, type WorkerMessage } from "./protocol";

// What we use of SharedWorker. Tests pass a fake.
export interface SharedWorkerLike {
  port: { onmessage: ((event: MessageEvent) => void) | null };
  onerror: ((event: ErrorEvent) => void) | null;
}

export type SharedWorkerCtor = new (
  url: URL,
  options: { name: string },
) => SharedWorkerLike;

// The browser globals we need, passed in so tests can fake them.
export interface EventsEnv {
  SharedWorker?: SharedWorkerCtor;
  EventSource?: SourceCtor;
  location: { pathname: string };
}

// Connect this tab to the event stream: through the SharedWorker if the
// browser has one and it works, else with a stream of its own (SPEC
// §11.6.2). In the second case onMessage gets {type: "unsupported"}
// first, so the tab can show the notice.
export function connectEvents(
  env: EventsEnv,
  onMessage: (msg: WorkerMessage) => void,
): void {
  let fellBack = false;
  const fallback = (): void => {
    if (fellBack) return;
    fellBack = true;
    onMessage({ type: "unsupported" });
    if (env.EventSource === undefined) return;
    const url = basePath(env.location.pathname) + "/api/events";
    createHub(url, env.EventSource).add({ post: onMessage });
  };

  // Vite finds the worker by this exact shape: `new SharedWorker(new
  // URL("...", import.meta.url), ...)`. Hence the local name.
  const SharedWorker = env.SharedWorker;
  if (SharedWorker === undefined) {
    fallback();
    return;
  }
  let worker: SharedWorkerLike;
  try {
    worker = new SharedWorker(new URL("./events.worker.ts", import.meta.url), {
      name: "haskell-reviewer-events",
    });
  } catch {
    // E.g. a security error, or workers turned off.
    fallback();
    return;
  }
  // The worker script didn't load or start.
  worker.onerror = fallback;
  worker.port.onmessage = (event) => {
    const msg = decodeWorkerMessage(event.data as unknown);
    if (msg === undefined) return;
    if (msg.type === "unsupported") fallback();
    else onMessage(msg);
  };
}
