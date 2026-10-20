import { basePath } from "../base";
import { createHub, type Hub, type SourceCtor } from "./hub";
import type { WorkerMessage } from "./protocol";

// What we use of the SharedWorker global scope. Declared here, so we
// need no WebWorker lib (it clashes with DOM) and no second tsconfig.
export interface PortLike {
  postMessage(msg: WorkerMessage): void;
}

export interface WorkerScope {
  location: { pathname: string };
  onconnect: ((event: { ports: readonly PortLike[] }) => void) | null;
  EventSource?: SourceCtor;
}

// The SharedWorker: one event stream for all tabs of this browser
// (SPEC §11.6.2, D-52). The worker script lives at
// /<token>/assets/<name>.js, so the token comes from its own URL.
export function startWorker(scope: WorkerScope): void {
  let hub: Hub | undefined;
  scope.onconnect = (event) => {
    const port = event.ports[0];
    if (port === undefined) return;
    const Source = scope.EventSource;
    if (Source === undefined) {
      port.postMessage({ type: "unsupported" });
      return;
    }
    hub ??= createHub(
      basePath(scope.location.pathname) + "/api/events",
      Source,
    );
    hub.add({ post: (msg) => port.postMessage(msg) });
  };
}
