import { createSignal } from "solid-js";
import { render } from "solid-js/web";
import { App } from "./App";
import { connectEvents } from "./events/connect";
import { ConnectionContext } from "./events/ConnectionNotice";
import type { Status } from "./events/protocol";
import "./styles/tokens.css";
import "./styles/frame.css";

const root = document.getElementById("root");
if (root === null) {
  throw new Error("no #root element");
}

// The event stream starts here, not in App, so App tests need no
// EventSource.
const [status, setStatus] = createSignal<Status>("connecting");
const [fallback, setFallback] = createSignal(false);

render(
  () => (
    <ConnectionContext.Provider value={{ status, fallback }}>
      <App />
    </ConnectionContext.Provider>
  ),
  root,
);

// Connect after the first render, in a later task: the notices' live
// region must be on the page before text goes in, or screen readers
// don't announce it.
setTimeout(() => {
  connectEvents(
    {
      SharedWorker: window.SharedWorker,
      EventSource: window.EventSource,
      location: window.location,
    },
    (msg) => {
      if (msg.type === "status") setStatus(msg.status);
      else if (msg.type === "unsupported") setFallback(true);
    },
  );
}, 0);
