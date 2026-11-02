import { createSignal } from "solid-js";
import { render } from "solid-js/web";
import { getJson } from "./api/client";
import type { Indexing } from "./api/events";
import { decodePrefs, defaultPrefs, type Prefs } from "./api/prefs";
import { App } from "./App";
import { connectEvents } from "./events/connect";
import { ConnectionContext } from "./events/ConnectionNotice";
import { IndexingContext } from "./events/indexing";
import type { Status } from "./events/protocol";
import { applyTheme } from "./prefs/theme";
import "./styles/tokens.css";
import "./styles/frame.css";
import "./styles/files.css";
import "./styles/modules.css";
import "./styles/docs.css";
import "./styles/search.css";

const root = document.getElementById("root");
if (root === null) {
  throw new Error("no #root element");
}

// GET api/prefs before the first render, so the theme is right from
// the first paint (no flash): built-in defaults if it's slow (500 ms)
// or fails. fetched (not raced): if the race's loser is the real GET,
// PrefsProvider applies it, for whichever keys the user hasn't
// touched by then (C3) — it's never dropped.
const fetched: Promise<Prefs> = getJson("/api/prefs", decodePrefs).then((r) =>
  r.kind === "ok" ? r.value : defaultPrefs,
);
const timeout = new Promise<Prefs>((resolve) => {
  setTimeout(() => resolve(defaultPrefs), 500);
});
const initialPrefs = await Promise.race([fetched, timeout]);
applyTheme(document.documentElement, initialPrefs.theme);

// The event stream starts here, not in App, so App tests need no
// EventSource.
const [status, setStatus] = createSignal<Status>("connecting");
const [fallback, setFallback] = createSignal(false);
const [indexing, setIndexing] = createSignal<Indexing>();

render(
  () => (
    <ConnectionContext.Provider value={{ status, fallback }}>
      <IndexingContext.Provider value={{ indexing }}>
        <App initialPrefs={initialPrefs} pendingPrefs={fetched} />
      </IndexingContext.Provider>
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
      else if (msg.type === "indexing") setIndexing(msg.indexing);
    },
  );
}, 0);
