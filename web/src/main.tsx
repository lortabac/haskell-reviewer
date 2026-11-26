import { createSignal } from "solid-js";
import { render } from "solid-js/web";
import { getJson } from "./api/client";
import { decodePrefs, defaultPrefs, type Prefs } from "./api/prefs";
import { decodeSession } from "./api/session";
import { App } from "./App";
import { connectEvents } from "./events/connect";
import { ConnectionContext } from "./events/ConnectionNotice";
import { createGenerationState, GenerationContext } from "./events/generation";
import { createIndexingState, IndexingContext } from "./events/indexing";
import type { Status } from "./events/protocol";
import { applyTheme } from "./prefs/theme";
import { installTips } from "./ui/tips";
import "./styles/tokens.css";
import "./styles/frame.css";
import "./styles/files.css";
import "./styles/home.css";
import "./styles/code.css";
import "./styles/diff.css";
import "./styles/modules.css";
import "./styles/docs.css";
import "./styles/search.css";
import "./styles/refs.css";
import "./styles/keys.css";
import "./styles/copy.css";

installTips();

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
// GET api/session at the same time, awaited, not raced: the mode
// decides every URL's side, and there is no safe default (D-332).
const sessionAnswer = getJson("/api/session", decodeSession);
const initialPrefs = await Promise.race([fetched, timeout]);
applyTheme(document.documentElement, initialPrefs.theme);
const session = await sessionAnswer;

// The event stream starts here, not in App, so App tests need no
// EventSource.
const [status, setStatus] = createSignal<Status>("connecting");
const [fallback, setFallback] = createSignal(false);
const indexing = createIndexingState();
const generation = createGenerationState();

render(
  () =>
    session.kind === "ok" ? (
      <ConnectionContext.Provider value={{ status, fallback }}>
        <IndexingContext.Provider value={indexing.state}>
          <GenerationContext.Provider value={generation}>
            <App
              initialPrefs={initialPrefs}
              pendingPrefs={fetched}
              session={session.value}
            />
          </GenerationContext.Provider>
        </IndexingContext.Provider>
      </ConnectionContext.Provider>
    ) : (
      <main>
        <p role="alert">
          Couldn't load the session:{" "}
          {session.kind === "error" ? session.message : "not found"}. Reload the
          page.
        </p>
      </main>
    ),
  root,
);

// Connect after the first render, in a later task: the notices' live
// region must be on the page before text goes in, or screen readers
// don't announce it.
// No session, no app: nothing to tell.
if (session.kind === "ok") {
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
        else if (msg.type === "generation") {
          // A live one is a reload even with no baseline yet.
          generation.seen(msg.generation, msg.replay !== true);
        } else if (msg.type === "indexing") {
          // After a reconnect no generation event comes: the indexing
          // events carry it (D-354).
          generation.seen(msg.indexing.generation);
          indexing.set(msg.indexing);
        }
      },
    );
  }, 0);
}
