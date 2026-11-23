import { Route, Router, type RouteSectionProps } from "@solidjs/router";
import { type JSX, untrack } from "solid-js";
import { basePath } from "./base";
import type { Prefs } from "./api/prefs";
import { browseSession, type Session } from "./api/session";
import { DiffView } from "./diff/DiffView";
import { DocsView } from "./docs/DocsView";
import { FileView } from "./files/FileView";
import { Home } from "./Home";
import { MarkdownView } from "./md/MarkdownView";
import { NotFound } from "./NotFound";
import { RefsView } from "./refs/RefsView";
import { Root } from "./Root";
import { SessionContext } from "./session";
import { TextSearchView } from "./textsearch/TextSearchView";

// The routes, and nothing else: the one place outside the URL modules
// that writes side URLs by hand (eslint.config.js).
//
// initialPrefs: main.tsx's GET api/prefs, resolved before render (so
// the theme is right from the first paint). Tests that mount App
// directly get the built-in defaults instead, with no fetch.
// pendingPrefs: that same GET, not raced against the 500 ms fallback
// (C3): when it resolves, it fills in whatever keys the user hasn't
// touched yet.
// session: main.tsx's GET api/session, awaited before render. Browse
// mode by default, for tests.
export function App(props: {
  initialPrefs?: Prefs;
  pendingPrefs?: Promise<Prefs>;
  session?: Session;
}): JSX.Element {
  const root = (p: RouteSectionProps) => (
    <Root
      {...p}
      initialPrefs={props.initialPrefs}
      pendingPrefs={props.pendingPrefs}
    />
  );
  // Fixed for the process: read once.
  const session = untrack(() => props.session) ?? browseSession;
  return (
    <SessionContext.Provider value={session}>
      <Router base={basePath(window.location.pathname)} root={root}>
        <Route path="/" component={Home} />
        <Route path="/docs/:side/*ref" component={DocsView} />
        <Route path="/refs/:side/*ref" component={RefsView} />
        <Route path="/src/:side/*path" component={FileView} />
        <Route path="/md/:side/*path" component={MarkdownView} />
        <Route path="/diff/*path" component={DiffView} />
        <Route path="/search/:side" component={TextSearchView} />
        <Route path="*" component={NotFound} />
      </Router>
    </SessionContext.Provider>
  );
}
