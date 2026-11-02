import { Route, Router, type RouteSectionProps } from "@solidjs/router";
import type { JSX } from "solid-js";
import { basePath } from "./base";
import { defaultPrefs, type Prefs } from "./api/prefs";
import { DocsView } from "./docs/DocsView";
import { FileView } from "./files/FileView";
import { Frame } from "./Frame";
import { Home } from "./Home";
import { ProjectNotices } from "./modules/ProjectNotices";
import { ModulesProvider } from "./modules/store";
import { NotFound } from "./NotFound";
import { PrefsProvider, PrefsStatus } from "./prefs/store";
import { ThemeSwitch } from "./prefs/ThemeSwitch";
import { SearchBox } from "./search/SearchBox";
import { Sidebar } from "./Sidebar";

// initialPrefs: main.tsx's GET api/prefs, resolved before render (so
// the theme is right from the first paint). Tests that mount App
// directly get the built-in defaults instead, with no fetch.
// pendingPrefs: that same GET, not raced against the 500 ms fallback
// (C3): when it resolves, it fills in whatever keys the user hasn't
// touched yet.
export function App(props: {
  initialPrefs?: Prefs;
  pendingPrefs?: Promise<Prefs>;
}): JSX.Element {
  const root = (p: RouteSectionProps) => (
    <Root
      {...p}
      initialPrefs={props.initialPrefs}
      pendingPrefs={props.pendingPrefs}
    />
  );
  return (
    <Router base={basePath(window.location.pathname)} root={root}>
      <Route path="/" component={Home} />
      <Route path="/docs/tree/*ref" component={DocsView} />
      <Route path="/src/tree/*path" component={FileView} />
      <Route path="*" component={NotFound} />
    </Router>
  );
}

function Root(
  props: RouteSectionProps & {
    initialPrefs?: Prefs;
    pendingPrefs?: Promise<Prefs>;
  },
): JSX.Element {
  return (
    <PrefsProvider
      initial={props.initialPrefs ?? defaultPrefs}
      pending={props.pendingPrefs}
    >
      <ModulesProvider>
        <Frame
          sidebar={<Sidebar />}
          notices={<ProjectNotices />}
          search={<SearchBox placement="header" />}
          controls={
            <>
              <ThemeSwitch />
              <PrefsStatus />
            </>
          }
        >
          {props.children}
        </Frame>
      </ModulesProvider>
    </PrefsProvider>
  );
}
