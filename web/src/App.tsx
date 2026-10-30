import { Route, Router, type RouteSectionProps } from "@solidjs/router";
import type { JSX } from "solid-js";
import { basePath } from "./base";
import { DocsView } from "./docs/DocsView";
import { FileView } from "./files/FileView";
import { Frame } from "./Frame";
import { Home } from "./Home";
import { ProjectNotices } from "./modules/ProjectNotices";
import { ModulesProvider } from "./modules/store";
import { NotFound } from "./NotFound";
import { Sidebar } from "./Sidebar";

export function App(): JSX.Element {
  return (
    <Router base={basePath(window.location.pathname)} root={Root}>
      <Route path="/" component={Home} />
      <Route path="/docs/tree/*ref" component={DocsView} />
      <Route path="/src/tree/*path" component={FileView} />
      <Route path="*" component={NotFound} />
    </Router>
  );
}

function Root(props: RouteSectionProps): JSX.Element {
  return (
    <ModulesProvider>
      <Frame sidebar={<Sidebar />} notices={<ProjectNotices />}>
        {props.children}
      </Frame>
    </ModulesProvider>
  );
}
