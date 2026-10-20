import { Route, Router, type RouteSectionProps } from "@solidjs/router";
import type { JSX } from "solid-js";
import { basePath } from "./base";
import { FileView } from "./files/FileView";
import { Sidebar } from "./files/Sidebar";
import { Frame } from "./Frame";
import { Home } from "./Home";
import { NotFound } from "./NotFound";

export function App(): JSX.Element {
  return (
    <Router base={basePath(window.location.pathname)} root={Root}>
      <Route path="/" component={Home} />
      <Route path="/src/tree/*path" component={FileView} />
      <Route path="*" component={NotFound} />
    </Router>
  );
}

function Root(props: RouteSectionProps): JSX.Element {
  return <Frame sidebar={<Sidebar />}>{props.children}</Frame>;
}
