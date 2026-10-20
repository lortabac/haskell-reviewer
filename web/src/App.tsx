import { Route, Router } from "@solidjs/router";
import type { JSX } from "solid-js";
import { basePath } from "./base";
import { Frame } from "./Frame";
import { Home } from "./Home";
import { NotFound } from "./NotFound";

export function App(): JSX.Element {
  return (
    <Router base={basePath(window.location.pathname)} root={Frame}>
      <Route path="/" component={Home} />
      <Route path="*" component={NotFound} />
    </Router>
  );
}
