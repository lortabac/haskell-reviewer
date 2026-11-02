import type { JSX, ParentProps } from "solid-js";
import { ConnectionNotice } from "./events/ConnectionNotice";

// The page frame (SPEC §4.1): header, sidebar, main view.
// No router hooks here, so tests can render it alone. The sidebar,
// header controls (the theme switch) and the project notices come in
// as props.
export function Frame(
  props: ParentProps<{
    sidebar?: JSX.Element;
    search?: JSX.Element;
    controls?: JSX.Element;
    notices?: JSX.Element;
  }>,
): JSX.Element {
  // Move focus by hand: following "#main" only scrolls, and the router
  // would also put the hash in the URL. A native listener runs before
  // the router's one on document.
  const skip = (event: MouseEvent): void => {
    event.preventDefault();
    document.getElementById("main")?.focus();
  };
  return (
    <div class="frame">
      <a class="skip-link" href="#main" on:click={skip}>
        Skip to main content
      </a>
      <header class="frame-header">
        <span class="app-name">haskell-reviewer</span>
        {props.search}
        {props.controls}
        <ConnectionNotice />
      </header>
      <nav class="frame-sidebar" aria-label="Project">
        <div class="frame-sidebar-content">{props.sidebar}</div>
      </nav>
      <main id="main" class="frame-main" tabindex="-1">
        {props.notices}
        {props.children}
      </main>
    </div>
  );
}
