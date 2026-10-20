import type { JSX, ParentProps } from "solid-js";
import { ConnectionNotice } from "./events/ConnectionNotice";

// The page frame (SPEC §4.1): header, sidebar, main view.
// No router hooks here, so tests can render it alone. The sidebar
// comes in as a prop.
export function Frame(
  props: ParentProps<{ sidebar?: JSX.Element }>,
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
        <ConnectionNotice />
      </header>
      <nav class="frame-sidebar" aria-label="Files">
        <div class="frame-sidebar-content">{props.sidebar}</div>
      </nav>
      <main id="main" class="frame-main" tabindex="-1">
        {props.children}
      </main>
    </div>
  );
}
