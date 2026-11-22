import { type JSX, onCleanup, type ParentProps } from "solid-js";
import { ConnectionNotice } from "./events/ConnectionNotice";
import { IndexingStatus } from "./events/IndexingStatus";
import { setVar } from "./ui/cssom";

// The page frame (SPEC §4.1): header, sidebar, main view.
// No router hooks here, so tests can render it alone. The sidebar,
// revisions, view switch, header controls (the theme switch) and the
// project notices come in as props.
export function Frame(
  props: ParentProps<{
    sidebar?: JSX.Element;
    revisions?: JSX.Element;
    views?: JSX.Element;
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
  // The sidebar's height leaves room for the header, which can wrap to
  // more lines (the revisions): measure it.
  let frame!: HTMLDivElement;
  const watchHeader = (header: HTMLElement): void => {
    // None (old browsers, tests): the CSS default stays.
    if (typeof ResizeObserver === "undefined") return;
    const ro = new ResizeObserver(() => {
      const h = header.getBoundingClientRect().height;
      if (h > 0) setVar(frame, "--header-height", `${h}px`);
    });
    ro.observe(header);
    onCleanup(() => ro.disconnect());
  };
  return (
    <div class="frame" ref={(el) => (frame = el)}>
      <a class="skip-link" href="#main" on:click={skip}>
        Skip to main content
      </a>
      <header class="frame-header" ref={watchHeader}>
        <span class="app-name">haskell-reviewer</span>
        {props.revisions}
        {props.views}
        {props.search}
        <IndexingStatus />
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
