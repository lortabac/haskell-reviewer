// Docs / Source in the header (SPEC §4.1, D-298). A view this page
// has none of is muted text, said "(not available)". A plain click
// goes to the same place in the other view, like 1 and 2; the href
// itself is the page's top (for a new tab). A plain click on the
// current one does nothing.
import { useResolvedPath } from "@solidjs/router";
import { For, type JSX, Show } from "solid-js";
import { withBase } from "../files/paths";
import { useKeys, type ViewLinks } from "./KeysProvider";

const views = [
  { to: "docs", label: "Docs" },
  { to: "source", label: "Source" },
] as const;

export function ViewSwitch(): JSX.Element {
  const keys = useKeys();
  return (
    <Show when={keys?.links()}>
      {(l) => (
        <nav class="view-switch" aria-label="View">
          <For each={views}>{(v) => <ViewItem links={l()} {...v} />}</For>
        </nav>
      )}
    </Show>
  );
}

function ViewItem(props: {
  links: ViewLinks;
  to: "docs" | "source";
  label: string;
}): JSX.Element {
  const keys = useKeys();
  const base = useResolvedPath(() => "/");
  const current = () => props.links.current === props.to;
  const onClick = (e: MouseEvent) => {
    if (e.button !== 0) return;
    if (e.ctrlKey || e.metaKey || e.shiftKey || e.altKey) return;
    // Native: before the router's own click handler. The current view:
    // nothing (no jump to the page's top).
    e.preventDefault();
    if (!current()) keys?.goView(props.to);
  };
  return (
    <Show
      when={props.links[props.to]()}
      fallback={
        <span class="view-missing">
          {props.label}
          <span class="sr-only"> (not available)</span>
        </span>
      }
    >
      {(href) => (
        <a
          class="view-link"
          href={withBase(base(), href())}
          aria-current={current() ? "page" : undefined}
          on:click={onClick}
        >
          {props.label}
        </a>
      )}
    </Show>
  );
}
