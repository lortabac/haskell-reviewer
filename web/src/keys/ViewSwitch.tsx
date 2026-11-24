// Docs / Source in the header, and Diff in diff mode (SPEC §4.1, D-298,
// D-348). A view this page has none of is muted text, said "(not
// available)". A plain click goes to the same place in the other view,
// like 1, 2 and 3; the href itself is the page's top (for a new tab).
// A plain click on the current one does nothing.
import { useResolvedPath } from "@solidjs/router";
import { For, type JSX, Show } from "solid-js";
import { withBase } from "../files/paths";
import { useSession } from "../session";
import { useKeys, type ToView, type ViewLinks } from "./KeysProvider";
import { plainClick } from "./plainClick";

interface Item {
  to: ToView;
  label: string;
}

const views: Item[] = [
  { to: "docs", label: "Docs" },
  { to: "source", label: "Source" },
];

// Diff only in diff mode.
const diffView: Item = { to: "diff", label: "Diff" };

export function ViewSwitch(): JSX.Element {
  const keys = useKeys();
  const session = useSession();
  const list = () => (session.mode === "diff" ? [...views, diffView] : views);
  return (
    <Show when={keys?.links()}>
      {(l) => (
        <nav class="view-switch" aria-label="View">
          <For each={list()}>{(v) => <ViewItem links={l()} {...v} />}</For>
        </nav>
      )}
    </Show>
  );
}

function ViewItem(props: {
  links: ViewLinks;
  to: ToView;
  label: string;
}): JSX.Element {
  const keys = useKeys();
  const base = useResolvedPath(() => "/");
  const current = () => props.links.current === props.to;
  // Native: before the router's own click handler. The current view:
  // nothing (no jump to the page's top).
  const onClick = (e: MouseEvent) => {
    if (plainClick(e) && !current()) keys?.goView(props.to);
  };
  return (
    <Show
      when={props.links[props.to]?.()}
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
