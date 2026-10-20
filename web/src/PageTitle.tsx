import { createEffect, type JSX, onCleanup } from "solid-js";

const appName = "haskell-reviewer";

// Sets document.title while mounted: "<text> — haskell-reviewer". Set
// in an effect, so it runs after the old page's cleanup.
export function PageTitle(props: { text: string }): JSX.Element {
  createEffect(() => {
    document.title = `${props.text} — ${appName}`;
  });
  onCleanup(() => {
    document.title = appName;
  });
  return null;
}
