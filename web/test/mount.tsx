import type { Accessor } from "solid-js";
import { render } from "solid-js/web";
import type { Indexing } from "../src/api/events";
import type { Prefs } from "../src/api/prefs";
import { App } from "../src/App";
import { IndexingContext } from "../src/events/indexing";

let dispose: (() => void) | undefined;

// Mounts the whole app at a URL, like a page load. initialPrefs: the
// built-in defaults unless a test needs something else (App's own
// default, no fetch). indexing: the indexing events, as main.tsx
// gives them.
export function mountAppAt(
  path: string,
  initialPrefs?: Prefs,
  indexing: Accessor<Indexing | undefined> = () => undefined,
): HTMLElement {
  window.history.replaceState(null, "", path);
  const root = document.createElement("div");
  document.body.append(root);
  dispose = render(
    () => (
      <IndexingContext.Provider value={{ indexing }}>
        <App initialPrefs={initialPrefs} />
      </IndexingContext.Provider>
    ),
    root,
  );
  return root;
}

export function unmountApp(): void {
  dispose?.();
  dispose = undefined;
  document.body.replaceChildren();
}
