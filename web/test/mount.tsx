import { render } from "solid-js/web";
import type { Prefs } from "../src/api/prefs";
import { App } from "../src/App";

let dispose: (() => void) | undefined;

// Mounts the whole app at a URL, like a page load. initialPrefs: the
// built-in defaults unless a test needs something else (App's own
// default, no fetch).
export function mountAppAt(path: string, initialPrefs?: Prefs): HTMLElement {
  window.history.replaceState(null, "", path);
  const root = document.createElement("div");
  document.body.append(root);
  dispose = render(() => <App initialPrefs={initialPrefs} />, root);
  return root;
}

export function unmountApp(): void {
  dispose?.();
  dispose = undefined;
  document.body.replaceChildren();
}
