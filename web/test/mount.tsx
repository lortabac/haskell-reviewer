import { render } from "solid-js/web";
import { App } from "../src/App";

let dispose: (() => void) | undefined;

// Mounts the whole app at a URL, like a page load.
export function mountAppAt(path: string): HTMLElement {
  window.history.replaceState(null, "", path);
  const root = document.createElement("div");
  document.body.append(root);
  dispose = render(() => <App />, root);
  return root;
}

export function unmountApp(): void {
  dispose?.();
  dispose = undefined;
  document.body.replaceChildren();
}
