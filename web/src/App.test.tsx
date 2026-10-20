import { render } from "solid-js/web";
import { afterEach, describe, expect, it } from "vitest";
import { App } from "./App";

let dispose: (() => void) | undefined;

function mountAt(path: string): HTMLElement {
  window.history.replaceState(null, "", path);
  const root = document.createElement("div");
  document.body.append(root);
  dispose = render(() => <App />, root);
  return root;
}

afterEach(() => {
  dispose?.();
  document.body.replaceChildren();
});

describe("App routing under the token", () => {
  it.each(["/tok", "/tok/"])("%s shows Home", (path) => {
    const main = mountAt(path).querySelector("main");
    expect(main?.textContent).toContain("Nothing here yet");
  });

  it.each(["/tok/src/tree/a.hs", "/tok/changes"])(
    "%s shows NotFound",
    (path) => {
      const main = mountAt(path).querySelector("main");
      expect(main?.textContent).toContain("Not found in this snapshot");
    },
  );
});
