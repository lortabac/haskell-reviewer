import { render } from "solid-js/web";
import { afterEach, describe, expect, it } from "vitest";
import { Frame } from "./Frame";

let dispose: (() => void) | undefined;

function mount(): HTMLElement {
  const root = document.createElement("div");
  document.body.append(root);
  dispose = render(() => <Frame>hello</Frame>, root);
  return root;
}

afterEach(() => {
  dispose?.();
  document.body.replaceChildren();
});

describe("Frame", () => {
  it("has the landmarks", () => {
    const root = mount();
    expect(root.querySelector("header")?.textContent).toContain(
      "haskell-reviewer",
    );
    const nav = root.querySelector("nav");
    expect(nav?.getAttribute("aria-label")).toBe("Files");
    const main = root.querySelector("main");
    expect(main?.id).toBe("main");
    expect(main?.textContent).toBe("hello");
  });

  it("skip link comes first and focuses main", () => {
    const root = mount();
    const first = root.querySelector("a, button, input");
    expect(first?.getAttribute("href")).toBe("#main");
    if (!(first instanceof HTMLElement)) throw new Error("no skip link");
    first.click();
    expect(document.activeElement).toBe(root.querySelector("main"));
  });
});
