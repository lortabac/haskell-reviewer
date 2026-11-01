import { render } from "solid-js/web";
import { afterEach, describe, expect, it } from "vitest";
import { Frame } from "./Frame";

let dispose: (() => void) | undefined;

function mount(): HTMLElement {
  const root = document.createElement("div");
  document.body.append(root);
  dispose = render(
    () => <Frame notices={<p class="note">note</p>}>hello</Frame>,
    root,
  );
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
    expect(nav?.getAttribute("aria-label")).toBe("Project");
    const main = root.querySelector("main");
    expect(main?.id).toBe("main");
    // Notices first, then the page.
    expect(main?.textContent).toBe("notehello");
    expect(main?.firstElementChild?.className).toBe("note");
  });

  it("the header: app name, then search, then controls", () => {
    const root = document.createElement("div");
    document.body.append(root);
    dispose = render(
      () => (
        <Frame
          search={<span class="s">search</span>}
          controls={<span class="c">controls</span>}
          notices={<p class="note">note</p>}
        >
          hello
        </Frame>
      ),
      root,
    );
    const children = [...(root.querySelector("header")?.children ?? [])];
    const classes = children.map((c) => c.className);
    expect(classes.indexOf("app-name")).toBeLessThan(classes.indexOf("s"));
    expect(classes.indexOf("s")).toBeLessThan(classes.indexOf("c"));
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
