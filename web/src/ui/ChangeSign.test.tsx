import { render } from "solid-js/web";
import { afterEach, describe, expect, it } from "vitest";
import { ChangeLegend, ChangeSign } from "./ChangeSign";

let dispose: (() => void) | undefined;

function mount(f: () => Element): HTMLElement {
  const root = document.createElement("div");
  document.body.append(root);
  dispose = render(f, root);
  return root;
}

afterEach(() => {
  dispose?.();
  document.body.replaceChildren();
});

describe("ChangeSign", () => {
  it("a hidden sign, and a word for screen readers", () => {
    const root = mount(() => (<ChangeSign change="removed" />) as Element);
    const hidden = root.querySelector("[aria-hidden=true]");
    expect(hidden?.textContent).toBe("−");
    expect(root.querySelector(".sr-only")?.textContent).toBe("removed");
    expect(root.querySelector(".change-removed")).not.toBeNull();
  });

  it("settings: the text-style gear; a word can replace the plain one", () => {
    const root = mount(
      () =>
        (
          <span>
            <ChangeSign change="settings" />
            <ChangeSign change="renamed" word="renamed from a" />
          </span>
        ) as Element,
    );
    const words = [...root.querySelectorAll(".sr-only")].map(
      (e) => e.textContent,
    );
    expect(words).toEqual(["settings changed", "renamed from a"]);
    expect(root.querySelector("[aria-hidden=true]")?.textContent).toBe("⚙︎");
  });

  it("the legend: every sign, with its word", () => {
    const root = mount(() => (<ChangeLegend />) as Element);
    expect(root.textContent).toBe(
      "+ added · − removed · ~ modified · → renamed · ⚙︎ settings",
    );
  });
});
