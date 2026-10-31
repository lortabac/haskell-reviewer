import { render } from "solid-js/web";
import { afterEach, describe, expect, it, vi } from "vitest";
import { stubFetch } from "../../test/fixtures";
import { defaultPrefs } from "../api/prefs";
import { PrefsProvider } from "./store";
import { ThemeSwitch } from "./ThemeSwitch";

let dispose: (() => void) | undefined;

function mount(): HTMLElement {
  window.history.replaceState(null, "", "/tok");
  stubFetch({});
  const root = document.createElement("div");
  document.body.append(root);
  dispose = render(
    () => (
      <PrefsProvider initial={defaultPrefs}>
        <ThemeSwitch />
      </PrefsProvider>
    ),
    root,
  );
  return root;
}

afterEach(() => {
  dispose?.();
  dispose = undefined;
  document.body.replaceChildren();
  document.documentElement.removeAttribute("data-theme");
  vi.restoreAllMocks();
});

describe("ThemeSwitch", () => {
  it("a labelled group of three buttons, System pressed by default", () => {
    const root = mount();
    const group = root.querySelector('[role="group"]');
    expect(group?.getAttribute("aria-label")).toBe("Theme");
    const buttons = [...root.querySelectorAll("button")];
    expect(buttons.map((b) => b.textContent)).toEqual([
      "System",
      "Light",
      "Dark",
    ]);
    expect(buttons.map((b) => b.getAttribute("aria-pressed"))).toEqual([
      "true",
      "false",
      "false",
    ]);
  });

  it("clicking Dark presses it, sets data-theme and unpresses the rest", () => {
    const root = mount();
    const dark = [...root.querySelectorAll("button")].find(
      (b) => b.textContent === "Dark",
    );
    dark?.click();
    expect(dark?.getAttribute("aria-pressed")).toBe("true");
    const system = [...root.querySelectorAll("button")].find(
      (b) => b.textContent === "System",
    );
    expect(system?.getAttribute("aria-pressed")).toBe("false");
    expect(document.documentElement.getAttribute("data-theme")).toBe("dark");
  });

  it("System removes the override", () => {
    const root = mount();
    const byName = (name: string) =>
      [...root.querySelectorAll("button")].find((b) => b.textContent === name);
    byName("Light")?.click();
    expect(document.documentElement.getAttribute("data-theme")).toBe("light");
    byName("System")?.click();
    expect(document.documentElement.hasAttribute("data-theme")).toBe(false);
  });
});
