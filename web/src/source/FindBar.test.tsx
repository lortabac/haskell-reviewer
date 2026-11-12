import { render } from "solid-js/web";
import { afterEach, beforeEach, describe, expect, it, vi } from "vitest";
import type { CodeApi, FindHits } from "./CodeView";
import { FindBar, typingDelay } from "./FindBar";
import { lineIndex } from "./lines";

const text = "foo\nbar\nFoo foo\n";
let dispose: (() => void) | undefined;
let last: FindHits | undefined;
let focused: [number, number] | undefined;

function mount(): HTMLElement {
  const root = document.createElement("div");
  document.body.append(root);
  const api = {
    cursor: () => ({ line: 1, col: 0, link: false }),
    focusAt: (n: number, c: number) => (focused = [n, c]),
  } as unknown as CodeApi;
  dispose = render(
    () => (
      <FindBar
        text={text}
        index={lineIndex(text)}
        api={() => api}
        onFind={(h) => (last = h)}
      />
    ),
    root,
  );
  return root;
}

beforeEach(() => {
  vi.useFakeTimers();
  last = undefined;
  focused = undefined;
});
afterEach(() => {
  dispose?.();
  dispose = undefined;
  document.body.replaceChildren();
  vi.useRealTimers();
});

const q = <T extends Element>(r: HTMLElement, s: string) => {
  const el = r.querySelector<T>(s);
  if (el === null) throw new Error(s);
  return el;
};
const button = (r: HTMLElement) => q<HTMLButtonElement>(r, ".code-find-button");
const input = (r: HTMLElement) => q<HTMLInputElement>(r, "input");
const live = (r: HTMLElement) => q(r, '[role="status"]');
function type(r: HTMLElement, s: string): void {
  const i = input(r);
  i.value = s;
  i.dispatchEvent(new Event("input", { bubbles: true }));
}
function key(el: Element, k: string, shiftKey = false): void {
  el.dispatchEvent(
    new KeyboardEvent("keydown", { key: k, shiftKey, bubbles: true }),
  );
}

describe("FindBar", () => {
  it("is closed at first, with the hint", () => {
    const r = mount();
    expect(button(r).getAttribute("aria-expanded")).toBe("false");
    expect(q(r, ".code-find-bar").hasAttribute("hidden")).toBe(true);
    expect(q(r, ".code-hint").textContent).toContain("use Find");
    expect(last).toBeUndefined();
  });

  it("opens, focuses the input, searches", () => {
    const r = mount();
    button(r).click();
    expect(button(r).getAttribute("aria-expanded")).toBe("true");
    expect(document.activeElement).toBe(input(r));
    type(r, "foo");
    expect(last?.matches.count).toBe(3);
    expect(last?.current).toBe(0);
  });

  it("says the count after a pause, at once on Enter", () => {
    const r = mount();
    button(r).click();
    type(r, "foo");
    expect(live(r).textContent).toBe("");
    vi.advanceTimersByTime(typingDelay);
    expect(live(r).textContent).toBe("Match 1 of 3, line 1");
    key(input(r), "Enter");
    expect(last?.current).toBe(1);
    expect(live(r).textContent).toBe("Match 2 of 3, line 3");
    key(input(r), "Enter", true);
    key(input(r), "Enter", true);
    expect(last?.current).toBe(2);
    expect(document.activeElement).toBe(input(r));
  });

  it("says no matches, and nothing for an empty query", () => {
    const r = mount();
    button(r).click();
    type(r, "zzz");
    vi.advanceTimersByTime(typingDelay);
    expect(live(r).textContent).toBe("No matches");
    expect(q<HTMLButtonElement>(r, ".code-find-bar button").disabled).toBe(
      true,
    );
    type(r, "");
    vi.advanceTimersByTime(typingDelay);
    expect(live(r).textContent).toBe("");
  });

  it("Esc closes and focuses the current hit", () => {
    const r = mount();
    button(r).click();
    type(r, "foo");
    key(input(r), "Enter");
    key(input(r), "Escape");
    expect(button(r).getAttribute("aria-expanded")).toBe("false");
    expect(last).toBeUndefined();
    expect(focused).toEqual([3, 0]);
  });

  it("closing with no hits focuses the button", () => {
    const r = mount();
    button(r).click();
    type(r, "zzz");
    key(input(r), "Escape");
    expect(focused).toBeUndefined();
    expect(document.activeElement).toBe(button(r));
  });

  it("ignores Enter and Esc while composing", () => {
    const r = mount();
    button(r).click();
    type(r, "foo");
    input(r).dispatchEvent(
      new KeyboardEvent("keydown", {
        key: "Enter",
        isComposing: true,
        bubbles: true,
      }),
    );
    expect(last?.current).toBe(0);
  });

  it("the same text twice: a no-break space comes and goes", () => {
    const r = mount();
    button(r).click();
    type(r, "zzz");
    vi.advanceTimersByTime(typingDelay);
    expect(live(r).textContent).toBe("No matches");
    type(r, "zzy");
    vi.advanceTimersByTime(typingDelay);
    expect(live(r).textContent).toBe("No matches\u00a0");
    type(r, "zzx");
    vi.advanceTimersByTime(typingDelay);
    expect(live(r).textContent).toBe("No matches");
  });

  it("reopening with a kept query says the status", () => {
    const r = mount();
    button(r).click();
    type(r, "foo");
    key(input(r), "Escape");
    expect(live(r).textContent).toBe("");
    button(r).click();
    expect(live(r).textContent).toBe("Match 1 of 3, line 1");
  });

  it("closing: focus moves before the bar is hidden", () => {
    const r = mount();
    button(r).click();
    type(r, "zzz");
    const seen: boolean[] = [];
    button(r).addEventListener("focus", () =>
      seen.push(q(r, ".code-find-bar").hasAttribute("hidden")),
    );
    key(input(r), "Escape");
    expect(seen).toEqual([false]);
  });

  it("ignores Enter with keyCode 229 (Safari IME)", () => {
    const r = mount();
    button(r).click();
    type(r, "foo");
    input(r).dispatchEvent(
      new KeyboardEvent("keydown", {
        key: "Enter",
        keyCode: 229,
        bubbles: true,
      }),
    );
    expect(last?.current).toBe(0);
  });
});
