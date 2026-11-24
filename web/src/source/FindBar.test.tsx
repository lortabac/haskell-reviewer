import { render } from "solid-js/web";
import { afterEach, beforeEach, describe, expect, it, vi } from "vitest";
import type { CodeApi, FindHits } from "./CodeView";
import { createSignal } from "solid-js";
import { createFinder, type Finder, type Matches } from "./find";
import { FindBar, type FindHandle, typingDelay } from "./FindBar";
import { lineIndex } from "./lines";

const text = "foo\nbar\nFoo foo\n";
let dispose: (() => void) | undefined;
let last: FindHits | undefined;
let focused: [number, number] | undefined;
let handle: FindHandle | undefined;

const finder = createFinder(text, lineIndex(text));
let setFinder: (f: Finder) => void = () => undefined;
let cursorAt = { line: 1, col: 0 };
let status: ((m: Matches, i: number) => string) | undefined;

function mount(keyHint?: string): HTMLElement {
  const root = document.createElement("div");
  document.body.append(root);
  const [current, setCurrent] = createSignal(finder);
  setFinder = setCurrent;
  const api = {
    cursor: () => ({ ...cursorAt, link: false }),
    focusAt: (n: number, c: number) => (focused = [n, c]),
  } as unknown as CodeApi;
  dispose = render(
    () => (
      <FindBar
        finder={current}
        status={status}
        api={() => api}
        onFind={(h) => (last = h)}
        handle={(h) => (handle = h)}
        keyHint={keyHint}
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
  cursorAt = { line: 1, col: 0 };
  status = undefined;
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
  it("the handle (f, Esc): open, open again, close to the hit", () => {
    const r = mount("(f)");
    expect(q(r, ".code-hint").textContent).toBe(
      "Browser find sees only visible lines — use Find (f) to search the whole file",
    );
    handle?.open();
    expect(handle?.isOpen()).toBe(true);
    expect(document.activeElement).toBe(input(r));
    type(r, "bar");
    button(r).focus();
    handle?.open();
    expect(document.activeElement).toBe(input(r));
    expect(handle?.isOpen()).toBe(true);
    handle?.close();
    expect(handle?.isOpen()).toBe(false);
    expect(focused).toEqual([2, 0]);
  });

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

  it("a new finder: the same query again, from the cursor", () => {
    const r = mount();
    button(r).click();
    type(r, "foo");
    vi.advanceTimersByTime(typingDelay);
    const other = "x\nfoo\nfoo\nfoo\n";
    cursorAt = { line: 3, col: 0 };
    setFinder(createFinder(other, lineIndex(other)));
    expect(last?.matches.count).toBe(3);
    expect(last?.current).toBe(1);
    vi.advanceTimersByTime(typingDelay);
    expect(live(r).textContent).toBe("Match 2 of 3, line 3");
  });

  it("a new finder while closed: its hits when it opens", () => {
    const r = mount();
    button(r).click();
    type(r, "foo");
    key(input(r), "Escape");
    const other = "foo\n";
    setFinder(createFinder(other, lineIndex(other)));
    expect(last).toBeUndefined();
    button(r).click();
    expect(last?.matches.count).toBe(1);
  });

  it("uses the status it is given", () => {
    status = (m, i) => `hit ${i + 1}/${m.count}`;
    const r = mount();
    button(r).click();
    type(r, "bar");
    vi.advanceTimersByTime(typingDelay);
    expect(live(r).textContent).toBe("hit 1/1");
  });

  describe("off (a diff with no rows)", () => {
    const text2 = "foo\nfoo\n";
    const f2 = createFinder(text2, lineIndex(text2));
    // A bar with a child button, and a button outside it.
    function mountOff() {
      const [off, setOff] = createSignal(false);
      const root = document.createElement("div");
      document.body.append(root);
      const other = document.createElement("button");
      document.body.append(other);
      dispose = render(
        () => (
          <FindBar
            off={off()}
            finder={() => f2}
            api={() => ({
              cursor: () => ({ line: 1, col: 0 }),
              focusAt: (l, c) => (focused = [l, c]),
            })}
            onFind={(x) => (last = x)}
            handle={(x) => (handle = x)}
          >
            <button type="button" class="child">
              Child
            </button>
          </FindBar>
        ),
        root,
      );
      return { root, other, setOff };
    }

    it("no off: the button and the hint are shown", () => {
      const r = mount();
      expect(button(r).hasAttribute("hidden")).toBe(false);
      expect(q(r, ".code-hint").hasAttribute("hidden")).toBe(false);
    });

    it("on while open: closes quietly, focus stays elsewhere", () => {
      const { root: r, other, setOff } = mountOff();
      handle?.open();
      type(r, "foo");
      expect(last?.matches.count).toBe(2);
      other.focus();
      setOff(true);
      expect(q<HTMLElement>(r, ".code-find-bar").hidden).toBe(true);
      expect(button(r).hidden).toBe(true);
      expect(q<HTMLElement>(r, ".code-hint").hidden).toBe(true);
      expect(last).toBeUndefined();
      expect(handle?.isOpen()).toBe(false);
      expect(focused).toBeUndefined();
      expect(document.activeElement).toBe(other);
      expect(live(r).textContent).toBe("");
      setOff(false);
      expect(q<HTMLElement>(r, ".code-find-bar").hidden).toBe(true);
      expect(button(r).hidden).toBe(false);
      handle?.open();
      expect(last?.matches.count).toBe(2);
    });

    it("on with focus in the bar or on the button: to the child", () => {
      const { root: r, setOff } = mountOff();
      handle?.open();
      expect(document.activeElement).toBe(input(r));
      setOff(true);
      expect(document.activeElement).toBe(q(r, ".child"));
      setOff(false);
      button(r).focus();
      setOff(true);
      expect(document.activeElement).toBe(q(r, ".child"));
    });
  });
});
