import { Route, Router } from "@solidjs/router";
import { createSignal } from "solid-js";
import { render } from "solid-js/web";
import { afterEach, describe, expect, it, vi } from "vitest";
import type { Target } from "../api/source";
import type { Cursor } from "./CodeView";
import type { Highlight, Seg } from "./highlight";
import { RefsBar } from "./RefsBar";

// Line 1: "f ys = g ys 🙂ys": ys binds at cp 3, used at cp 10 and 14 (after
// an astral character). Line 2 is an outside name.
const text = ["f ys = g ys 🙂ys", "other"];
const lineText = (n: number) => text[n - 1] ?? "";
const targets: Target[] = [
  ["local", 1, 3],
  ["decl", "M", "M.hs", 1, "val:g", [], 1, null, false],
  ["external", "Data.Char", null, "listed", 1, "toUpper"],
];
const seg = (from: number, to: number, link: number): Seg => ({
  from,
  to,
  cls: null,
  link,
});
const runs: Seg[][] = [
  [],
  [seg(3, 5, 0), seg(8, 9, 2), seg(10, 12, 1), seg(14, 16, 1)],
  [seg(1, 6, 3)],
];
// A new use before the one under the cursor.
const runs2: Seg[][] = [
  [],
  [seg(3, 5, 0), seg(6, 7, 1), seg(8, 9, 2), seg(10, 12, 1), seg(14, 16, 1)],
  runs[2]!,
];

let dispose: (() => void) | undefined;
afterEach(() => {
  dispose?.();
  dispose = undefined;
  document.body.replaceChildren();
});

function mount() {
  const [cursor, setCursor] = createSignal<Cursor>({
    line: 1,
    col: 9,
    link: true,
  });
  const [hl, setHl] = createSignal({ runs } as unknown as Highlight);
  const focusAt = vi.fn();
  const onHits = vi.fn();
  let close: (() => void) | undefined;
  const root = document.createElement("div");
  document.body.append(root);
  dispose = render(
    () => (
      <Router>
        <Route
          path="*"
          component={() => (
            <RefsBar
              hl={hl}
              targets={() => targets}
              lineText={lineText}
              api={() => ({ cursor, focusAt }) as never}
              onHits={onHits}
              handle={(h) => (close = h.closeQuietly)}
            />
          )}
        />
      </Router>
    ),
    root,
  );
  const q = (s: string) => root.querySelector<HTMLElement>(s)!;
  const button = (name: string) =>
    [...root.querySelectorAll("button")].find((b) => b.textContent === name)!;
  const last = () =>
    onHits.mock.calls.at(-1)?.[0] as
      { current: number; matches: { count: number } } | undefined;
  return {
    q,
    button,
    focusAt,
    onHits,
    last,
    setCursor,
    setHl,
    close: () => close?.(),
    refs: () => q(".code-refs-button"),
    bar: () => q(".code-refs-bar"),
    said: () => q(".code-refs-status").textContent,
    note: () => q(".code-refs-note").textContent,
  };
}

describe("RefsBar", () => {
  it("opens on the hit under the cursor; Next and Previous wrap", () => {
    const m = mount();
    expect(m.refs().textContent).toBe("References to ys");
    expect(m.refs().getAttribute("aria-expanded")).toBeNull();
    expect(m.bar().hidden).toBe(true);
    m.refs().click();
    expect(m.bar().hidden).toBe(false);
    expect(m.said()).toBe("Reference 2 of 3, line 1");
    expect(m.last()?.current).toBe(1);
    expect(document.activeElement).toBe(m.button("Next"));
    m.button("Next").click();
    expect(m.last()?.current).toBe(2);
    m.button("Next").click();
    expect(m.last()?.current).toBe(0);
    expect(m.said().replace(/\u00a0$/, "")).toBe(
      "Reference 1 of 3, line 1, the binder",
    );
    m.button("Previous").click();
    expect(m.last()?.current).toBe(2);
  });

  it("Esc focuses the hit and closes; closeQuietly doesn't focus", () => {
    const m = mount();
    m.refs().click();
    m.button("Next").click();
    m.button("Next").click();
    m.bar().dispatchEvent(
      new KeyboardEvent("keydown", { key: "Escape", bubbles: true }),
    );
    expect(m.focusAt).toHaveBeenCalledWith(1, 2);
    expect(m.bar().hidden).toBe(true);
    expect(m.last()).toBeUndefined();
    m.focusAt.mockClear();
    m.refs().click();
    m.close();
    expect(m.bar().hidden).toBe(true);
    expect(m.focusAt).not.toHaveBeenCalled();
    expect(m.last()).toBeUndefined();
  });

  it("a note closes an open bar", () => {
    const m = mount();
    m.refs().click();
    expect(m.bar().hidden).toBe(false);
    m.setCursor({ line: 2, col: 1, link: true });
    m.refs().click();
    expect(m.bar().hidden).toBe(true);
    expect(m.note()).toBe("References need a name declared in this project.");
    m.setCursor({ line: 1, col: 4, link: false });
    m.refs().click();
    expect(m.note()).toBe("Put the cursor on a name first.");
  });

  it("a new highlight keeps the current hit by its place", () => {
    const m = mount();
    m.refs().click();
    expect(m.last()?.current).toBe(1);
    m.setHl({ runs: runs2 } as unknown as Highlight);
    expect(m.last()?.matches.count).toBe(4);
    expect(m.last()?.current).toBe(2);
  });
});
