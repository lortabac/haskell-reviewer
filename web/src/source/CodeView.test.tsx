import { createSignal } from "solid-js";
import { render } from "solid-js/web";
import { afterEach, beforeEach, describe, expect, it, vi } from "vitest";
import { fakeLayout } from "../../test/layout";
import { lineIndex } from "./lines";
import {
  type CodeApi,
  CodeView,
  fallbackRowHeight as h,
  type LineMarks,
} from "./CodeView";

let dispose: (() => void) | undefined;

function mount(
  text: string,
  marks?: () => LineMarks | undefined,
): { root: HTMLElement; api: CodeApi } {
  const root = document.createElement("div");
  document.body.append(root);
  let api: CodeApi | undefined;
  dispose = render(
    () => (
      <CodeView
        text={text}
        index={lineIndex(text)}
        marks={marks?.()}
        ref={(a) => (api = a)}
      />
    ),
    root,
  );
  if (api === undefined) throw new Error("no api");
  return { root, api };
}

const view = (root: HTMLElement) => {
  const el = root.querySelector<HTMLElement>(".code-view");
  if (el === null) throw new Error("no view");
  return el;
};
const rowLines = (root: HTMLElement) =>
  [...root.querySelectorAll(".code-rows > .code-row")].map((r) =>
    Number(r.getAttribute("aria-rowindex")),
  );
const cell = (root: HTMLElement, n: number) => {
  const el = root.querySelector<HTMLElement>(
    `.code-row[data-line="${n}"] .code-cell`,
  );
  if (el === null) throw new Error(`no line ${n}`);
  return el;
};
const lines = (n: number) =>
  Array.from({ length: n }, (_, i) => `line ${i + 1}`).join("\n") + "\n";

beforeEach(() => fakeLayout());

afterEach(() => {
  dispose?.();
  dispose = undefined;
  document.body.replaceChildren();
  vi.restoreAllMocks();
});

describe("CodeView", () => {
  it("is a grid with row count and row indexes", () => {
    const { root } = mount("a\nb\tc\n");
    const grid = root.querySelector('[role="grid"]');
    expect(grid?.getAttribute("aria-rowcount")).toBe("2");
    expect(grid?.getAttribute("aria-label")).toBe("File contents");
    expect(grid?.getAttribute("aria-readonly")).toBe("true");
    expect(rowLines(root)).toEqual([1, 2]);
    const row = root.querySelector('[role="row"]');
    expect(
      row?.querySelector('[role="rowheader"]')?.getAttribute("aria-label"),
    ).toBe("1");
    expect(row?.querySelector('[role="gridcell"]')?.textContent).toBe("a");
    expect(cell(root, 2).textContent).toBe("b\tc");
  });

  it("line numbers are not text", () => {
    const { root } = mount("a\nb\n");
    expect(view(root).textContent).toBe("ab");
  });

  it("mounts only the rows near the screen", () => {
    const { root } = mount(lines(20000));
    const ls = rowLines(root);
    expect(ls.length).toBeGreaterThan(20);
    expect(ls.length).toBeLessThan(100);
    expect(ls[0]).toBe(1);
  });

  it("places rows by line", () => {
    const { root } = mount(lines(50));
    const row = root.querySelector<HTMLElement>('.code-row[data-line="3"]');
    expect(row?.getAttribute("style")).toMatch(/--code-y:\s*42px/);
  });

  it("uses the measured row height", () => {
    vi.spyOn(HTMLElement.prototype, "getBoundingClientRect").mockImplementation(
      function (this: HTMLElement) {
        return this.classList.contains("code-probe")
          ? new DOMRect(0, 0, 10, 30)
          : new DOMRect();
      },
    );
    const { root } = mount(lines(1000));
    expect(root.querySelector(".code-rows")?.getAttribute("style")).toMatch(
      /--code-h:\s*30000px/,
    );
    const row = root.querySelector('.code-row[data-line="3"]');
    expect(row?.getAttribute("style")).toMatch(/--code-y:\s*60px/);
    // 600 / 30 = 20 rows on screen, plus overscan.
    expect(rowLines(root).length).toBeLessThan(32);
  });

  it("a taller row near the end keeps the top line", () => {
    let probeHeight = h;
    const observers: ResizeObserverCallback[] = [];
    vi.stubGlobal(
      "ResizeObserver",
      class {
        constructor(cb: ResizeObserverCallback) {
          observers.push(cb);
        }
        observe(): void {}
        unobserve(): void {}
        disconnect(): void {}
      },
    );
    vi.spyOn(HTMLElement.prototype, "getBoundingClientRect").mockImplementation(
      function (this: HTMLElement) {
        return this.classList.contains("code-probe")
          ? new DOMRect(0, 0, 10, probeHeight)
          : new DOMRect();
      },
    );
    const { root } = mount(lines(1000));
    const v = view(root);
    // At the bottom: line 972 at the top.
    v.scrollTop = 1000 * h - 600;
    const top = v.scrollTop / h;
    probeHeight = 30;
    for (const cb of observers) cb([], {} as ResizeObserver);
    expect(v.scrollTop / 30).toBeCloseTo(top);
    vi.unstubAllGlobals();
  });

  it("control characters show as glyphs", () => {
    const { root } = mount("a\u0007b\n");
    const c = cell(root, 1);
    expect(c.textContent).toBe("a\u2407b");
    expect(c.querySelector(".code-ctrl")?.textContent).toBe("\u2407");
  });

  it("the cursor's line is the one Tab stop, and stays mounted", () => {
    const { root, api } = mount(lines(20000));
    const stops = () =>
      [...root.querySelectorAll('.code-cell[tabindex="0"]')].map((c) =>
        c.parentElement?.getAttribute("data-line"),
      );
    expect(stops()).toEqual(["1"]);
    api.setCursor(15000);
    expect(stops()).toEqual(["15000"]);
    expect(rowLines(root)).toContain(15000);
    // Still in line order.
    const ls = rowLines(root);
    expect([...ls].sort((a, b) => a - b)).toEqual(ls);
  });

  it("a row that stays is never moved, so it keeps focus", async () => {
    const { root } = mount(lines(20000));
    const c = cell(root, 20);
    c.focus();
    const rows = root.querySelector(".code-rows");
    if (rows === null) throw new Error("no rows");
    const removed: Node[] = [];
    new MutationObserver((ms) => {
      for (const m of ms) removed.push(...m.removedNodes);
    }).observe(rows, { childList: true });
    const v = view(root);
    for (const top of [210000, 100000, 300000, 0]) {
      v.scrollTop = top;
      v.dispatchEvent(new Event("scroll"));
      await vi.waitFor(() => {
        expect(rowLines(root)).toContain(Math.floor(top / h) + 1);
      });
    }
    expect(removed).not.toContain(c.parentElement);
    expect(document.activeElement).toBe(c);
    const ls = rowLines(root);
    expect([...ls].sort((a, b) => a - b)).toEqual(ls);
  });

  it("pins keep lines mounted", () => {
    const { root, api } = mount(lines(20000));
    api.pin([9000, 12000]);
    expect(rowLines(root)).toEqual(expect.arrayContaining([9000, 12000]));
    api.pin([]);
    expect(rowLines(root)).not.toContain(9000);
  });

  it("arrows, pages, Home and End move the focused line", () => {
    const { root, api } = mount(lines(1000));
    cell(root, 1).focus();
    const key = (key: string, ctrlKey = false) =>
      document.activeElement?.dispatchEvent(
        new KeyboardEvent("keydown", { key, ctrlKey, bubbles: true }),
      );
    key("ArrowDown");
    expect(api.cursor().line).toBe(2);
    expect(document.activeElement).toBe(cell(root, 2));
    key("ArrowUp");
    key("ArrowUp");
    expect(api.cursor().line).toBe(1);
    // A page: the rows on screen less one.
    key("PageDown");
    expect(api.cursor().line).toBe(1 + Math.floor(600 / h) - 1);
    key("End", true);
    expect(api.cursor().line).toBe(1000);
    expect(document.activeElement).toBe(cell(root, 1000));
    key("ArrowDown");
    expect(api.cursor().line).toBe(1000);
    key("Home");
    expect(api.cursor().line).toBe(1);
  });

  it("a focused line becomes the cursor", () => {
    const { root, api } = mount(lines(10));
    cell(root, 4).focus();
    expect(api.cursor().line).toBe(4);
  });

  it("marks lines", () => {
    const [marks, setMarks] = createSignal<LineMarks | undefined>({
      from: 2,
      to: 3,
    });
    // eslint-disable-next-line solid/reactivity -- mount reads it in JSX
    const { root } = mount(lines(10), marks);
    const marked = () =>
      [...root.querySelectorAll(".code-line-target")].map((r) =>
        r.getAttribute("data-line"),
      );
    expect(marked()).toEqual(["2", "3"]);
    setMarks(undefined);
    expect(marked()).toEqual([]);
  });

  it("shows the Ctrl+F hint", () => {
    const { root } = mount("a\n");
    expect(root.querySelector(".code-hint")?.textContent).toBe(
      "Browser find (Ctrl+F) sees only the lines on screen.",
    );
  });
});

describe("copy", () => {
  function copy(): { text: string | undefined; native: boolean } {
    const data = new DataTransfer();
    const e = new ClipboardEvent("copy", {
      clipboardData: data,
      cancelable: true,
      bubbles: true,
    });
    document.dispatchEvent(e);
    return {
      text: e.defaultPrevented ? data.getData("text/plain") : undefined,
      native: !e.defaultPrevented,
    };
  }

  function select(a: [Node, number], b: [Node, number]): void {
    const sel = getSelection();
    sel?.setBaseAndExtent(a[0], a[1], b[0], b[1]);
  }

  const textOf = (root: HTMLElement, n: number) => {
    const t = cell(root, n).firstChild;
    if (t === null) throw new Error("no text");
    return t;
  };

  it("copies the file's text between the ends, CRLF kept", () => {
    const text = "\ufeffab\r\ncd\r\nef\r\n";
    const { root } = mount(text);
    select([textOf(root, 1), 1], [textOf(root, 3), 1]);
    expect(copy().text).toBe("b\r\ncd\r\ne");
  });

  it("reversed ends", () => {
    const { root } = mount("ab\ncd\n");
    select([textOf(root, 2), 1], [textOf(root, 1), 1]);
    expect(copy().text).toBe("b\nc");
  });

  it("lines off screen are copied too", () => {
    const text = lines(20000);
    const { root, api } = mount(text);
    api.pin([15000]);
    select([textOf(root, 2), 0], [textOf(root, 15000), 4]);
    const want = text.slice(
      text.indexOf("line 2\n"),
      text.indexOf("line 15000\n") + 4,
    );
    expect(copy().text).toBe(want);
  });

  it("from outside the view: from the file's start, no chrome", () => {
    const { root } = mount("\ufeffab\ncd\n");
    const before = document.createElement("p");
    before.textContent = "chrome";
    root.prepend(before);
    select([before.firstChild as Node, 0], [textOf(root, 2), 1]);
    expect(copy().text).toBe("ab\nc");
  });

  it("to past the view: to the file's end", () => {
    const { root } = mount("ab\ncd\n");
    const after = document.createElement("p");
    after.textContent = "chrome";
    root.append(after);
    select([textOf(root, 1), 1], [after.firstChild as Node, 2]);
    expect(copy().text).toBe("b\ncd\n");
  });

  it("all outside, or collapsed: native", () => {
    const { root } = mount("ab\n");
    const p = document.createElement("p");
    p.textContent = "chrome";
    root.prepend(p);
    select([p.firstChild as Node, 0], [p.firstChild as Node, 3]);
    expect(copy().native).toBe(true);
    select([textOf(root, 1), 1], [textOf(root, 1), 1]);
    expect(copy().native).toBe(true);
  });

  it("from before the view to after it: the whole file, no chrome", () => {
    const { root } = mount("\ufeffab\ncd\n");
    const before = document.createElement("p");
    before.textContent = "chrome";
    const after = document.createElement("p");
    after.textContent = "more";
    root.prepend(before);
    root.append(after);
    select([before.firstChild as Node, 1], [after.firstChild as Node, 2]);
    expect(copy().text).toBe("ab\ncd\n");
  });

  it("Ctrl+A in the view copies the whole file, without a BOM", async () => {
    const text = "\ufeff" + lines(20000);
    const { root } = mount(text);
    const v = view(root);
    const key = (key: string, ctrlKey = false) =>
      document.activeElement?.dispatchEvent(
        new KeyboardEvent("keydown", { key, ctrlKey, bubbles: true }),
      );
    cell(root, 1).focus();
    key("a", true);
    expect(copy().text).toBe(text.slice(1));
    // Moving and scrolling keep it.
    key("PageDown");
    key("ArrowDown");
    v.scrollTop = 9529 * h;
    v.dispatchEvent(new Event("scroll"));
    await vi.waitFor(() => {
      expect(rowLines(root)).toContain(9530);
    });
    expect(copy().text).toBe(text.slice(1));
    // The mounted rows are still the selection.
    const rows = root.querySelector(".code-rows");
    expect(getSelection()?.anchorNode).toBe(rows);
    expect(getSelection()?.focusOffset).toBe(rows?.childNodes.length);
    // Another selection ends it: a caret, then a word.
    const t = textOf(root, 9530);
    select([t, 1], [t, 1]);
    expect(copy().native).toBe(true);
    select([t, 0], [t, 2]);
    expect(copy().text).toBe("li");
  });

  it("leaves other copies alone", () => {
    const { root } = mount("ab\n");
    select([textOf(root, 1), 0], [textOf(root, 1), 2]);
    // A field outside the view.
    const input = document.createElement("input");
    document.body.append(input);
    const data = new DataTransfer();
    const e = new ClipboardEvent("copy", {
      clipboardData: data,
      cancelable: true,
      bubbles: true,
    });
    input.dispatchEvent(e);
    expect(e.defaultPrevented).toBe(false);
    // Someone else already took it.
    const taken = new ClipboardEvent("copy", {
      clipboardData: new DataTransfer(),
      cancelable: true,
    });
    taken.preventDefault();
    document.dispatchEvent(taken);
    expect(taken.clipboardData?.getData("text/plain")).toBe("");
  });
});
