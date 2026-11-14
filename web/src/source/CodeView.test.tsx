import { createEffect, createRoot, createSignal, untrack } from "solid-js";
import { render } from "solid-js/web";
import { afterEach, beforeEach, describe, expect, it, vi } from "vitest";
import { webkitFocus } from "../../test/focus";
import { fakeLayout } from "../../test/layout";
import { plainPieces } from "./display";
import { createFinder } from "./find";
import { lineIndex } from "./lines";
import type { LinkInfo } from "./links";
import { lineNodes } from "./paint";
import {
  type CodeApi,
  CodeView,
  type CodeViewProps,
  type FindHits,
  fallbackRowHeight as h,
  type LineDecor,
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
    expect(row?.querySelector(".code-cell")?.textContent).toBe("a");
    // Number, marker column, code.
    expect(row?.querySelectorAll('[role="gridcell"]')).toHaveLength(2);
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
    const { root, api } = mount(lines(20000));
    api.setCursor(20);
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

  it("only the cursor's line is focusable", () => {
    const { root, api } = mount(lines(10));
    expect(cell(root, 1).getAttribute("tabindex")).toBe("0");
    expect(cell(root, 2).hasAttribute("tabindex")).toBe(false);
    api.setCursor(2);
    expect(cell(root, 1).hasAttribute("tabindex")).toBe(false);
    expect(cell(root, 2).getAttribute("tabindex")).toBe("0");
  });

  it("focusLine from an effect waits for the tabindex", () => {
    const { root, api } = mount(lines(10));
    const done = createRoot((d) => {
      createEffect(() => api.focusLine(5));
      return d;
    });
    expect(document.activeElement).toBe(cell(root, 5));
    done();
  });

  // A mousedown on a row (on the cell, or on el); the browser focuses
  // the view. Then the selection.
  function press(
    root: HTMLElement,
    n: number,
    sel: () => void,
    opts: { button?: number; on?: Element } = {},
  ): void {
    (opts.on ?? cell(root, n)).dispatchEvent(
      new MouseEvent("mousedown", { bubbles: true, button: opts.button ?? 0 }),
    );
    view(root).focus();
    sel();
  }
  // The mouseup (on the document: drags can end outside), then the task
  // after it.
  const release = () =>
    document.dispatchEvent(new MouseEvent("mouseup", { bubbles: true }));
  async function mouseUp(): Promise<void> {
    release();
    await new Promise((done) => setTimeout(done, 0));
  }
  async function click(
    root: HTMLElement,
    n: number,
    sel: () => void,
    opts: { button?: number; on?: Element } = {},
  ): Promise<void> {
    press(root, n, sel, opts);
    await mouseUp();
  }
  const textAt = (root: HTMLElement, n: number) =>
    cell(root, n).firstChild as Node;
  const key = (key: string) =>
    document.activeElement?.dispatchEvent(
      new KeyboardEvent("keydown", { key, bubbles: true }),
    );
  const caret = (t: Node, i: number) => () =>
    getSelection()?.setBaseAndExtent(t, i, t, i);

  it("a click moves the cursor there and focuses the line", async () => {
    const { root, api } = mount(lines(10));
    await click(root, 6, caret(textAt(root, 6), 3));
    expect(api.cursor()).toEqual({ line: 6, col: 3, link: false });
    expect(document.activeElement).toBe(cell(root, 6));
    // Keys work from there.
    key("ArrowDown");
    expect(document.activeElement).toBe(cell(root, 7));
  });

  it("a key right after a click is not undone by its late check", async () => {
    const { root } = mount(lines(10));
    press(root, 6, caret(textAt(root, 6), 0));
    release();
    expect(document.activeElement).toBe(cell(root, 6));
    key("ArrowDown");
    await new Promise((done) => setTimeout(done, 0));
    expect(document.activeElement).toBe(cell(root, 7));
  });

  it("a click in the gutter: that line, column 0", async () => {
    const { root, api } = mount(lines(10));
    const num = root.querySelector('.code-row[data-line="3"] .code-num');
    if (num === null) throw new Error("no gutter");
    await click(root, 3, () => getSelection()?.removeAllRanges(), { on: num });
    expect(api.cursor()).toEqual({ line: 3, col: 0, link: false });
    expect(document.activeElement).toBe(cell(root, 3));
  });

  it("a right or middle click moves the cursor too", async () => {
    const { root, api } = mount(lines(10));
    await click(root, 6, caret(textAt(root, 6), 2), { button: 2 });
    expect(api.cursor()).toEqual({ line: 6, col: 2, link: false });
    expect(document.activeElement).toBe(cell(root, 6));
    await click(root, 8, caret(textAt(root, 8), 0), { button: 1 });
    expect(document.activeElement).toBe(cell(root, 8));
  });

  it("a drag: the cursor at the selection's end, which WebKit keeps", async () => {
    webkitFocus();
    const { root, api } = mount(lines(10));
    const a = textAt(root, 2);
    // The end of row 5: the line's end.
    const b = cell(root, 5).parentElement as Node;
    await click(root, 2, () =>
      getSelection()?.setBaseAndExtent(a, 1, b, b.childNodes.length),
    );
    expect(api.cursor()).toEqual({
      line: 5,
      col: "line 5".length,
      link: false,
    });
    expect(document.activeElement).toBe(cell(root, 5));
    const sel = getSelection();
    expect([sel?.anchorNode, sel?.anchorOffset]).toEqual([a, 1]);
    expect(sel?.focusNode).toBe(b);
    // An arrow keeps it too.
    key("ArrowDown");
    expect(document.activeElement).toBe(cell(root, 6));
    expect(getSelection()?.anchorNode).toBe(a);
    expect(getSelection()?.focusNode).toBe(b);
  });

  it("a triple click: the cursor on that line, not the next", async () => {
    const { root, api } = mount(lines(10));
    const sel = () =>
      getSelection()?.setBaseAndExtent(textAt(root, 4), 0, textAt(root, 5), 0);
    await click(root, 4, sel);
    expect(api.cursor()).toEqual({
      line: 4,
      col: "line 4".length,
      link: false,
    });
    // Backwards to a line's start: that line.
    const back = () =>
      getSelection()?.setBaseAndExtent(textAt(root, 8), 2, textAt(root, 6), 0);
    await click(root, 8, back);
    expect(api.cursor()).toEqual({ line: 6, col: 0, link: false });
  });

  it("focus taken elsewhere: no cursor move", async () => {
    const { root, api } = mount(lines(10));
    const input = document.createElement("input");
    document.body.append(input);
    await click(root, 4, () => input.focus());
    expect(api.cursor().line).toBe(1);
    expect(document.activeElement).toBe(input);
  });

  it("a mouseup with no mousedown on a row does nothing", async () => {
    const { root, api } = mount(lines(10));
    press(root, 4, caret(textAt(root, 4), 0));
    // A mousedown on the view, not on a row (the scrollbar).
    view(root).dispatchEvent(new MouseEvent("mousedown", { bubbles: true }));
    await mouseUp();
    expect(api.cursor().line).toBe(1);
  });

  it("an API move before the click's late check wins over it", async () => {
    for (const move of ["focusLine", "focusAt", "setCursor"] as const) {
      const { root, api } = mount(lines(10));
      press(root, 4, caret(textAt(root, 4), 0));
      release();
      // The late check (a task later) hasn't run yet.
      if (move === "focusLine") api.focusLine(7);
      else if (move === "focusAt") api.focusAt(7, 0);
      else api.setCursor(7);
      await new Promise((done) => setTimeout(done, 0));
      expect(api.cursor().line).toBe(7);
      dispose?.();
      dispose = undefined;
      document.body.replaceChildren();
    }
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
    webkitFocus();
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

  it("a left click on a line ends Ctrl+A at once; scrollbar, right click don't", async () => {
    const text = lines(20000);
    const { root, api } = mount(text);
    const key = (key: string, ctrlKey = false) =>
      document.activeElement?.dispatchEvent(
        new KeyboardEvent("keydown", { key, ctrlKey, bubbles: true }),
      );
    const down = (el: Element, button = 0) =>
      el.dispatchEvent(new MouseEvent("mousedown", { bubbles: true, button }));
    const up = async () => {
      document.dispatchEvent(new MouseEvent("mouseup", { bubbles: true }));
      await new Promise((done) => setTimeout(done, 0));
    };
    cell(root, 1).focus();
    key("a", true);
    // The scrollbar (the view, no row) and a right click keep it, and
    // the select-all end is not a line to go to.
    down(view(root));
    await up();
    down(cell(root, 5), 2);
    await up();
    expect(copy().text).toBe(text);
    expect(api.cursor().line).toBe(5);
    // A left click: off before any selectionchange.
    down(cell(root, 7));
    expect(copy().text).not.toBe(text);
    await up();
    expect(api.cursor().line).toBe(7);
  });

  it("clearSelection (Esc): select-all ends at once, a copy gets nothing", () => {
    const text = "ab\ncd\n";
    const { root, api } = mount(text);
    webkitFocus();
    cell(root, 1).focus();
    document.activeElement?.dispatchEvent(
      new KeyboardEvent("keydown", { key: "a", ctrlKey: true, bubbles: true }),
    );
    expect(copy().text).toBe(text);
    expect(api.clearSelection()).toBe(true);
    // No selectionchange yet: select-all must be off anyway.
    expect(copy().text).not.toBe(text);
    expect(api.clearSelection()).toBe(false);
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

describe("highlighting and decor", () => {
  // Each word of a line in a span: a new renderLine.
  const words = (_n: number, text: string): Node[] =>
    text.split(/(?= )/).map((w) => {
      const s = document.createElement("span");
      s.className = "hl-keyword";
      s.textContent = w;
      return s;
    });

  function mountHl(text: string) {
    const root = document.createElement("div");
    document.body.append(root);
    const [renderFn, setRender] = createSignal<
      ((n: number, t: string) => Node[]) | undefined
    >(undefined);
    const [decor, setDecor] = createSignal<
      ((n: number) => LineDecor | undefined) | undefined
    >(undefined);
    let api: CodeApi | undefined;
    dispose = render(
      () => (
        <CodeView
          text={text}
          index={lineIndex(text)}
          renderLine={renderFn()}
          lineDecor={decor()}
          ref={(a) => (api = a)}
        />
      ),
      root,
    );
    if (api === undefined) throw new Error("no api");
    return { root, api, setRender, setDecor };
  }

  function copied(): string | undefined {
    const data = new DataTransfer();
    const e = new ClipboardEvent("copy", {
      clipboardData: data,
      cancelable: true,
      bubbles: true,
    });
    document.dispatchEvent(e);
    return e.defaultPrevented ? data.getData("text/plain") : undefined;
  }

  it("a new renderLine redraws the mounted rows; plain again without", () => {
    const { root, setRender } = mountHl("one two\nthree\n");
    expect(cell(root, 1).querySelector("span")).toBeNull();
    setRender(() => words);
    expect(
      [...cell(root, 1).querySelectorAll(".hl-keyword")].map(
        (s) => s.textContent,
      ),
    ).toEqual(["one", " two"]);
    expect(cell(root, 1).textContent).toBe("one two");
    setRender(undefined);
    expect(cell(root, 1).querySelector("span")).toBeNull();
    expect(cell(root, 1).textContent).toBe("one two");
  });

  it("the selection and focus stay, copy is the same", () => {
    const { root, api, setRender } = mountHl("one two\nthree four\nfive\n");
    api.focusLine(2);
    const c = cell(root, 2);
    expect(document.activeElement).toBe(c);
    // From "two" (line 1, col 4) to "fo|ur" (line 2, col 8).
    getSelection()?.setBaseAndExtent(
      cell(root, 1).firstChild!,
      4,
      c.firstChild!,
      8,
    );
    const before = copied();
    expect(before).toBe("two\nthree fo");
    const shown = getSelection()?.toString();
    setRender(() => words);
    expect(c.querySelector("span")).not.toBeNull();
    expect(document.activeElement).toBe(c);
    expect(getSelection()?.toString()).toBe(shown);
    expect(copied()).toBe(before);
    // The focus end is in the new spans.
    expect(c.contains(getSelection()?.focusNode ?? null)).toBe(true);
  });

  it("decor: CPP kind as a row class and label, a marker", () => {
    const { root, setDecor } = mountHl("a\nb\nc\n");
    const row = (n: number) => cell(root, n).parentElement!;
    const label = (n: number) =>
      row(n).querySelector(".code-num")?.getAttribute("aria-label");
    setDecor(() => (n: number): LineDecor | undefined => {
      if (n === 1) return { kind: "excluded" };
      if (n === 3) return { kind: "directive" };
      if (n === 2) {
        const a = document.createElement("a");
        a.href = "/docs";
        return { mark: a };
      }
      return undefined;
    });
    expect(row(1).classList.contains("code-excluded")).toBe(true);
    expect(label(1)).toBe("1, excluded by CPP");
    expect(row(2).querySelector(".code-mark a")?.getAttribute("href")).toBe(
      "/docs",
    );
    expect(label(2)).toBe("2");
    expect(label(3)).toBe("3, CPP directive");
    expect(row(3).classList.contains("code-directive")).toBe(true);
    // Markers are not text.
    expect(view(root).textContent).toBe("abc");
    setDecor(undefined);
    expect(row(1).classList.contains("code-excluded")).toBe(false);
    expect(label(1)).toBe("1");
    expect(row(2).querySelector(".code-mark a")).toBeNull();
  });
});

describe("token cursor", () => {
  // Each word "lN" is a link; other text plain.
  const linkInfo = (k: number): LinkInfo => ({
    href: `#L${k}`,
    about: "here",
    guess: false,
    external: false,
    here: true,
  });
  const linked = (_n: number, text: string): Node[] =>
    lineNodes(
      text
        .split(/(\s+)/)
        .filter((w) => w !== "")
        .map((w) => ({
          text: w,
          cls: null,
          link: /^l\d+$/.test(w) ? Number(w.slice(1)) : 0,
          ctrl: false,
        })),
      linkInfo,
    );

  function mountLinked(text: string) {
    const root = document.createElement("div");
    document.body.append(root);
    const [renderFn, setRender] = createSignal<
      ((n: number, t: string) => Node[]) | undefined
    >(linked);
    let api: CodeApi | undefined;
    dispose = render(
      () => (
        <CodeView
          text={text}
          index={lineIndex(text)}
          renderLine={renderFn()}
          ref={(a) => (api = a)}
        />
      ),
      root,
    );
    if (api === undefined) throw new Error("no api");
    return { root, api, setRender };
  }
  const press = (key: string, shiftKey = false) =>
    document.activeElement?.dispatchEvent(
      new KeyboardEvent("keydown", { key, shiftKey, bubbles: true }),
    );
  const link = (root: HTMLElement, n: number, text: string) => {
    const a = [...cell(root, n).querySelectorAll("a.code-link")].find(
      (e) => e.textContent === text,
    );
    if (!(a instanceof HTMLElement)) throw new Error(`no link ${text}`);
    return a;
  };
  const stops = (root: HTMLElement) =>
    root.querySelectorAll('.code-view [tabindex="0"]');

  it("→ and ← move between links and the cell; one Tab stop", () => {
    const { root, api } = mountLinked("ab l1 cd l2\nx\n");
    cell(root, 1).focus();
    press("ArrowRight");
    expect(document.activeElement).toBe(link(root, 1, "l1"));
    expect(api.cursor()).toEqual({ line: 1, col: 3, link: true });
    expect(stops(root)).toHaveLength(1);
    expect(cell(root, 1).getAttribute("tabindex")).toBe("-1");
    press("ArrowRight");
    expect(document.activeElement).toBe(link(root, 1, "l2"));
    expect(link(root, 1, "l1").tabIndex).toBe(-1);
    expect(stops(root)).toHaveLength(1);
    // The end: focus stays, the view scrolls.
    press("ArrowRight");
    expect(document.activeElement).toBe(link(root, 1, "l2"));
    expect(view(root).scrollLeft).toBe(40);
    press("ArrowLeft");
    press("ArrowLeft");
    expect(document.activeElement).toBe(cell(root, 1));
    expect(api.cursor()).toEqual({ line: 1, col: 3, link: false });
    expect(cell(root, 1).getAttribute("tabindex")).toBe("0");
    press("ArrowLeft");
    expect(document.activeElement).toBe(cell(root, 1));
  });

  it("↑ and ↓ keep the goal column: the link under it, else the cell", () => {
    const { root } = mountLinked("ab l1\nx\ncd l3 l3\n");
    cell(root, 1).focus();
    press("ArrowRight");
    press("ArrowDown");
    // "x" is short: the cell.
    expect(document.activeElement).toBe(cell(root, 2));
    press("ArrowDown");
    // Column 3 is in the first l3.
    expect(document.activeElement).toBe(cell(root, 3).querySelector("a"));
    press("Home");
    expect(document.activeElement).toBe(cell(root, 1));
  });

  it("PageDown to a row not mounted: the link under the goal", () => {
    const text =
      Array.from({ length: 300 }, (_, i) => `ab l${i + 1}`).join("\n") + "\n";
    const { root } = mountLinked(text);
    cell(root, 1).focus();
    press("ArrowRight");
    press("End");
    press("ArrowRight");
    expect(document.activeElement).toBe(link(root, 300, "l300"));
    press("Home");
    press("ArrowRight");
    for (let i = 0; i < 5; i++) press("PageDown");
    const n = 1 + 5 * (Math.floor(600 / h) - 1);
    expect(document.activeElement).toBe(link(root, n, `l${n}`));
  });

  it("a redraw keeps focus on the link, or the cell if it went", () => {
    const { root, api, setRender } = mountLinked("ab l1\n");
    cell(root, 1).focus();
    press("ArrowRight");
    const old = link(root, 1, "l1");
    setRender(() => (n: number, t: string) => linked(n, t));
    expect(old.isConnected).toBe(false);
    expect(document.activeElement).toBe(link(root, 1, "l1"));
    expect(link(root, 1, "l1").tabIndex).toBe(0);
    setRender(undefined);
    expect(document.activeElement).toBe(cell(root, 1));
    expect(api.cursor().link).toBe(false);
    expect(cell(root, 1).getAttribute("tabindex")).toBe("0");
  });

  it("a click in a link focuses it; focusAt lands on a link", async () => {
    const { root, api } = mountLinked("ab l1\ncd l2\n");
    const t = link(root, 2, "l2").firstChild as Node;
    cell(root, 2).dispatchEvent(new MouseEvent("mousedown", { bubbles: true }));
    view(root).focus();
    getSelection()?.setBaseAndExtent(t, 1, t, 1);
    document.dispatchEvent(new MouseEvent("mouseup", { bubbles: true }));
    await new Promise((done) => setTimeout(done, 0));
    expect(document.activeElement).toBe(link(root, 2, "l2"));
    expect(api.cursor()).toEqual({ line: 2, col: 3, link: true });
    api.focusAt(1, 4);
    expect(document.activeElement).toBe(link(root, 1, "l1"));
    api.focusAt(1, 1);
    expect(document.activeElement).toBe(cell(root, 1));
  });

  it("a click on a link with a selection doesn't follow it", () => {
    const { root } = mountLinked("ab l1\n");
    const a = link(root, 1, "l1");
    const t = cell(root, 1).firstChild as Node;
    getSelection()?.setBaseAndExtent(t, 0, a.firstChild as Node, 1);
    const click = new MouseEvent("click", {
      bubbles: true,
      cancelable: true,
      detail: 1,
    });
    a.dispatchEvent(click);
    expect(click.defaultPrevented).toBe(true);
    // Enter (detail 0) follows it.
    const enter = new MouseEvent("click", { bubbles: true, cancelable: true });
    a.dispatchEvent(enter);
    expect(enter.defaultPrevented).toBe(false);
  });

  it("Ctrl or Cmd+click, or a selection outside the view: the browser's", () => {
    const { root } = mountLinked("ab l1\n");
    const a = link(root, 1, "l1");
    const t = cell(root, 1).firstChild as Node;
    getSelection()?.setBaseAndExtent(t, 0, a.firstChild as Node, 1);
    for (const mod of [
      { ctrlKey: true },
      { metaKey: true },
      { altKey: true },
    ]) {
      const click = new MouseEvent("click", {
        bubbles: true,
        cancelable: true,
        detail: 1,
        ...mod,
      });
      a.dispatchEvent(click);
      expect(click.defaultPrevented).toBe(false);
    }
    const p = document.createElement("p");
    p.textContent = "elsewhere";
    document.body.append(p);
    getSelection()?.setBaseAndExtent(
      p.firstChild as Node,
      0,
      p.firstChild as Node,
      4,
    );
    const click = new MouseEvent("click", {
      bubbles: true,
      cancelable: true,
      detail: 1,
    });
    a.dispatchEvent(click);
    expect(click.defaultPrevented).toBe(false);
  });

  it("a click past the middle of a link's last character: that link", async () => {
    const { root, api } = mountLinked("ab l1 cd\n");
    const a = link(root, 1, "l1");
    const t = a.firstChild as Text;
    a.dispatchEvent(new MouseEvent("mousedown", { bubbles: true }));
    view(root).focus();
    // The caret at the link's end, as the browser puts it there.
    getSelection()?.setBaseAndExtent(t, t.length, t, t.length);
    document.dispatchEvent(new MouseEvent("mouseup", { bubbles: true }));
    await new Promise((done) => setTimeout(done, 0));
    expect(document.activeElement).toBe(a);
    expect(api.cursor()).toEqual({ line: 1, col: 3, link: true });
  });

  it("a link drag ends when the window loses focus", async () => {
    const { root } = mountLinked(
      Array.from({ length: 300 }, (_, i) => `ab l${i + 1}`).join("\n") + "\n",
    );
    const a = link(root, 2, "l2");
    const t = a.firstChild as Node;
    // happy-dom has no layout: the point is always in l2.
    Object.defineProperty(document, "caretPositionFromPoint", {
      value: () => ({ offsetNode: t, offset: 1 }),
      configurable: true,
    });
    try {
      a.dispatchEvent(
        new MouseEvent("mousedown", { bubbles: true, button: 0, detail: 1 }),
      );
      // Below the view (its box is empty here): it scrolls.
      document.dispatchEvent(
        new MouseEvent("mousemove", { bubbles: true, buttons: 1, clientY: 50 }),
      );
      const v = view(root);
      await vi.waitFor(() => expect(v.scrollTop).toBeGreaterThan(0));
      window.dispatchEvent(new Event("blur"));
      const top = v.scrollTop;
      await new Promise((done) => setTimeout(done, 150));
      expect(v.scrollTop).toBe(top);
    } finally {
      Reflect.deleteProperty(document, "caretPositionFromPoint");
    }
  });

  it("focus some other way drops a focus waiting for another line", async () => {
    const text =
      Array.from({ length: 300 }, (_, i) => `ab l${i + 1}`).join("\n") + "\n";
    const { root, api } = mountLinked(text);
    const l1 = link(root, 1, "l1");
    const done = createRoot((d) => {
      createEffect(() =>
        untrack(() => {
          // Waits for its row (inside an effect)...
          api.focusLine(250);
          // ...then focus goes elsewhere in the view.
          l1.focus();
        }),
      );
      return d;
    });
    // The view scrolled to line 250: its row comes.
    view(root).dispatchEvent(new Event("scroll"));
    await vi.waitFor(() => expect(rowLines(root)).toContain(250));
    await new Promise((r) => setTimeout(r, 20));
    expect(document.activeElement).toBe(l1);
    done();
  });

  it("focus that comes some other way moves the cursor and the Tab stop", () => {
    const { root, api } = mountLinked("ab l1\ncd l2 l3\n");
    // A screen reader's browse mode, say.
    link(root, 2, "l3").focus();
    expect(api.cursor()).toEqual({ line: 2, col: 6, link: true });
    expect(stops(root)).toHaveLength(1);
    expect(link(root, 2, "l3").tabIndex).toBe(0);
    expect(cell(root, 1).hasAttribute("tabindex")).toBe(false);
    // Keys go on from there.
    press("ArrowLeft");
    expect(document.activeElement).toBe(link(root, 2, "l2"));
  });
});

describe("find", () => {
  class FakeHighlight extends Set<Range> {
    priority = 0;
  }
  let registry: Map<string, FakeHighlight>;
  beforeEach(() => {
    registry = new Map();
    vi.stubGlobal("Highlight", FakeHighlight);
    vi.stubGlobal("CSS", { highlights: registry });
  });
  afterEach(() => vi.unstubAllGlobals());

  function mountFind(
    text: string,
    find: () => FindHits | undefined,
    renderLine?: () => CodeViewProps["renderLine"],
  ): { root: HTMLElement; api: CodeApi } {
    const root = document.createElement("div");
    document.body.append(root);
    let api: CodeApi | undefined;
    dispose = render(
      () => (
        <CodeView
          text={text}
          index={lineIndex(text)}
          find={find()}
          renderLine={renderLine?.()}
          ref={(a) => (api = a)}
        />
      ),
      root,
    );
    if (api === undefined) throw new Error("no api");
    return { root, api };
  }
  const hits = (text: string, q: string, current: number): FindHits => ({
    matches: createFinder(text, lineIndex(text)).search(q),
    current,
  });
  const texts = (name: string) =>
    [...(registry.get(name) ?? [])].map((r) => r.toString());

  it("draws the hits, the current one apart", () => {
    const text = "ab ab\ncd\nab\n";
    const { root } = mountFind(text, () => hits(text, "ab", 1));
    expect(texts("code-find")).toEqual(["ab", "ab"]);
    expect(texts("code-find-current")).toEqual(["ab"]);
    expect(
      [...root.querySelectorAll(".code-find-line")].map((r) =>
        r.getAttribute("data-line"),
      ),
    ).toEqual(["1"]);
  });

  it("a range goes across a link and a glyph", () => {
    const text = "a\x01b cd\n";
    const render = (n: number, t: string): Node[] => {
      void n;
      const a = document.createElement("a");
      a.className = "code-link";
      a.textContent = "␁b";
      return [
        document.createTextNode("a"),
        a,
        document.createTextNode(t.slice(3)),
      ];
    };
    mountFind(
      text,
      () => hits(text, "\x01b c", 0),
      () => render,
    );
    expect(texts("code-find-current")).toEqual(["␁b c"]);
  });

  it("draws again after a new renderLine", () => {
    const text = "ab\n";
    const [render, setRender] = createSignal<CodeViewProps["renderLine"]>();
    // eslint-disable-next-line solid/reactivity -- mountFind reads it in JSX
    mountFind(text, () => hits(text, "ab", 0), render);
    const before = [...(registry.get("code-find-current") ?? [])][0];
    setRender(() => (_n: number, t: string) => lineNodes(plainPieces(t)));
    const after = [...(registry.get("code-find-current") ?? [])][0];
    expect(after).toBeDefined();
    expect(after).not.toBe(before);
    expect(after?.toString()).toBe("ab");
  });

  it("moves the cursor to the hit, not the focus", () => {
    const text = lines(100);
    const [find, setFind] = createSignal<FindHits>();
    // eslint-disable-next-line solid/reactivity -- mountFind reads it in JSX
    const { api } = mountFind(text, find);
    const input = document.createElement("input");
    document.body.append(input);
    input.focus();
    setFind(hits(text, "line 50", 0));
    expect(api.cursor()).toEqual({ line: 50, col: 0, link: false });
    expect(document.activeElement).toBe(input);
    input.remove();
  });

  it("clears when there are no hits", () => {
    const text = "ab\n";
    const [find, setFind] = createSignal<FindHits | undefined>(
      hits(text, "ab", 0),
    );
    // eslint-disable-next-line solid/reactivity -- mountFind reads it in JSX
    mountFind(text, find);
    expect(registry.size).toBe(2);
    setFind(undefined);
    expect(registry.size).toBe(0);
  });

  it("draws hits on rows that scroll in, later", async () => {
    const text = lines(2000);
    const { root } = mountFind(text, () => hits(text, "line", -1));
    const first = texts("code-find").length;
    expect(first).toBeGreaterThan(0);
    const v = view(root);
    v.scrollTop = 1500 * h;
    v.dispatchEvent(new Event("scroll"));
    await vi.waitFor(() => expect(rowLines(root)).toContain(1500));
    const set = registry.get("code-find");
    const rows = new Set(
      [...(set ?? [])].map((r) =>
        r.startContainer.parentElement?.closest(".code-row"),
      ),
    );
    const drawn = [...rows].map((r) => r?.getAttribute("data-line"));
    expect(drawn).toContain("1500");
  });

  it("caps the ranges of a row, but the current hit is drawn", () => {
    const text = "a ".repeat(5010) + "\n";
    mountFind(text, () => hits(text, "a", 5009));
    expect(registry.get("code-find")?.size).toBe(5000);
    expect(texts("code-find-current")).toEqual(["a"]);
    const cur = [...(registry.get("code-find-current") ?? [])][0];
    expect(cur?.startOffset).toBe(2 * 5009);
  });

  it("an outline box per rect of the current hit, none outside", () => {
    const text = "ab\ncd\n";
    const [find, setFind] = createSignal<FindHits | undefined>(
      hits(text, "ab", 0),
    );
    vi.spyOn(Range.prototype, "getClientRects").mockImplementation(
      () =>
        [
          { left: 10, top: 20, right: 10, bottom: 35, width: 0, height: 15 },
          { left: 10, top: 20, right: 30, bottom: 35, width: 20, height: 15 },
          { left: 30, top: 20, right: 40, bottom: 35, width: 10, height: 15 },
        ] as unknown as DOMRectList,
    );
    // eslint-disable-next-line solid/reactivity -- mountFind reads it in JSX
    const { root } = mountFind(text, find);
    const boxes = () => root.querySelectorAll(".code-find-box");
    expect(boxes().length).toBe(1);
    expect(
      root.querySelector(".code-find-boxes")?.getAttribute("aria-hidden"),
    ).toBe("true");
    // Not in the cells: not in a copy.
    expect(root.querySelector(".code-cell .code-find-box")).toBeNull();
    setFind(undefined);
    expect(boxes().length).toBe(0);
  });

  it("ranges go with the view, and with the hits", () => {
    const text = "ab\n";
    const [find, setFind] = createSignal<FindHits | undefined>(
      hits(text, "ab", 0),
    );
    // eslint-disable-next-line solid/reactivity -- mountFind reads it in JSX
    mountFind(text, find);
    expect(registry.size).toBe(2);
    dispose?.();
    dispose = undefined;
    expect(registry.size).toBe(0);
    void setFind;
  });
});
