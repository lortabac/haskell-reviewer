// Copy in the diff view (D-347): the copy event, Ctrl+A, Esc, y and
// Copy location. The texts themselves: copy.test.ts.
import { afterEach, beforeEach, describe, expect, it, vi } from "vitest";
import { cbor, stubFetch } from "../../test/fixtures";
import { fakeLayout } from "../../test/layout";
import { mountAppAt, unmountApp } from "../../test/mount";
import { diffSession } from "../../test/sessions";
import { defaultPrefs } from "../api/prefs";
import { selectionKeys } from "./copy";

// 21 lines a side; line 11 differs. Base is src/Old.hs (a rename).
const same = Array.from({ length: 10 }, (_, i) => `e${i + 1}`);
const after = Array.from({ length: 10 }, (_, i) => `e${i + 12}`);
const baseText = [...same, "old", ...after].join("\n") + "\n";

const file = (text: string) =>
  cbor({ generation: 1, filtered: false, kind: "text", text });

let writeText: ReturnType<typeof vi.fn>;

// added: head's added lines (one: "new"; more: an empty base cell).
function open(url: string, added = ["new"]) {
  const head = [...same, ...added, ...after].join("\n") + "\n";
  writeText = vi.fn(() => Promise.resolve());
  vi.stubGlobal("navigator", { clipboard: { writeText } });
  stubFetch({
    "/tok/api/diff/src/A.hs": [
      200,
      cbor({
        generation: 1,
        path: "src/A.hs",
        oldPath: "src/Old.hs",
        status: "renamed",
        similarity: 90,
        note: null,
        ignoreWhitespace: false,
        base: { kind: "text" },
        head: { kind: "text" },
        diff: {
          kind: "lines",
          runs: [
            ["equal", 10],
            ["delete", 1],
            ["insert", added.length],
            ["equal", 10],
          ],
          crlfLines: 0,
          bom: null,
        },
      }),
    ],
    "/tok/api/file/base/src/Old.hs": [200, file(baseText)],
    "/tok/api/file/head/src/A.hs": [200, file(head)],
    "/tok/api/source/base/src/Old.hs": [404, null],
    "/tok/api/source/head/src/A.hs": [404, null],
    "/tok/api/prefs": [200, defaultPrefs],
  });
  const root = mountAppAt(url, undefined, undefined, null, diffSession);
  const main = root.querySelector("main");
  if (main === null) throw new Error("no main");
  return main;
}

const grid = (main: HTMLElement) =>
  main.querySelector<HTMLElement>('[role="grid"]');
async function ready(main: HTMLElement): Promise<HTMLElement> {
  await vi.waitFor(() => expect(grid(main)).not.toBeNull());
  return grid(main)!;
}

// The code cell whose row (unified) or half (split) number says label.
function cellOf(main: HTMLElement, label: string): HTMLElement {
  const num = main.querySelector(`.code-num[aria-label="${label}"]`);
  const box = num?.closest(".diff-half") ?? num?.closest(".code-row");
  const cell = box?.querySelector<HTMLElement>(".code-cell");
  if (cell == null) throw new Error(`no cell ${label}`);
  return cell;
}

// The text node and offset at col in a cell.
function textAt(cell: HTMLElement, col: number): [Node, number] {
  const w = document.createTreeWalker(cell, NodeFilter.SHOW_TEXT);
  let left = col;
  for (let t = w.nextNode(); t !== null; t = w.nextNode()) {
    const len = (t as Text).length;
    if (left <= len) return [t, left];
    left -= len;
  }
  throw new Error(`no col ${col}`);
}

function select(a: [Node, number], b: [Node, number]): void {
  getSelection()?.setBaseAndExtent(a[0], a[1], b[0], b[1]);
}

function copy(target: EventTarget = document): string | undefined {
  const data = new DataTransfer();
  const e = new ClipboardEvent("copy", {
    clipboardData: data,
    cancelable: true,
    bubbles: true,
  });
  target.dispatchEvent(e);
  return e.defaultPrevented ? data.getData("text/plain") : undefined;
}

const key = (key: string, ctrlKey = false) =>
  (document.activeElement ?? document.body).dispatchEvent(
    new KeyboardEvent("keydown", {
      key,
      code: `Key${key.toUpperCase()}`,
      ctrlKey,
      bubbles: true,
      cancelable: true,
    }),
  );

// A press (pointerdown) on a cell.
function press(cell: HTMLElement): void {
  cell.dispatchEvent(
    new PointerEvent("pointerdown", { bubbles: true, button: 0 }),
  );
}

// A click on a cell: the cursor goes there, with focus.
function click(cell: HTMLElement): void {
  press(cell);
  cell.dispatchEvent(new MouseEvent("mousedown", { bubbles: true, button: 0 }));
  getSelection()?.collapse(cell, 0);
  cell.dispatchEvent(new MouseEvent("mouseup", { bubbles: true, button: 0 }));
}

const lines = (xs: string[]) => xs.map((x) => x + "\n").join("");

beforeEach(() => fakeLayout());

afterEach(() => {
  getSelection()?.removeAllRanges();
  unmountApp();
  vi.unstubAllGlobals();
  vi.restoreAllMocks();
});

describe("copy in the diff view", () => {
  it("unified: each row's own text from the files, no signs", async () => {
    const main = open("/tok/diff/src/A.hs");
    await ready(main);
    select(
      textAt(cellOf(main, "line 9, unchanged"), 1),
      textAt(cellOf(main, "added line 11"), 2),
    );
    expect(copy()).toBe("9\ne10\nold\nne");
  });

  it("unified: from the page's top, hidden gap lines included", async () => {
    const main = open("/tok/diff/src/A.hs");
    await ready(main);
    const h1 = main.querySelector("h1")!;
    select([h1.firstChild!, 0], textAt(cellOf(main, "line 8, unchanged"), 0));
    expect(copy()).toBe(lines(same.slice(0, 7)));
  });

  it("split: only the anchor's side", async () => {
    const main = open("/tok/diff/src/A.hs?layout=split");
    await ready(main);
    select(
      textAt(cellOf(main, "base line 9, unchanged"), 0),
      textAt(cellOf(main, "added line 11"), 2),
    );
    expect(copy()).toBe("e9\ne10\nold");
    select(
      textAt(cellOf(main, "line 10, unchanged"), 1),
      textAt(cellOf(main, "line 12, unchanged"), 3),
    );
    expect(copy()).toBe("10\nnew\ne12");
  });

  it("split: a press in a half makes the other side unselectable", async () => {
    const main = open("/tok/diff/src/A.hs?layout=split");
    const g = await ready(main);
    press(cellOf(main, "added line 11"));
    expect(g.classList.contains("diff-select-head")).toBe(true);
    expect(g.classList.contains("diff-select-base")).toBe(false);
    press(cellOf(main, "removed line 11"));
    expect(g.classList.contains("diff-select-base")).toBe(true);
  });

  // Row d (display rows 7-13 are shown, gaps around them).
  const rowAt = (main: HTMLElement, d: number) =>
    main.querySelector<HTMLElement>(`.code-row[data-key="${d}"]`)!;
  const in10 = (main: HTMLElement, sel: string) =>
    rowAt(main, 10).querySelector(sel)!;
  const in9 = (main: HTMLElement, sel: string) =>
    rowAt(main, 9).querySelector(sel)!;
  // Split points with no text: base's start in row 10 (line 11 col
  // 0); the end of base's half in row 9 (line 10's end); past base's
  // half in row 9 (snaps: line 10's end, or line 11's start).
  const starts = (main: HTMLElement): [string, [Node, number]][] => {
    const r10 = rowAt(main, 10);
    const rows = r10.parentElement!;
    return [
      ["row 10 start", [r10, 0]],
      ["rowgroup before row 10", [rows, [...rows.children].indexOf(r10)]],
      ["split box start", [in10(main, ".diff-split"), 0]],
      ["base half start", [in10(main, ".diff-base"), 0]],
      ["base gutter", [in10(main, ".diff-base .code-num"), 0]],
    ];
  };
  const ends = (main: HTMLElement): [string, [Node, number]][] => [
    ["base clip end", [in9(main, ".diff-base .diff-clip"), 1]],
    ["base half end", [in9(main, ".diff-base"), 2]],
  ];
  const past = (main: HTMLElement): [string, [Node, number]][] => {
    const r9 = rowAt(main, 9);
    return [
      ["row 9 end", [r9, r9.childNodes.length]],
      ["between the halves", [in9(main, ".diff-split"), 1]],
      ["split box end", [in9(main, ".diff-split"), 2]],
      ["head gutter", [in9(main, ".diff-head .code-num"), 0]],
    ];
  };

  it("split: a spot with no text maps to the line's start or end", async () => {
    const main = open("/tok/diff/src/A.hs?layout=split");
    await ready(main);
    press(cellOf(main, "removed line 11"));
    const from = () => textAt(cellOf(main, "base line 9, unchanged"), 0);
    const to = () => textAt(cellOf(main, "base line 12, unchanged"), 2);
    const location = () =>
      document.querySelector<HTMLButtonElement>(".copy-location")?.click();
    for (const [name, p] of starts(main)) {
      select(from(), p);
      expect(copy(), name).toBe("e9\ne10\n");
      location();
      expect(writeText, name).toHaveBeenLastCalledWith("src/Old.hs:9-10");
      select(p, to());
      expect(copy(), name).toBe("old\ne1");
    }
    for (const [name, p] of ends(main)) {
      select(from(), p);
      expect(copy(), name).toBe("e9\ne10");
      location();
      expect(writeText, name).toHaveBeenLastCalledWith("src/Old.hs:9-10");
      select(p, to());
      expect(copy(), name).toBe("\nold\ne1");
    }
    for (const [name, p] of past(main)) {
      select(from(), p);
      expect(copy(), name).toBe("e9\ne10");
      select(p, to());
      expect(copy(), name).toBe("old\ne1");
    }
    // Head anchored: row 9's start is before head's half (snaps to
    // line 9's end).
    press(cellOf(main, "added line 11"));
    select(textAt(cellOf(main, "line 8, unchanged"), 0), [
      in9(main, ".diff-split"),
      0,
    ]);
    expect(copy()).toBe("e8\ne9");
  });

  it("unified: a gutter is the line's start", async () => {
    const main = open("/tok/diff/src/A.hs");
    await ready(main);
    const row = cellOf(main, "line 10, unchanged").parentElement!;
    for (const g of [".code-num", ".diff-sign"]) {
      const p: [Node, number] = [row.querySelector(g)!, 0];
      select(textAt(cellOf(main, "line 9, unchanged"), 0), p);
      expect(copy(), g).toBe("e9\n");
      select(p, textAt(cellOf(main, "added line 11"), 1));
      expect(copy(), g).toBe("e10\nold\nn");
    }
  });

  for (const layout of ["", "?layout=split"]) {
    it(`the rowgroup next to a gap leaves the gap out${layout}`, async () => {
      const main = open("/tok/diff/src/A.hs" + layout);
      const g = await ready(main);
      if (layout !== "") press(cellOf(main, "removed line 11"));
      const rows = g.querySelector(".code-rows")!;
      const n = rows.children.length;
      expect(rows.lastElementChild?.classList.contains("diff-gap")).toBe(true);
      const first =
        layout === "" ? "line 8, unchanged" : "base line 8, unchanged";
      // Later end before the last gap: up to line 14, no hidden line
      // (unified: the row's start, so line 14's break is in; split:
      // snapped to line 14's end).
      select(textAt(cellOf(main, first), 0), [rows, n - 1]);
      const want =
        layout === ""
          ? "e8\ne9\ne10\nold\nnew\ne12\ne13\ne14\n"
          : "e8\ne9\ne10\nold\ne12\ne13\ne14";
      expect(copy()).toBe(want);
      // Earlier end after the last gap: none of it (unified: the last
      // break; split: nothing, so the browser's own copy).
      const probe = main.querySelector(".code-probe")!;
      select([rows, n], [probe, 0]);
      expect(copy()).toBe(layout === "" ? "\n" : undefined);
    });
  }

  it("split Ctrl+A on an empty cell: the other side's file", async () => {
    const main = open("/tok/diff/src/A.hs?layout=split", ["new", "new2"]);
    await ready(main);
    const empty = main.querySelector<HTMLElement>(
      '.code-row[data-key="11"] .code-cell[data-side="base"]',
    )!;
    click(empty);
    await vi.waitFor(() => expect(document.activeElement).toBe(empty));
    key("a", true);
    expect(copy()).toBe([...same, "new", "new2", ...after].join("\n") + "\n");
  });

  it("Ctrl+A: every row; a left click ends it; Esc too", async () => {
    const main = open("/tok/diff/src/A.hs");
    await ready(main);
    click(cellOf(main, "removed line 11"));
    await vi.waitFor(() =>
      expect(document.activeElement).toBe(cellOf(main, "removed line 11")),
    );
    key("a", true);
    const all = lines([...same, "old", "new", ...after]);
    expect(copy()).toBe(all);
    // A right click keeps it, a left one ends it at once.
    const row = cellOf(main, "line 9, unchanged");
    row.dispatchEvent(
      new MouseEvent("mousedown", { bubbles: true, button: 2 }),
    );
    expect(copy()).toBe(all);
    row.dispatchEvent(
      new MouseEvent("mousedown", { bubbles: true, button: 0 }),
    );
    expect(copy()).not.toBe(all);
    // Again, then Esc.
    cellOf(main, "removed line 11").focus();
    key("a", true);
    expect(copy()).toBe(all);
    key("Escape");
    expect(copy()).toBeUndefined();
    expect(getSelection()?.isCollapsed ?? true).toBe(true);
  });

  it("split Ctrl+A: the cursor side's file", async () => {
    const main = open("/tok/diff/src/A.hs?layout=split");
    await ready(main);
    click(cellOf(main, "removed line 11"));
    await vi.waitFor(() =>
      expect(document.activeElement).toBe(cellOf(main, "removed line 11")),
    );
    key("a", true);
    expect(copy()).toBe(baseText);
  });

  it("selectionKeys: the rows of the ends in the grid", async () => {
    const main = open("/tok/diff/src/A.hs?layout=split");
    const g = await ready(main);
    select(
      textAt(cellOf(main, "base line 9, unchanged"), 0),
      textAt(cellOf(main, "added line 11"), 1),
    );
    expect(selectionKeys(g, getSelection())).toEqual([8, 10]);
    const h1 = main.querySelector("h1")!;
    select([h1.firstChild!, 0], textAt(cellOf(main, "added line 11"), 1));
    expect(selectionKeys(g, getSelection())).toEqual([10]);
    expect(selectionKeys(g, null)).toEqual([]);
  });

  it("a copy in the find box is not ours", async () => {
    const main = open("/tok/diff/src/A.hs");
    await ready(main);
    select(
      textAt(cellOf(main, "line 9, unchanged"), 0),
      textAt(cellOf(main, "line 9, unchanged"), 2),
    );
    const input = main.querySelector<HTMLInputElement>(".code-find-bar input");
    expect(input).not.toBeNull();
    expect(copy(input!)).toBeUndefined();
    expect(copy()).toBe("e9");
  });

  it("y and Copy location: path:line on the cursor's side", async () => {
    const main = open("/tok/diff/src/A.hs");
    await ready(main);
    click(cellOf(main, "removed line 11"));
    key("y");
    expect(writeText).toHaveBeenLastCalledWith("src/Old.hs:11");
    click(cellOf(main, "added line 11"));
    key("y");
    expect(writeText).toHaveBeenLastCalledWith("src/A.hs:11");
    const button = () =>
      document.querySelector<HTMLButtonElement>(".copy-location");
    button()?.click();
    expect(writeText).toHaveBeenLastCalledWith("src/A.hs:11");
    // A selection: its lines.
    select(
      textAt(cellOf(main, "line 9, unchanged"), 1),
      textAt(cellOf(main, "line 10, unchanged"), 1),
    );
    button()?.click();
    expect(writeText).toHaveBeenLastCalledWith("src/A.hs:9-10");
  });

  it("split Copy location: the anchor's side", async () => {
    const main = open("/tok/diff/src/A.hs?layout=split");
    await ready(main);
    select(
      textAt(cellOf(main, "base line 9, unchanged"), 1),
      textAt(cellOf(main, "added line 11"), 1),
    );
    document.querySelector<HTMLButtonElement>(".copy-location")?.click();
    expect(writeText).toHaveBeenLastCalledWith("src/Old.hs:9-11");
  });
});
