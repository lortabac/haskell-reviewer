import { afterEach, describe, expect, it } from "vitest";
import { lineIndex } from "./lines";
import {
  domPoint,
  type Point,
  replaceKeepingSelection,
  textBetween,
} from "./selection";

const at = (line: number, col: number): Point => ({ kind: "at", line, col });
const before: Point = { kind: "before" };
const after: Point = { kind: "after" };

describe("textBetween", () => {
  const text = "\ufeffab\tc\r\nd\ud835\udc65e\r\nfg\n";
  const index = lineIndex(text);
  const cut = (a: Point, b: Point) => textBetween(text, index, a, b);

  it("part of one line", () => {
    expect(cut(at(1, 1), at(1, 3))).toBe("b\t");
  });

  it("across lines, CRLF kept", () => {
    expect(cut(at(1, 2), at(2, 1))).toBe("\tc\r\nd");
  });

  it("either order", () => {
    expect(cut(at(2, 1), at(1, 2))).toBe("\tc\r\nd");
  });

  it("an astral character is two columns", () => {
    expect(cut(at(2, 1), at(2, 3))).toBe("\ud835\udc65");
  });

  it("an end inside a surrogate pair takes the whole character", () => {
    expect(cut(at(2, 2), at(2, 4))).toBe("\ud835\udc65e");
    expect(cut(at(2, 0), at(2, 2))).toBe("d\ud835\udc65");
  });

  it("columns past the end are the line's end", () => {
    expect(cut(at(1, 99), at(2, 0))).toBe("\r\n");
    expect(cut(at(3, 0), at(3, Infinity))).toBe("fg");
  });

  it("before and after: the whole file, no BOM", () => {
    expect(cut(before, after)).toBe(text.slice(1));
    expect(cut(before, at(1, 2))).toBe("ab");
    expect(cut(at(3, 1), after)).toBe("g\n");
  });

  it("lines out of range are clamped", () => {
    expect(cut(at(0, 0), at(1, 1))).toBe("a");
    expect(cut(at(3, 1), at(9, 9))).toBe("g");
  });

  it("an empty selection is empty", () => {
    expect(cut(at(2, 1), at(2, 1))).toBe("");
  });
});

// A DOM shaped like CodeView's: view > .code-rows > .code-row >
// (.code-num, .code-cell). Line i is lines[i - 1], cells as given.
function viewOf(cells: (string | Node[])[]): HTMLElement {
  const view = document.createElement("div");
  const rows = document.createElement("div");
  rows.className = "code-rows";
  cells.forEach((c, i) => {
    const row = document.createElement("div");
    row.className = "code-row";
    row.setAttribute("data-line", String(i + 1));
    const num = document.createElement("div");
    num.className = "code-num";
    const cell = document.createElement("div");
    cell.className = "code-cell";
    if (typeof c === "string") cell.textContent = c;
    else cell.append(...c);
    row.append(num, cell);
    rows.append(row);
  });
  view.append(rows);
  document.body.replaceChildren(view);
  return view;
}

describe("domPoint", () => {
  const cellOf = (v: HTMLElement, n: number) => {
    const c = v.querySelector(`.code-row[data-line="${n}"] .code-cell`);
    if (c === null) throw new Error(`no line ${n}`);
    return c;
  };

  it("maps cells, gutters, rows and gaps", () => {
    const v = viewOf(["ab", "\ud835\udc65cd", "ef"]);
    const rows = v.querySelector(".code-rows");
    const row2 = v.querySelector('.code-row[data-line="2"]');
    const num2 = row2?.querySelector(".code-num");
    if (rows === null || row2 === null || num2 == null)
      throw new Error("no rows");
    const t2 = cellOf(v, 2).firstChild as Node;
    expect(domPoint(v, t2, 3)).toEqual(at(2, 3));
    expect(domPoint(v, cellOf(v, 2), 1)).toEqual(at(2, 4));
    expect(domPoint(v, num2, 0)).toEqual(at(2, 0));
    expect(domPoint(v, row2, 1)).toEqual(at(2, 0));
    expect(domPoint(v, row2, 2)).toEqual(at(2, Infinity));
    expect(domPoint(v, rows, 1)).toEqual(at(2, 0));
    expect(domPoint(v, rows, 3)).toEqual(at(3, Infinity));
    expect(domPoint(v, v, 0)).toEqual(before);
    expect(domPoint(v, v, 1)).toEqual(after);
    expect(domPoint(v, document.body, 0)).toEqual(before);
    expect(domPoint(v, document.body, 1)).toEqual(after);
  });

  it("nested spans count as text", () => {
    const span = document.createElement("span");
    span.textContent = "\u2401";
    const v = viewOf([
      [document.createTextNode("a"), span, document.createTextNode("bc")],
    ]);
    expect(domPoint(v, span.firstChild as Node, 1)).toEqual(at(1, 2));
  });
});

describe("replaceKeepingSelection", () => {
  afterEach(() => document.body.replaceChildren());

  function cells(): [HTMLElement, HTMLElement] {
    const a = document.createElement("div");
    a.textContent = "hello world";
    const b = document.createElement("div");
    b.textContent = "second";
    document.body.append(a, b);
    return [a, b];
  }
  const spans = (...texts: string[]) =>
    texts.map((t) => {
      const s = document.createElement("span");
      s.textContent = t;
      return s;
    });

  it("ends in the cell go back to the same columns", () => {
    const [a, b] = cells();
    getSelection()?.setBaseAndExtent(b.firstChild!, 2, a.firstChild!, 7);
    replaceKeepingSelection(a, spans("hello", " ", "world"));
    const sel = getSelection();
    expect(a.textContent).toBe("hello world");
    expect(sel?.anchorNode).toBe(b.firstChild);
    expect(sel?.anchorOffset).toBe(2);
    expect(sel?.focusNode).toBe(a.childNodes[2]?.firstChild);
    expect(sel?.focusOffset).toBe(1);
  });

  it("both ends in the cell, direction kept", () => {
    const [a] = cells();
    getSelection()?.setBaseAndExtent(a.firstChild!, 8, a.firstChild!, 3);
    replaceKeepingSelection(a, spans("hel", "lo wo", "rld"));
    const sel = getSelection();
    expect(sel?.toString()).toBe("lo wo");
    expect(sel?.anchorNode).toBe(a.childNodes[1]?.firstChild);
    expect(sel?.anchorOffset).toBe(5);
    expect(sel?.focusNode).toBe(a.childNodes[0]?.firstChild);
    expect(sel?.focusOffset).toBe(3);
  });

  it("no selection in the cell: just swaps", () => {
    const [a, b] = cells();
    getSelection()?.setBaseAndExtent(b.firstChild!, 1, b.firstChild!, 4);
    const range = getSelection()?.getRangeAt(0);
    replaceKeepingSelection(a, spans("x"));
    expect(a.textContent).toBe("x");
    expect(getSelection()?.getRangeAt(0)).toBe(range);
  });

  it("an empty cell: the end goes to the cell", () => {
    const [a, b] = cells();
    getSelection()?.setBaseAndExtent(b.firstChild!, 1, a.firstChild!, 4);
    replaceKeepingSelection(a, []);
    expect(getSelection()?.focusNode).toBe(a);
    expect(getSelection()?.focusOffset).toBe(0);
  });

  it("ends outside the cell that the swap moved go back (WebKit)", () => {
    const [a, b] = cells();
    const box = document.body;
    // Like select-all: both ends on a parent of the cells.
    getSelection()?.setBaseAndExtent(box, 0, box, 2);
    const swap = a.replaceChildren.bind(a);
    a.replaceChildren = (...nodes: (Node | string)[]) => {
      swap(...nodes);
      getSelection()?.setBaseAndExtent(b.firstChild!, 0, a, 0);
    };
    replaceKeepingSelection(a, spans("x"));
    const sel = getSelection();
    expect([sel?.anchorNode, sel?.anchorOffset]).toEqual([box, 0]);
    expect([sel?.focusNode, sel?.focusOffset]).toEqual([box, 2]);
  });
});
