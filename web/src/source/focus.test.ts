import { afterEach, describe, expect, it } from "vitest";
import { webkitFocus } from "../../test/focus";
import { focusKeepingSelection } from "./focus";

function setup(): { a: Text; b: Text; target: HTMLElement } {
  const p = document.createElement("p");
  p.textContent = "first";
  const q = document.createElement("p");
  q.textContent = "second";
  const target = document.createElement("div");
  target.tabIndex = 0;
  document.body.append(p, q, target);
  return { a: p.firstChild as Text, b: q.firstChild as Text, target };
}

afterEach(() => {
  document.body.replaceChildren();
});

describe("focusKeepingSelection", () => {
  it("puts back a dropped selection, same direction", () => {
    webkitFocus();
    const { a, b, target } = setup();
    getSelection()?.setBaseAndExtent(b, 3, a, 1);
    focusKeepingSelection(target);
    const sel = getSelection();
    expect(document.activeElement).toBe(target);
    expect(sel?.anchorNode).toBe(b);
    expect(sel?.anchorOffset).toBe(3);
    expect(sel?.focusNode).toBe(a);
    expect(sel?.focusOffset).toBe(1);
  });

  it("leaves a kept selection alone", () => {
    const { a, b, target } = setup();
    getSelection()?.setBaseAndExtent(a, 1, b, 2);
    const range = getSelection()?.getRangeAt(0);
    focusKeepingSelection(target);
    expect(getSelection()?.getRangeAt(0)).toBe(range);
  });

  it("an end past its node's end is cut", () => {
    webkitFocus();
    const { target } = setup();
    const box = document.createElement("div");
    box.append("x", "y", "z");
    document.body.append(box);
    getSelection()?.setBaseAndExtent(box, 0, box, 3);
    // Rows going away while focusing.
    target.addEventListener("focus", () => box.lastChild?.remove());
    focusKeepingSelection(target);
    const sel = getSelection();
    expect([sel?.anchorNode, sel?.anchorOffset]).toEqual([box, 0]);
    expect([sel?.focusNode, sel?.focusOffset]).toEqual([box, 2]);
  });

  it("no selection, or its nodes gone: nothing back", () => {
    webkitFocus();
    const { a, b, target } = setup();
    getSelection()?.removeAllRanges();
    focusKeepingSelection(target);
    expect(getSelection()?.rangeCount).toBe(0);
    getSelection()?.setBaseAndExtent(a, 1, b, 2);
    target.blur();
    target.addEventListener("focus", () => a.parentElement?.remove());
    focusKeepingSelection(target);
    expect(getSelection()?.rangeCount).toBe(0);
  });
});
