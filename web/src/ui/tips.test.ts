import { afterEach, beforeEach, describe, expect, it, vi } from "vitest";
import { installTips } from "./tips";

// happy-dom has no real :hover or :focus-visible: the test says which
// elements have them.
const hover = new Set<Element>();
const fv = new Set<Element>();
let off: () => void;
let media = true;

beforeEach(() => {
  hover.clear();
  fv.clear();
  media = true;
  // eslint-disable-next-line @typescript-eslint/unbound-method -- called with this below
  const real = Element.prototype.matches;
  vi.spyOn(Element.prototype, "matches").mockImplementation(function (
    this: Element,
    s: string,
  ) {
    if (s === ":hover") return hover.has(this);
    if (s === ":focus-visible") return fv.has(this);
    return real.call(this, s);
  });
  vi.spyOn(window, "matchMedia").mockImplementation(
    () => ({ matches: media }) as MediaQueryList,
  );
  document.body.replaceChildren();
  for (const id of ["a", "b", "c"]) {
    const el = document.createElement("button");
    el.id = id;
    if (id !== "c") el.dataset["tip"] = "Tip";
    document.body.append(el);
  }
  off = installTips();
});
afterEach(() => {
  off();
  vi.restoreAllMocks();
});

const esc = (target: EventTarget = document) => {
  const e = new KeyboardEvent("keydown", {
    key: "Escape",
    bubbles: true,
    cancelable: true,
  });
  target.dispatchEvent(e);
  return e;
};
const el = (id: string) => document.getElementById(id)!;
// Focus arrives: with a key (focus-visible) or not.
const focusIn = (id: string, visible: boolean) => {
  if (visible) fv.add(el(id));
  el(id).dispatchEvent(new Event("focusin", { bubbles: true }));
};
const isOff = (id: string) => el(id).hasAttribute("data-tip-off");

describe("tips", () => {
  it("Esc with a tip shown (focus-visible) hides it and is taken", () => {
    focusIn("a", true);
    expect(esc().defaultPrevented).toBe(true);
    expect(isOff("a")).toBe(true);
    // Hidden now: the second Esc is not ours.
    expect(esc().defaultPrevented).toBe(false);
  });

  it("hover and focus on two controls: Esc hides both", () => {
    focusIn("a", true);
    hover.add(el("b"));
    expect(esc().defaultPrevented).toBe(true);
    expect(isOff("a")).toBe(true);
    expect(isOff("b")).toBe(true);
  });

  it("mouse focus, then the key turns focus-visible on: Esc is not ours", () => {
    focusIn("a", false);
    fv.add(el("a"));
    expect(esc().defaultPrevented).toBe(false);
    // The tip is hidden all the same.
    expect(isOff("a")).toBe(true);
  });

  it("click focus, then another key: the tip shows, and Esc is ours", () => {
    focusIn("a", false);
    fv.add(el("a"));
    el("a").focus();
    document.dispatchEvent(new KeyboardEvent("keydown", { key: "x" }));
    expect(esc().defaultPrevented).toBe(true);
    expect(isOff("a")).toBe(true);
  });

  it("a node gone from the page: Esc is not ours", () => {
    const gone = el("a");
    hover.add(gone);
    gone.remove();
    expect(esc().defaultPrevented).toBe(false);
  });

  it("no hover on the device: Esc is not ours", () => {
    media = false;
    hover.add(el("a"));
    expect(esc().defaultPrevented).toBe(false);
  });

  it("Esc in an open dialog is the dialog's", () => {
    const d = document.createElement("dialog");
    d.setAttribute("open", "");
    document.body.append(d);
    focusIn("a", true);
    expect(esc(d).defaultPrevented).toBe(false);
  });

  it("Esc while composing is not ours", () => {
    focusIn("a", true);
    const e = new KeyboardEvent("keydown", {
      key: "Escape",
      isComposing: true,
      bubbles: true,
      cancelable: true,
    });
    document.dispatchEvent(e);
    expect(e.defaultPrevented).toBe(false);
  });

  it("leaving or blurring allows the tip again", () => {
    focusIn("a", true);
    esc();
    el("a").dispatchEvent(new Event("focusout", { bubbles: true }));
    expect(isOff("a")).toBe(false);
    hover.add(el("b"));
    esc();
    el("b").dispatchEvent(new Event("pointerout", { bubbles: true }));
    expect(isOff("b")).toBe(false);
  });
});
