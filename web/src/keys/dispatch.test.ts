import { afterEach, beforeEach, describe, expect, it, vi } from "vitest";
import { createDispatcher, isEditable } from "./dispatch";
import type { ActionId } from "./keymap";

let on = true;
let dialog = false;
let ran: ActionId[] = [];
let handler: EventListener;

// tinykeys wants a code too.
const codes: Record<string, string> = {
  "/": "Slash",
  "?": "Slash",
  "1": "Digit1",
  "!": "Digit1",
  Escape: "Escape",
  Shift: "ShiftLeft",
};

function press(
  key: string,
  opts: KeyboardEventInit = {},
  target: EventTarget = document.body,
): KeyboardEvent {
  const e = new KeyboardEvent("keydown", {
    key,
    code: codes[key] ?? `Key${key.toUpperCase()}`,
    bubbles: true,
    cancelable: true,
    ...opts,
  });
  target.dispatchEvent(e);
  return e;
}

beforeEach(() => {
  vi.useFakeTimers();
  on = true;
  dialog = false;
  ran = [];
  handler = createDispatcher({
    enabled: () => on,
    dialogOpen: () => dialog,
    run: (id) => {
      ran.push(id);
      return true;
    },
  });
  document.addEventListener("keydown", handler);
});

afterEach(() => {
  document.removeEventListener("keydown", handler);
  document.body.replaceChildren();
  vi.useRealTimers();
});

describe("createDispatcher", () => {
  it("single keys fire and stop the default", () => {
    const e = press("j");
    expect(e.defaultPrevented).toBe(true);
    press("k");
    press("s");
    press("f");
    press("/");
    press("1");
    press("Escape");
    expect(ran).toEqual([
      "next",
      "prev",
      "source",
      "find",
      "search",
      "docsView",
      "escape",
    ]);
  });

  it("keys with no binding do nothing", () => {
    const e = press("z");
    press("Enter");
    expect(ran).toEqual([]);
    expect(e.defaultPrevented).toBe(false);
  });

  it("g d and g r, within the timeout", () => {
    press("g");
    expect(ran).toEqual([]);
    press("d");
    press("g");
    vi.advanceTimersByTime(900);
    press("r");
    expect(ran).toEqual(["definition", "references"]);
  });

  it("g d r: only g d (the other half-done sequence is dropped)", () => {
    press("g");
    press("d");
    press("r");
    expect(ran).toEqual(["definition"]);
  });

  it("g then a pause: the sequence is gone", () => {
    press("g");
    vi.advanceTimersByTime(1100);
    press("d");
    expect(ran).toEqual([]);
  });

  it("g then another key: that key, and no sequence", () => {
    press("g");
    press("j");
    press("d");
    expect(ran).toEqual(["next"]);
  });

  it("Shift: allowed on ? / 1, not on letters", () => {
    press("?", { shiftKey: true });
    press("/", { shiftKey: true });
    press("1", { shiftKey: true });
    press("J", { shiftKey: true });
    expect(ran).toEqual(["help", "search", "docsView"]);
  });

  it("Shift held between g and d keeps the sequence", () => {
    press("g");
    press("Shift", { shiftKey: true });
    press("d");
    expect(ran).toEqual(["definition"]);
  });

  it("Ctrl, Alt, Meta: never", () => {
    press("j", { ctrlKey: true });
    press("j", { altKey: true });
    press("j", { metaKey: true });
    press("/", { ctrlKey: true });
    expect(ran).toEqual([]);
  });

  it("not in text fields", () => {
    const input = document.createElement("input");
    const area = document.createElement("textarea");
    const box = document.createElement("div");
    box.setAttribute("role", "combobox");
    document.body.append(input, area, box);
    press("j", {}, input);
    press("/", {}, area);
    press("?", {}, box);
    press("Escape", {}, input);
    expect(ran).toEqual([]);
  });

  it("on a checkbox or a button: yes", () => {
    const check = document.createElement("input");
    check.type = "checkbox";
    const button = document.createElement("button");
    document.body.append(check, button);
    press("j", {}, check);
    press("k", {}, button);
    expect(ran).toEqual(["next", "prev"]);
  });

  it("composing: never", () => {
    press("j", { isComposing: true });
    press("j", { keyCode: 229 });
    expect(ran).toEqual([]);
  });

  it("off, or with the overlay open: nothing", () => {
    on = false;
    press("j");
    press("?", { shiftKey: true });
    on = true;
    dialog = true;
    press("j");
    expect(ran).toEqual([]);
  });

  it("a key a component took (preventDefault): left alone", () => {
    const button = document.createElement("button");
    document.body.append(button);
    button.addEventListener("keydown", (e) => e.preventDefault());
    press("j", {}, button);
    expect(ran).toEqual([]);
  });

  it("held down: only j and k repeat", () => {
    press("j", { repeat: true });
    press("k", { repeat: true });
    press("s", { repeat: true });
    press("Escape", { repeat: true });
    expect(ran).toEqual(["next", "prev"]);
  });

  it("nothing done: the default stays", () => {
    document.removeEventListener("keydown", handler);
    handler = createDispatcher({
      enabled: () => true,
      dialogOpen: () => false,
      run: () => false,
    });
    document.addEventListener("keydown", handler);
    expect(press("Escape").defaultPrevented).toBe(false);
  });
});

describe("isEditable", () => {
  it("text inputs, not other inputs", () => {
    const make = (type: string) => {
      const i = document.createElement("input");
      i.type = type;
      return i;
    };
    expect(isEditable(make("text"))).toBe(true);
    expect(isEditable(make("search"))).toBe(true);
    expect(isEditable(make("checkbox"))).toBe(false);
    expect(isEditable(make("radio"))).toBe(false);
    expect(isEditable(document.createElement("select"))).toBe(true);
    expect(isEditable(document.createElement("a"))).toBe(false);
    expect(isEditable(null)).toBe(false);
  });

  it("contenteditable and text roles", () => {
    const d = document.createElement("div");
    d.contentEditable = "true";
    document.body.append(d);
    expect(isEditable(d)).toBe(true);
    const s = document.createElement("div");
    s.setAttribute("role", "searchbox");
    expect(isEditable(s)).toBe(true);
  });
});
