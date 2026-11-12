import { createSignal } from "solid-js";
import { render } from "solid-js/web";
import { afterEach, beforeEach, describe, expect, it, vi } from "vitest";
import { fakeLayout } from "../../test/layout";
import { type CodeApi, CodeView } from "./CodeView";
import { hideDelay, showDelay } from "./createHoverCard";
import type { CardText } from "./hover";
import { lineIndex } from "./lines";
import type { LinkInfo } from "./links";
import { lineNodes } from "./paint";

let dispose: (() => void) | undefined;

const rowH = 21;
// Rows move by shiftY (a scroll); links start at linkX.
let shiftY = 0;
let linkX = 100;

// Rects for placement: rows by line, the gutter to x 40, links at
// linkX, the card 200 × 50.
function fakeRects(): void {
  vi.spyOn(HTMLElement.prototype, "getBoundingClientRect").mockImplementation(
    function (this: HTMLElement) {
      const row = this.closest(".code-row");
      const top =
        (Number(row?.getAttribute("data-line") ?? 1) - 1) * rowH + shiftY;
      if (this.classList.contains("code-view"))
        return new DOMRect(0, 0, 800, 600);
      if (this.classList.contains("code-wrap"))
        return new DOMRect(0, 0, 800, 640);
      if (this.classList.contains("code-card"))
        return new DOMRect(0, 0, 200, 50);
      if (this.classList.contains("code-row"))
        return new DOMRect(0, top, 800, rowH);
      if (this.classList.contains("code-mark"))
        return new DOMRect(30, top, 10, rowH);
      if (this.matches("a.code-link")) return new DOMRect(linkX, top, 20, rowH);
      return new DOMRect();
    },
  );
}

const linkInfo = (): LinkInfo => ({
  href: "#",
  about: "here",
  guess: false,
  external: false,
  here: true,
});
// Each word "lN" is link word N.
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
const cardOf = (k: number): CardText | undefined =>
  k === 9 ? undefined : { code: `l${k} :: Int`, lines: [`About l${k}.`] };

function mount(text: string) {
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
        card={cardOf}
        ref={(a) => (api = a)}
      />
    ),
    root,
  );
  if (api === undefined) throw new Error("no api");
  return { root, api, setRender };
}

const cell = (root: HTMLElement, n: number) => {
  const el = root.querySelector<HTMLElement>(
    `.code-row[data-line="${n}"] .code-cell`,
  );
  if (el === null) throw new Error(`no line ${n}`);
  return el;
};
const link = (root: HTMLElement, n: number, text: string) => {
  const a = [...cell(root, n).querySelectorAll("a.code-link")].find(
    (e) => e.textContent === text,
  );
  if (!(a instanceof HTMLElement)) throw new Error(`no link ${text}`);
  return a;
};
const view = (root: HTMLElement) => {
  const el = root.querySelector<HTMLElement>(".code-view");
  if (el === null) throw new Error("no view");
  return el;
};
const card = (root: HTMLElement) => {
  const el = root.querySelector<HTMLElement>(".code-card");
  if (el === null) throw new Error("no card");
  return el;
};
const shown = (root: HTMLElement) => !card(root).hidden;
const press = (key: string) => {
  const e = new KeyboardEvent("keydown", {
    key,
    bubbles: true,
    cancelable: true,
  });
  (document.activeElement ?? document.body).dispatchEvent(e);
  return e;
};
const pointer = (
  type: string,
  el: Element,
  init: PointerEventInit = {},
): void => {
  el.dispatchEvent(
    new PointerEvent(type, {
      bubbles: type === "pointerover" || type === "pointerout",
      pointerType: "mouse",
      ...init,
    }),
  );
};

beforeEach(() => {
  shiftY = 0;
  linkX = 100;
  fakeLayout();
  fakeRects();
  vi.useFakeTimers();
  // A key first: the last input wasn't a pointer press.
  press("Shift");
});

afterEach(() => {
  dispose?.();
  dispose = undefined;
  document.body.replaceChildren();
  vi.useRealTimers();
  vi.restoreAllMocks();
});

describe("hover card", () => {
  it("pointer: shows after the delay, one tooltip, text only", () => {
    const { root } = mount("ab l1 l2\nx\n");
    pointer("pointerover", link(root, 1, "l1"));
    vi.advanceTimersByTime(showDelay - 1);
    expect(shown(root)).toBe(false);
    vi.advanceTimersByTime(1);
    expect(shown(root)).toBe(true);
    const c = card(root);
    expect(root.querySelectorAll('[role="tooltip"]')).toHaveLength(1);
    expect(c.querySelector(".code-card-code")?.textContent).toBe("l1 :: Int");
    expect(c.querySelector("p")?.textContent).toBe("About l1.");
    // Under the row, at the link.
    expect(c.getAttribute("style")).toMatch(/--card-x:\s*100px/);
    expect(c.getAttribute("style")).toMatch(/--card-y:\s*21px/);
    // Focus is elsewhere: no description.
    expect(link(root, 1, "l1").hasAttribute("aria-describedby")).toBe(false);
    // Another link while shown: at once.
    pointer("pointerover", link(root, 1, "l2"), {
      relatedTarget: link(root, 1, "l1"),
    });
    expect(c.querySelector("p")?.textContent).toBe("About l2.");
  });

  it("pointer: a grace time, the card is hoverable", () => {
    const { root } = mount("ab l1\nx\n");
    const a = link(root, 1, "l1");
    pointer("pointerover", a);
    vi.advanceTimersByTime(showDelay);
    pointer("pointerout", a, { relatedTarget: cell(root, 2) });
    vi.advanceTimersByTime(hideDelay - 1);
    expect(shown(root)).toBe(true);
    // Into the card: it stays.
    pointer("pointerenter", card(root));
    vi.advanceTimersByTime(hideDelay * 3);
    expect(shown(root)).toBe(true);
    pointer("pointerleave", card(root), { relatedTarget: cell(root, 2) });
    vi.advanceTimersByTime(hideDelay);
    expect(shown(root)).toBe(false);
  });

  it("pointer: none for touch, with a button down, or without a card", () => {
    const { root } = mount("ab l1 l9\n");
    pointer("pointerover", link(root, 1, "l1"), { pointerType: "touch" });
    pointer("pointerover", link(root, 1, "l1"), { buttons: 1 });
    vi.advanceTimersByTime(showDelay);
    expect(shown(root)).toBe(false);
    pointer("pointerover", link(root, 1, "l9"));
    vi.advanceTimersByTime(showDelay);
    expect(shown(root)).toBe(false);
  });

  it("keys: shows at once with the description; the cell hides it", () => {
    const { root } = mount("ab l1 l2\n");
    cell(root, 1).focus();
    press("ArrowRight");
    const a = link(root, 1, "l1");
    expect(document.activeElement).toBe(a);
    expect(shown(root)).toBe(true);
    expect(a.getAttribute("aria-describedby")).toBe(card(root).id);
    press("ArrowRight");
    expect(a.hasAttribute("aria-describedby")).toBe(false);
    expect(link(root, 1, "l2").getAttribute("aria-describedby")).toBe(
      card(root).id,
    );
    press("ArrowLeft");
    press("ArrowLeft");
    expect(document.activeElement).toBe(cell(root, 1));
    expect(shown(root)).toBe(false);
    expect(cell(root, 1).hasAttribute("aria-describedby")).toBe(false);
    expect(root.querySelector("[aria-describedby]")).toBeNull();
  });

  it("Esc hides it and is stopped; no card: Esc is left alone", () => {
    const { root } = mount("ab l1 l2\n");
    const seen = vi.fn();
    document.addEventListener("keydown", seen);
    try {
      cell(root, 1).focus();
      press("ArrowRight");
      const e = press("Escape");
      expect(e.defaultPrevented).toBe(true);
      expect(seen).not.toHaveBeenCalledWith(e);
      expect(shown(root)).toBe(false);
      // Focus stays.
      expect(document.activeElement).toBe(link(root, 1, "l1"));
      expect(root.querySelector("[aria-describedby]")).toBeNull();
      const again = press("Escape");
      expect(again.defaultPrevented).toBe(false);
      expect(seen).toHaveBeenCalledWith(again);
    } finally {
      document.removeEventListener("keydown", seen);
    }
  });

  it("dismissed: a redraw doesn't bring it back, another link does", () => {
    const { root, setRender } = mount("ab l1 l2\n");
    cell(root, 1).focus();
    press("ArrowRight");
    press("Escape");
    setRender(() => (n: number, t: string) => linked(n, t));
    expect(document.activeElement).toBe(link(root, 1, "l1"));
    expect(shown(root)).toBe(false);
    press("ArrowRight");
    expect(shown(root)).toBe(true);
    press("ArrowLeft");
    expect(shown(root)).toBe(true);
  });

  it("a redraw keeps the card on the new link", () => {
    const { root, setRender } = mount("ab l1\n");
    cell(root, 1).focus();
    press("ArrowRight");
    const old = link(root, 1, "l1");
    setRender(() => (n: number, t: string) => linked(n, t));
    const a = link(root, 1, "l1");
    expect(a).not.toBe(old);
    expect(shown(root)).toBe(true);
    expect(a.getAttribute("aria-describedby")).toBe(card(root).id);
    // Plain again: the link went, so did the card.
    setRender(undefined);
    expect(shown(root)).toBe(false);
  });

  it("a mousedown hides it; focus after a press gets none", () => {
    const { root } = mount("ab l1 l2\n");
    cell(root, 1).focus();
    press("ArrowRight");
    const down = new MouseEvent("mousedown", { bubbles: true });
    cell(root, 1).dispatchEvent(down);
    expect(shown(root)).toBe(false);
    link(root, 1, "l2").focus();
    expect(shown(root)).toBe(false);
    // A key again: focus shows it.
    press("ArrowLeft");
    expect(shown(root)).toBe(true);
  });

  it("a mousedown on the card keeps focus and the selection", () => {
    const { root } = mount("ab l1\n");
    cell(root, 1).focus();
    press("ArrowRight");
    const down = new MouseEvent("mousedown", {
      bubbles: true,
      cancelable: true,
    });
    card(root).dispatchEvent(down);
    expect(down.defaultPrevented).toBe(true);
    expect(shown(root)).toBe(true);
  });

  it("focus out of the view hides a focus card", () => {
    const { root } = mount("ab l1\n");
    const out = document.createElement("button");
    document.body.append(out);
    cell(root, 1).focus();
    press("ArrowRight");
    out.focus();
    expect(shown(root)).toBe(false);
  });

  it("unmounted: Esc is left alone", () => {
    const { root } = mount("ab l1\n");
    cell(root, 1).focus();
    press("ArrowRight");
    dispose?.();
    dispose = undefined;
    expect(press("Escape").defaultPrevented).toBe(false);
  });

  const text = (root: HTMLElement) =>
    card(root).querySelector("p")?.textContent;
  const describedBy = (root: HTMLElement) =>
    [...root.querySelectorAll("[aria-describedby]")].map((e) => e.textContent);

  it("pointer over a focus card: after the delay, then back to focus", () => {
    const { root } = mount("ab l1 l2\nx\n");
    cell(root, 1).focus();
    press("ArrowRight");
    expect(text(root)).toBe("About l1.");
    pointer("pointerover", link(root, 1, "l2"));
    vi.advanceTimersByTime(showDelay - 1);
    expect(text(root)).toBe("About l1.");
    vi.advanceTimersByTime(1);
    expect(text(root)).toBe("About l2.");
    expect(describedBy(root)).toEqual([]);
    pointer("pointerout", link(root, 1, "l2"), {
      relatedTarget: cell(root, 2),
    });
    vi.advanceTimersByTime(hideDelay);
    expect(shown(root)).toBe(true);
    expect(text(root)).toBe("About l1.");
    expect(describedBy(root)).toEqual(["l1"]);
  });

  it("pointer card, then keys to that link: the grace time ends", () => {
    const { root } = mount("ab l1\nx\n");
    const a = link(root, 1, "l1");
    pointer("pointerover", a);
    vi.advanceTimersByTime(showDelay);
    pointer("pointerout", a, { relatedTarget: cell(root, 2) });
    cell(root, 1).focus();
    press("ArrowRight");
    vi.advanceTimersByTime(hideDelay * 2);
    expect(shown(root)).toBe(true);
    expect(describedBy(root)).toEqual(["l1"]);
  });

  it("pointerout: inside the link or to the card keeps it", () => {
    const { root } = mount("ab l1\nx\n");
    const a = link(root, 1, "l1");
    pointer("pointerover", a);
    vi.advanceTimersByTime(showDelay);
    pointer("pointerout", a, { relatedTarget: a });
    pointer("pointerout", a, { relatedTarget: card(root) });
    vi.advanceTimersByTime(hideDelay * 2);
    expect(shown(root)).toBe(true);
    // Out before the delay: no card.
    pointer("pointerleave", card(root), { relatedTarget: cell(root, 2) });
    vi.advanceTimersByTime(hideDelay);
    pointer("pointerover", a);
    pointer("pointerout", a, { relatedTarget: cell(root, 2) });
    vi.advanceTimersByTime(showDelay * 2);
    expect(shown(root)).toBe(false);
  });

  it("a redraw before the delay: the card for the new link", () => {
    const { root, setRender } = mount("ab l1\n");
    pointer("pointerover", link(root, 1, "l1"));
    setRender(() => (n: number, t: string) => linked(n, t));
    vi.advanceTimersByTime(showDelay);
    expect(shown(root)).toBe(true);
    expect(text(root)).toBe("About l1.");
  });

  it("a redraw opens nothing; a pointer card stays open", () => {
    const { root, setRender } = mount("ab l1 l2\n");
    // Focus by a press: no card, and none after the redraw.
    document.dispatchEvent(new MouseEvent("mousedown", { bubbles: true }));
    link(root, 1, "l1").focus();
    expect(shown(root)).toBe(false);
    setRender(() => (n: number, t: string) => linked(n, t));
    expect(document.activeElement).toBe(link(root, 1, "l1"));
    expect(shown(root)).toBe(false);
    pointer("pointerover", link(root, 1, "l2"));
    vi.advanceTimersByTime(showDelay);
    setRender(() => (n: number, t: string) => linked(n, t));
    expect(shown(root)).toBe(true);
    expect(text(root)).toBe("About l2.");
  });

  it("dismissed: focus leaving the link ends it; the pointer elsewhere doesn't", () => {
    const { root } = mount("ab l1 l2\nx\n");
    cell(root, 1).focus();
    press("ArrowRight");
    press("Escape");
    // A pointer card on l2 comes and goes: none back for l1.
    pointer("pointerover", link(root, 1, "l2"));
    vi.advanceTimersByTime(showDelay);
    expect(text(root)).toBe("About l2.");
    pointer("pointerout", link(root, 1, "l2"), {
      relatedTarget: cell(root, 2),
    });
    vi.advanceTimersByTime(hideDelay);
    expect(shown(root)).toBe(false);
    // Over l1 itself: still dismissed (focus is on it).
    pointer("pointerover", link(root, 1, "l1"));
    pointer("pointerout", link(root, 1, "l1"), {
      relatedTarget: cell(root, 2),
    });
    pointer("pointerover", link(root, 1, "l1"));
    vi.advanceTimersByTime(showDelay);
    expect(shown(root)).toBe(false);
    // To the cell and back: shown again.
    press("ArrowLeft");
    press("ArrowRight");
    expect(text(root)).toBe("About l1.");
  });

  it("follows a scroll in a frame, hides out of view, comes back", () => {
    const { root } = mount("ab l1\n");
    cell(root, 1).focus();
    press("ArrowRight");
    expect(card(root).getAttribute("style")).toMatch(/--card-y:\s*21px/);
    shiftY = 50;
    view(root).dispatchEvent(new Event("scroll"));
    vi.advanceTimersToNextFrame();
    expect(card(root).getAttribute("style")).toMatch(/--card-y:\s*71px/);
    shiftY = 700;
    view(root).dispatchEvent(new Event("scroll"));
    vi.advanceTimersToNextFrame();
    expect(shown(root)).toBe(false);
    shiftY = 0;
    window.dispatchEvent(new Event("resize"));
    vi.advanceTimersToNextFrame();
    expect(shown(root)).toBe(true);
  });

  it("a link under the gutter: described at focus, shown once revealed", () => {
    linkX = 10;
    const { root } = mount("ab l1\n");
    cell(root, 1).focus();
    press("ArrowRight");
    expect(document.activeElement).toBe(link(root, 1, "l1"));
    expect(shown(root)).toBe(false);
    expect(describedBy(root)).toEqual(["l1"]);
    linkX = 100;
    view(root).dispatchEvent(new Event("scroll"));
    vi.advanceTimersToNextFrame();
    expect(shown(root)).toBe(true);
  });

  it("focus out with a pointer card on the focused link: no description", () => {
    const { root } = mount("ab l1\n");
    const out = document.createElement("button");
    document.body.append(out);
    document.dispatchEvent(new MouseEvent("mousedown", { bubbles: true }));
    link(root, 1, "l1").focus();
    pointer("pointerover", link(root, 1, "l1"));
    vi.advanceTimersByTime(showDelay);
    expect(describedBy(root)).toEqual(["l1"]);
    out.focus();
    expect(shown(root)).toBe(true);
    expect(describedBy(root)).toEqual([]);
  });

  it("a key with no view mounted ends a press", () => {
    mount("ab l1\n");
    dispose?.();
    dispose = undefined;
    document.body.replaceChildren();
    document.dispatchEvent(new MouseEvent("mousedown", { bubbles: true }));
    document.body.dispatchEvent(
      new KeyboardEvent("keydown", { key: "Tab", bubbles: true }),
    );
    const { root } = mount("ab l1\n");
    link(root, 1, "l1").focus();
    expect(shown(root)).toBe(true);
  });

  it("a focusout to nowhere while focus stays (Chromium's redraw) ends nothing", async () => {
    const { root, setRender } = mount("ab l1\n");
    cell(root, 1).focus();
    press("ArrowRight");
    press("Escape");
    link(root, 1, "l1").dispatchEvent(
      new FocusEvent("focusout", { bubbles: true, relatedTarget: null }),
    );
    await Promise.resolve();
    setRender(() => (n: number, t: string) => linked(n, t));
    expect(document.activeElement).toBe(link(root, 1, "l1"));
    expect(shown(root)).toBe(false);
  });
});
