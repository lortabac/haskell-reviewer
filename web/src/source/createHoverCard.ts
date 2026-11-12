// The hover card of one source view (G12, WCAG 1.4.13, D-285): on a
// token link under the pointer (after a delay) or with keyboard focus
// (at once). Hoverable, Esc hides it, no timeout. One card per view.
import { onCleanup, onMount } from "solid-js";
import { setVar } from "../ui/cssom";
import type { CardText } from "./hover";
import { placeCard } from "./place";
import { colIn } from "./selection";

export const showDelay = 500;
export const hideDelay = 200;

export interface HoverCard {
  // el (a link) is about to get focus by keys or focusAt: its card
  // first, so the description is there when focus comes.
  beforeFocus: (el: HTMLElement) => void;
  // The rows were redrawn: the card moves to the new link at the same
  // spot, with its new text, or goes. Never opens one.
  relink: () => void;
}

interface Spot {
  line: number;
  col: number;
}

interface Owner extends Spot {
  el: HTMLElement;
  source: "pointer" | "focus";
}

// The focused link of the view. card: it came by keys (or a screen
// reader, a script), not by a press, so it may have a card.
interface Focused extends Spot {
  el: HTMLElement;
  card: boolean;
}

// The last input was a pointer press (not a key): focus that follows
// gets no card. For the whole page, views or not: a click can open
// another view, keys outside one count too.
let pressed = false;
if (typeof document !== "undefined") {
  document.addEventListener("mousedown", () => (pressed = true), true);
  document.addEventListener("keydown", () => (pressed = false), true);
}

const linkOf = (t: EventTarget | null): HTMLElement | undefined =>
  t instanceof Element
    ? (t.closest<HTMLElement>("a.code-link") ?? undefined)
    : undefined;

const same = (a: Spot | undefined, b: Spot | undefined) =>
  a !== undefined && b !== undefined && a.line === b.line && a.col === b.col;

export function createHoverCard(opts: {
  view: () => HTMLElement;
  card: () => HTMLElement;
  row: (line: number) => HTMLElement | undefined;
  // The card of a link word; undefined: none.
  text: (link: number) => CardText | undefined;
  // A drag from a link is on: no card.
  dragging: () => boolean;
}): HoverCard {
  let owner: Owner | undefined;
  let visible = false;
  let focused: Focused | undefined;
  // Hidden by Esc: no card here until focus leaves this link, or (if
  // focus isn't on it) the pointer leaves it.
  let dismissed: Spot | undefined;
  // The link the pointer waits on (showDelay).
  let pending: Spot | undefined;
  let showTimer: ReturnType<typeof setTimeout> | undefined;
  let hideTimer: ReturnType<typeof setTimeout> | undefined;
  let frame: number | undefined;
  // The link with our aria-describedby.
  let described: HTMLElement | undefined;

  const spotOf = (el: HTMLElement): Spot | undefined => {
    const r = el.closest(".code-row");
    const cell = el.closest(".code-cell");
    if (r === null || cell === null) return undefined;
    return {
      line: Number(r.getAttribute("data-line")),
      col: colIn(cell, el, 0),
    };
  };
  // The link at a spot now (rows get redrawn).
  const linkAt = (s: Spot) => {
    const cell = opts.row(s.line)?.querySelector(".code-cell");
    if (cell == null) return undefined;
    return [...cell.querySelectorAll<HTMLElement>("a.code-link")].find(
      (a) => colIn(cell, a, 0) === s.col,
    );
  };

  // The focused link has the card's text as its description while the
  // card is its own: shown, or a focus card off view for now (a reveal
  // may be coming).
  const syncDescribed = () => {
    const want =
      owner !== undefined &&
      same(owner, focused) &&
      (visible || owner.source === "focus")
        ? owner.el
        : undefined;
    if (described === want) return;
    described?.removeAttribute("aria-describedby");
    described = want;
    want?.setAttribute("aria-describedby", opts.card().id);
  };

  const setVisible = (v: boolean) => {
    if (visible !== v) {
      visible = v;
      opts.card().hidden = !v;
      if (v) document.addEventListener("keydown", onEsc, true);
      else document.removeEventListener("keydown", onEsc, true);
    }
    syncDescribed();
  };

  const stopPending = () => {
    clearTimeout(showTimer);
    pending = undefined;
  };

  const hide = () => {
    stopPending();
    clearTimeout(hideTimer);
    owner = undefined;
    setVisible(false);
  };

  // A pointer card ended: the focused link's card again, if it had one.
  const endPointer = () => {
    hide();
    const f = focused;
    if (f !== undefined && f.card && !same(f, dismissed) && f.el.isConnected) {
      show(f.el, "focus");
    }
  };

  // Put the card at its owner, or hide it there if the owner is out
  // of view: a focus owner keeps the card for when it's back.
  const place = () => {
    const o = owner;
    if (o === undefined) return;
    const r = o.el.closest<HTMLElement>(".code-row");
    if (!o.el.isConnected || r === null) {
      if (o.source === "pointer") endPointer();
      else hide();
      return;
    }
    const card = opts.card();
    // Its size with the new text. Shown for this read only: no paint
    // comes in between.
    card.hidden = false;
    const c = card.getBoundingClientRect();
    card.hidden = !visible;
    const v = opts.view();
    const b = v.getBoundingClientRect();
    const top = b.top + v.clientTop;
    const left = b.left + v.clientLeft;
    const spot = placeCard({
      link: o.el.getBoundingClientRect(),
      row: r.getBoundingClientRect(),
      view: {
        left,
        top,
        right: left + v.clientWidth,
        bottom: top + v.clientHeight,
      },
      gutter:
        r.querySelector(".code-mark")?.getBoundingClientRect().right ?? left,
      wrap: (card.parentElement ?? v).getBoundingClientRect(),
      card: { width: c.width, height: c.height },
      windowHeight: window.innerHeight,
    });
    if (spot === undefined) {
      if (o.source === "pointer") endPointer();
      else setVisible(false);
      return;
    }
    setVar(card, "--card-x", `${spot.x}px`);
    setVar(card, "--card-y", `${spot.y}px`);
    card.dataset.side = spot.side;
    setVisible(true);
  };

  // Fill the card for el's link word; false if it has none.
  const fill = (el: HTMLElement) => {
    const t = opts.text(Number(el.dataset.link));
    if (t === undefined) return false;
    const card = opts.card();
    const nodes: Node[] = [];
    if (t.code !== null) {
      const c = document.createElement("div");
      c.className = "code-card-code";
      c.textContent = t.code;
      nodes.push(c);
    }
    for (const l of t.lines) {
      const p = document.createElement("p");
      p.textContent = l;
      nodes.push(p);
    }
    card.replaceChildren(...nodes);
    return true;
  };

  const show = (el: HTMLElement, source: Owner["source"]) => {
    stopPending();
    clearTimeout(hideTimer);
    const s = spotOf(el);
    if (s === undefined || !fill(el)) {
      hide();
      return;
    }
    if (owner?.el !== el) {
      // The old link loses its description first.
      owner = undefined;
      syncDescribed();
    }
    owner = { el, ...s, source };
    place();
  };

  // The same owner on a new <a> (a redraw), with its new text; it goes
  // if the link or its card went. Opens nothing.
  const moveOwner = (el: HTMLElement | undefined) => {
    const o = owner;
    if (o === undefined) return;
    if (el === undefined || !fill(el)) {
      hide();
      return;
    }
    if (el !== o.el) {
      owner = undefined;
      syncDescribed();
    }
    owner = { ...o, el };
    if (visible || o.source === "focus") place();
  };

  // Focus is coming (or came) to el.
  const focusLink = (el: HTMLElement) => {
    const s = spotOf(el);
    if (s === undefined) return;
    if (same(s, focused) && focused !== undefined) {
      // Focus on the same spot again: a redraw's new link. Keep what
      // was there, open or closed.
      focused = { ...focused, el };
      if (same(owner, s)) moveOwner(el);
      syncDescribed();
      return;
    }
    // Focus moved: leaving a dismissed link ends that.
    if (
      dismissed !== undefined &&
      !same(dismissed, s) &&
      same(dismissed, focused)
    ) {
      dismissed = undefined;
    }
    const card = !pressed && !opts.dragging();
    focused = { ...s, el, card };
    if (!card || same(dismissed, s)) {
      // A click: no card, and a focus card for another link goes.
      if (owner?.source === "focus") hide();
      else syncDescribed();
      return;
    }
    show(el, "focus");
  };

  // Focus left the links (to the cell, or out of the view).
  const unfocus = () => {
    if (same(dismissed, focused)) dismissed = undefined;
    focused = undefined;
    if (owner?.source === "focus") hide();
    else syncDescribed();
  };

  const onPointerOver = (e: PointerEvent) => {
    const a = linkOf(e.target);
    if (a === undefined || linkOf(e.relatedTarget) === a) return;
    if (e.pointerType === "touch" || e.buttons !== 0 || opts.dragging()) {
      return;
    }
    if (owner?.el === a) {
      // Back from the card or within the grace time.
      clearTimeout(hideTimer);
      return;
    }
    const s = spotOf(a);
    if (s === undefined || same(s, dismissed)) return;
    stopPending();
    // From one pointer card to the next: at once. Else (no card, or
    // the focus card) after the delay.
    if (visible && owner?.source === "pointer") {
      show(a, "pointer");
      return;
    }
    pending = s;
    showTimer = setTimeout(() => {
      const p = pending;
      pending = undefined;
      // Looked up again: a redraw may have made a new <a>.
      const el = p === undefined ? undefined : linkAt(p);
      if (el !== undefined) show(el, "pointer");
    }, showDelay);
  };
  const leave = () => {
    if (owner?.source !== "pointer") return;
    clearTimeout(hideTimer);
    hideTimer = setTimeout(endPointer, hideDelay);
  };
  const onPointerOut = (e: PointerEvent) => {
    const a = linkOf(e.target);
    if (a === undefined || linkOf(e.relatedTarget) === a) return;
    const s = spotOf(a);
    if (same(s, pending)) stopPending();
    // A pointer dismissal ends when the pointer leaves; a focus one
    // when focus does.
    if (same(s, dismissed) && !same(s, focused)) dismissed = undefined;
    const to = e.relatedTarget;
    if (to instanceof Node && opts.card().contains(to)) return;
    if (owner?.el === a) leave();
  };
  const onCardEnter = () => clearTimeout(hideTimer);
  const onCardLeave = (e: PointerEvent) => {
    if (owner !== undefined && linkOf(e.relatedTarget) === owner.el) return;
    leave();
  };

  const onFocusIn = (e: FocusEvent) => {
    const a = linkOf(e.target);
    if (a !== undefined) focusLink(a);
    else if (focused !== undefined) unfocus();
  };
  const onFocusOut = (e: FocusEvent) => {
    // A redraw took the focused link: its new one comes (focusLink).
    if (e.target instanceof Node && !e.target.isConnected) return;
    const to = e.relatedTarget;
    if (to instanceof Node && opts.view().contains(to)) return;
    if (to !== null) {
      unfocus();
      return;
    }
    // Nowhere: Chromium says so when a redraw removes the focused
    // link, which is focused again right after. Look once that's done.
    queueMicrotask(() => {
      if (!opts.view().contains(document.activeElement)) unfocus();
    });
  };

  // Only while the card shows (capture: before search and the rest).
  function onEsc(e: KeyboardEvent): void {
    if (e.key !== "Escape" || e.isComposing || owner === undefined) return;
    e.preventDefault();
    e.stopPropagation();
    dismissed = { line: owner.line, col: owner.col };
    hide();
  }

  const onDown = (e: MouseEvent) => {
    if (e.target instanceof Node && opts.card().contains(e.target)) {
      // Focus and the selection stay; its text isn't selectable.
      e.preventDefault();
      return;
    }
    // A press: the focused link gets no card back after this.
    if (focused !== undefined) focused = { ...focused, card: false };
    hide();
  };

  const follow = () => {
    if (owner === undefined || frame !== undefined) return;
    frame = requestAnimationFrame(() => {
      frame = undefined;
      place();
    });
  };

  const relink = () => {
    if (focused !== undefined) {
      const el = linkAt(focused);
      if (el !== undefined) focused = { ...focused, el };
    }
    if (owner !== undefined) moveOwner(linkAt(owner));
  };

  onMount(() => {
    const v = opts.view();
    const card = opts.card();
    v.addEventListener("pointerover", onPointerOver);
    v.addEventListener("pointerout", onPointerOut);
    v.addEventListener("focusin", onFocusIn);
    v.addEventListener("focusout", onFocusOut);
    card.addEventListener("pointerenter", onCardEnter);
    card.addEventListener("pointerleave", onCardLeave);
    document.addEventListener("mousedown", onDown, true);
    // Capture: the view's own scroll and the page's.
    document.addEventListener("scroll", follow, true);
    window.addEventListener("resize", follow);
    onCleanup(() => {
      hide();
      if (frame !== undefined) cancelAnimationFrame(frame);
      document.removeEventListener("mousedown", onDown, true);
      document.removeEventListener("scroll", follow, true);
      window.removeEventListener("resize", follow);
    });
  });

  return {
    beforeFocus: (el) => {
      if (document.activeElement !== el) focusLink(el);
    },
    relink,
  };
}
