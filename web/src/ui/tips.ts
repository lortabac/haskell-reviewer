// Tips on buttons and links (D-300), and on a few spans that are Tab
// stops for them (the header's revisions, D-335): CSS shows [data-tip]
// on hover and keyboard focus (and only when the device can hover).
// Esc hides every tip on screen (WCAG 1.4.13) until the pointer leaves
// or focus moves.
// Only then it is ours (preventDefault, so it does not also clear a
// selection); with no tip on screen, Esc goes on as usual.
const TIP = "[data-tip]";

// Does the CSS show its tip now? (Same rule as the CSS.)
function shown(el: Element, win: Window | null): boolean {
  if (el.hasAttribute("data-tip-off") || !el.isConnected) return false;
  if (win?.matchMedia?.("(hover: hover)").matches === false) return false;
  return el.matches(":hover") || el.matches(":focus-visible");
}

// Focused by keyboard (or script after a key): checked when focus
// arrives. Chromium turns :focus-visible on at the first key press
// (Esc too) for a control focused by a click, and that tip was not on
// screen before the key.
function tipOf(t: Element | null): Element | null {
  return t === null ? null : t.closest(TIP);
}

const keyFocused = new WeakSet<Element>();

// The tip grows right from its control. Where that would leave the
// window, it grows left (data-tip-side). Measured each time it is about
// to show (not on resize: the next show measures again).
function place(el: Element, win: Window | null): void {
  if (win === null) return;
  el.removeAttribute("data-tip-side");
  win.requestAnimationFrame(() => {
    const w = Number.parseFloat(win.getComputedStyle(el, "::after").width);
    if (Number.isNaN(w)) return;
    if (
      el.getBoundingClientRect().left + w >
      win.document.documentElement.clientWidth
    ) {
      el.setAttribute("data-tip-side", "right");
    }
  });
}

// Returns a function that removes the listeners.
export function installTips(doc: Document = document): () => void {
  // Moving out or blurring: the tip is allowed again.
  const clear = (e: Event) => {
    if (e.target instanceof Element) {
      const el = e.target.closest(TIP);
      el?.removeAttribute("data-tip-off");
      if (el !== null && e.type === "focusout") keyFocused.delete(el);
    }
  };
  const onKey = (e: KeyboardEvent) => {
    if (e.key !== "Escape") {
      // A control focused by a click turns :focus-visible at the next
      // key (Chromium, WebKit): its tip shows now, so place it, and
      // Esc is ours from here on. Not on Esc itself: that tip was not
      // on screen before.
      const a = tipOf(doc.activeElement);
      if (a !== null && !keyFocused.has(a) && a.matches(":focus-visible")) {
        keyFocused.add(a);
        place(a, doc.defaultView);
      }
      return;
    }
    if (e.isComposing || e.keyCode === 229) return;
    // A modal dialog over the page (the ? overlay): its Esc, not ours.
    if (e.target instanceof Element && e.target.closest("dialog[open]")) return;
    const on = [...doc.querySelectorAll(TIP)].filter((el) =>
      shown(el, doc.defaultView),
    );
    // Hide all of them. Ours only if one was on screen before this key.
    let seen = false;
    for (const el of on) {
      el.setAttribute("data-tip-off", "");
      if (el.matches(":hover") || keyFocused.has(el)) seen = true;
    }
    if (seen) e.preventDefault();
  };
  const show = (e: Event) => {
    if (e.target instanceof Element) {
      const el = e.target.closest(TIP);
      if (el !== null) {
        place(el, doc.defaultView);
        if (e.type === "focusin" && el.matches(":focus-visible")) {
          keyFocused.add(el);
        }
      }
    }
  };
  doc.addEventListener("pointerover", show);
  doc.addEventListener("focusin", show);
  const events = ["pointerout", "focusout"];
  for (const ev of events) doc.addEventListener(ev, clear);
  // Capture, and installed in main.tsx before the other Esc handlers,
  // so it runs first. A hover card on screen closes on the same Esc.
  doc.addEventListener("keydown", onKey, true);
  return () => {
    for (const ev of events) doc.removeEventListener(ev, clear);
    doc.removeEventListener("pointerover", show);
    doc.removeEventListener("focusin", show);
    doc.removeEventListener("keydown", onKey, true);
  };
}
