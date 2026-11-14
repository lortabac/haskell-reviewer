// Keys on the docs page (D-298), read from the DOM: the declarations
// are section.decl, nested items li.subdecl.

const itemSel = "li.subdecl, section.decl";

// The declarations a reader can see: not in a closed <details>.
export function sections(root: ParentNode): HTMLElement[] {
  return [...root.querySelectorAll<HTMLElement>("section.decl")].filter(
    (el) =>
      el.closest("details:not([open])") === null &&
      el.closest("[hidden]") === null,
  );
}

function focusedIn(root: Element, sel: string): HTMLElement | undefined {
  const a = document.activeElement;
  if (a === null || !root.contains(a)) return undefined;
  const el = a.closest<HTMLElement>(sel);
  return el !== null && root.contains(el) ? el : undefined;
}

// The declaration with focus, else the first one on screen.
export function currentSection(root: Element): HTMLElement | undefined {
  return (
    focusedIn(root, "section.decl") ??
    sections(root).find((el) => el.getBoundingClientRect().bottom > 0)
  );
}

// The nested item or declaration with focus, else currentSection.
export function currentItem(root: Element): HTMLElement | undefined {
  return focusedIn(root, itemSel) ?? currentSection(root);
}

// The elements of item that are its own: not those of an item inside.
export function ownOf<T extends Element>(item: Element, sel: string): T[] {
  return [...item.querySelectorAll<T>(sel)].filter(
    (el) => el.closest(itemSel) === item,
  );
}

// j / k: focus the next or previous declaration. From the focused
// one, else from the top of the screen. No hash change.
export function stepSection(root: Element, dir: 1 | -1): boolean {
  const all = sections(root);
  const from = focusedIn(root, "section.decl");
  let to: HTMLElement | undefined;
  if (from !== undefined && all.includes(from)) {
    to = all[all.indexOf(from) + dir];
  } else if (dir === 1) {
    // One j or k leaves a section at its scroll margin, not at 0: that
    // one is the current one, not the next or the previous.
    to = all.find((el) => topOf(el) > marginOf(el) + 2);
  } else {
    for (const el of all) {
      if (topOf(el) < marginOf(el) - 2) to = el;
    }
  }
  if (to === undefined) return false;
  to.scrollIntoView({ block: "start" });
  to.focus({ preventScroll: true });
  return true;
}

const topOf = (el: Element) => el.getBoundingClientRect().top;

const marginOf = (el: Element) =>
  Number.parseFloat(getComputedStyle(el).scrollMarginTop) || 0;
