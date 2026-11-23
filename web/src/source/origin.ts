// Back goes to where a link was followed (D-339). Before the source
// view leaves, the cursor's line gets its own history entry:
// `#L<line>`, the column in the state. The docs page: its declaration
// (D-340). The diff view: `#<side>-L<line>` (D-342).

export interface Origin {
  // UTF-16, from 0.
  col: number;
  // The diff view's cursor was on a gap: land there, don't open it.
  gap?: true;
}

export function originOf(state: unknown): Origin | undefined {
  if (typeof state !== "object" || state === null) return undefined;
  const o = (state as { origin?: unknown }).origin;
  if (typeof o !== "object" || o === null) return undefined;
  const { col, gap } = o as { col?: unknown; gap?: unknown };
  if (typeof col !== "number" || col < 0) return undefined;
  return gap === true ? { col, gap } : { col };
}

// hash: the cursor's place. marked: the URL marks it now. first: it is
// where a page with no hash lands (line 1, the first row). to: where
// the link goes (a full URL), if known.
export function saveOrigin(
  hash: string,
  origin: Origin,
  marked: boolean,
  first: boolean,
  to?: string,
): void {
  const here = marked || (window.location.hash === "" && first);
  pushOrigin(hash, { origin }, to, here);
}

// A plain left click on a same-site link in the view: Back should come
// here (D-339). sameFile: a same-file jump, which may take the click
// (preventDefault). save: push the origin, with the link's URL.
export function onLeaveClick(
  e: MouseEvent,
  sameFile: (e: MouseEvent, a: HTMLAnchorElement) => void,
  save: (to: string) => void,
): void {
  const a =
    e.target instanceof Element
      ? e.target.closest<HTMLAnchorElement>("a[href]")
      : null;
  if (a === null || e.defaultPrevented || e.button !== 0) return;
  if (e.ctrlKey || e.metaKey || e.altKey || e.shiftKey) return;
  // A new tab, or another site: no history here.
  if (a.target !== "" || a.origin !== window.location.origin) return;
  // The router leaves these to the browser.
  if (a.hasAttribute("download") || a.relList.contains("external")) return;
  sameFile(e, a);
  if (e.defaultPrevented || a.href === window.location.href) return;
  save(a.href);
}

// An entry for hash on this page, before a link leaves it (D-339,
// D-340). here: this entry lands there already, so no new entry, only
// state goes in it. to: where the link goes (a full URL), if known.
export function pushOrigin(
  hash: string,
  state: Record<string, unknown>,
  to?: string,
  here = false,
): void {
  const l = window.location;
  const url = `${l.pathname}${l.search}${hash}`;
  const h = window.history;
  // The link goes there: its own entry does it.
  if (to === l.origin + url) return;
  // The hash too: the router lags after our push.
  if (here || l.hash === hash) {
    h.replaceState({ ...(h.state as object | null), ...state }, "");
    return;
  }
  h.pushState(state, "", url);
  // As the router stamps its own entries (it reads this on popstate).
  h.replaceState({ ...(h.state as object), _depth: h.length - 1 }, "");
}
