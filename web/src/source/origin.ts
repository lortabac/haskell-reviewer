// Back goes to where a link was followed (D-339). Before the source
// view leaves, the cursor's line gets its own history entry:
// `#L<line>`, the column in the state. The docs page: its declaration
// (D-340).

export interface Origin {
  // UTF-16, from 0.
  col: number;
}

export function originOf(state: unknown): Origin | undefined {
  if (typeof state !== "object" || state === null) return undefined;
  const o = (state as { origin?: unknown }).origin;
  if (typeof o !== "object" || o === null) return undefined;
  const col = (o as { col?: unknown }).col;
  return typeof col === "number" && col >= 0 ? { col } : undefined;
}

// line, col: the cursor. marked: the first line the URL marks now.
// to: where the link goes (a full URL), if known.
export function saveOrigin(
  line: number,
  col: number,
  marked: number | undefined,
  to?: string,
): void {
  const here =
    marked === line ||
    (marked === undefined && window.location.hash === "" && line === 1);
  pushOrigin(`#L${line}`, { origin: { col } }, to, here);
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
