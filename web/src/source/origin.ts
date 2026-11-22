// Back goes to where a link was followed (D-339). Before the source
// view leaves, the cursor's line gets its own history entry:
// `#L<line>`, the column in the state.

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
  const l = window.location;
  const url = `${l.pathname}${l.search}#L${line}`;
  const h = window.history;
  const state = h.state as Record<string, unknown> | null;
  // The link goes to this very line: its own entry does it.
  if (to === l.origin + url) return;
  // The hash too: the router's marks lag after our push.
  if (
    marked === line ||
    l.hash === `#L${line}` ||
    (marked === undefined && l.hash === "" && line === 1)
  ) {
    // This entry lands there already: keep the column.
    h.replaceState({ ...state, origin: { col } }, "");
    return;
  }
  h.pushState({ origin: { col } }, "", url);
  // As the router stamps its own entries (it reads this on popstate).
  h.replaceState({ ...h.state, _depth: h.length - 1 }, "");
}
