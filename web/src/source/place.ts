// Where the hover card goes (D-285). Pure: rects in, a spot out. All
// rects in window coordinates; the result is in the wrap's.

export interface Box {
  left: number;
  top: number;
  right: number;
  bottom: number;
}

export interface PlaceIn {
  link: Box;
  // The link's row: the card is not over it, if it fits.
  row: Box;
  // The scroll box's inside (no scrollbars).
  view: Box;
  // The right edge of the sticky gutter (numbers and markers).
  gutter: number;
  // .code-wrap: the card's containing block.
  wrap: Box;
  card: { width: number; height: number };
  windowHeight: number;
}

export interface Spot {
  x: number;
  y: number;
  side: "below" | "above";
}

// Undefined: the link is out of the visible view (under the gutter,
// past an edge, off the window).
export function placeCard(p: PlaceIn): Spot | undefined {
  const { link, row, view, card } = p;
  const left = Math.max(view.left, p.gutter);
  if (link.right <= left || link.left >= view.right) return undefined;
  if (row.bottom <= view.top || row.top >= view.bottom) return undefined;
  if (row.bottom <= 0 || row.top >= p.windowHeight) return undefined;
  const below = row.bottom;
  const above = row.top - card.height;
  let side: Spot["side"];
  let top: number;
  if (below + card.height <= p.windowHeight) {
    side = "below";
    top = below;
  } else if (above >= 0) {
    side = "above";
    top = above;
  } else {
    // Fits neither way: the roomier side, kept in the window (it may
    // cover the row then; Esc hides it). Taller than the window: at
    // its top.
    side = p.windowHeight - row.bottom >= row.top ? "below" : "above";
    const want = side === "below" ? below : above;
    top = Math.max(0, Math.min(want, p.windowHeight - card.height));
  }
  const width = p.wrap.right - p.wrap.left;
  const x = Math.max(
    0,
    Math.min(Math.max(link.left, p.gutter) - p.wrap.left, width - card.width),
  );
  const y = top - p.wrap.top;
  return { x, y, side };
}
