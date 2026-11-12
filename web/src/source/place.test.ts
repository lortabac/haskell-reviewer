import { describe, expect, it } from "vitest";
import { type Box, type PlaceIn, placeCard } from "./place";

const box = (left: number, top: number, w: number, h: number): Box => ({
  left,
  top,
  right: left + w,
  bottom: top + h,
});

// A view at (0, 100), 800 × 400, gutter to x 50, in a wrap at (0, 80).
// The link at x 200 on a row at y 300, 20 high. Card 300 × 60.
function input(over: Partial<PlaceIn> = {}): PlaceIn {
  return {
    link: box(200, 300, 40, 20),
    row: box(0, 300, 800, 20),
    view: box(0, 100, 800, 400),
    gutter: 50,
    wrap: box(0, 80, 800, 440),
    card: { width: 300, height: 60 },
    windowHeight: 700,
    ...over,
  };
}

describe("placeCard", () => {
  it("below the row when it fits, at the link's left", () => {
    expect(placeCard(input())).toEqual({ x: 200, y: 320 - 80, side: "below" });
  });

  it("above when below doesn't fit in the window", () => {
    const p = placeCard(input({ windowHeight: 350 }));
    expect(p).toEqual({ x: 200, y: 300 - 60 - 80, side: "above" });
  });

  it("neither fits: the roomier side, kept in the window", () => {
    const tall = { width: 300, height: 500 };
    const at = (top: number) =>
      input({
        card: tall,
        row: box(0, top, 800, 20),
        link: box(200, top, 40, 20),
      });
    // Window 700: room 400 above, 280 below. Clamped to the top.
    expect(placeCard(at(400))).toEqual({ x: 200, y: 0 - 80, side: "above" });
    // Room 250 above, 430 below: from the row's bottom, moved up to
    // fit (over the row).
    expect(placeCard(at(250))).toEqual({
      x: 200,
      y: 700 - 500 - 80,
      side: "below",
    });
    // Taller than the window: at its top.
    const huge = placeCard({ ...at(300), card: { width: 300, height: 900 } });
    expect(huge?.y).toBe(-80);
  });

  it("x: not left of the gutter, kept inside the wrap", () => {
    expect(placeCard(input({ link: box(30, 300, 40, 20) }))?.x).toBe(50);
    expect(placeCard(input({ link: box(700, 300, 40, 20) }))?.x).toBe(500);
    // A wrap narrower than the card (300 px window): at its left.
    const narrow = input({
      wrap: box(10, 80, 280, 440),
      view: box(10, 100, 280, 400),
      gutter: 40,
      link: box(150, 300, 30, 20),
    });
    expect(placeCard(narrow)?.x).toBe(0);
  });

  it("out of view: under the gutter, past an edge, off the window", () => {
    expect(placeCard(input({ link: box(10, 300, 40, 20) }))).toBeUndefined();
    expect(placeCard(input({ link: box(800, 300, 40, 20) }))).toBeUndefined();
    expect(placeCard(input({ row: box(0, 80, 800, 20) }))).toBeUndefined();
    expect(placeCard(input({ row: box(0, 500, 800, 20) }))).toBeUndefined();
    expect(
      placeCard(input({ row: box(0, 300, 800, 20), windowHeight: 290 })),
    ).toBeUndefined();
    // Half under the gutter: still there.
    expect(placeCard(input({ link: box(40, 300, 40, 20) }))?.x).toBe(50);
  });
});
