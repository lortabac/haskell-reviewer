import { describe, expect, it } from "vitest";
import type { StyledPiece } from "./display";
import type { LinkInfo } from "./links";
import { lineNodes } from "./paint";

const piece = (
  text: string,
  cls: string | null,
  link = 0,
  ctrl = false,
): StyledPiece => ({ text, cls, link, ctrl });

const info = (over: Partial<LinkInfo> = {}): LinkInfo => ({
  href: "/tok/src/tree/B.hs#L3",
  about: "defined in B.hs line 3",
  guess: false,
  external: false,
  here: false,
  ...over,
});

function paint(ps: StyledPiece[], links: Record<number, LinkInfo>) {
  const p = document.createElement("p");
  p.append(...lineNodes(ps, (k) => links[k]));
  return p;
}

describe("lineNodes with links", () => {
  it("pieces of one link go in one <a>, control glyphs too; text 1:1", () => {
    const p = paint(
      [
        piece("a", "variable", 1),
        piece("␇", null, 1, true),
        piece("b", "variable", 1),
        piece(" ", null),
        piece("c", "variable", 2),
        piece("d", "variable", 3),
      ],
      { 1: info(), 3: info() },
    );
    expect(p.textContent).toBe("a␇b cd");
    const as = [...p.querySelectorAll("a")];
    expect(as.map((a) => a.textContent)).toEqual(["a␇b", "d"]);
    expect(as[0]?.querySelector(".code-ctrl")?.textContent).toBe("␇");
    // No LinkInfo: no link.
    const c = p.childNodes[2];
    expect(c instanceof HTMLSpanElement && c.textContent).toBe("c");
  });

  it("two links side by side: two <a>", () => {
    const p = paint([piece("a", "variable", 1), piece("b", "variable", 2)], {
      1: info(),
      2: info(),
    });
    expect([...p.querySelectorAll("a")].map((a) => a.textContent)).toEqual([
      "a",
      "b",
    ]);
  });

  it("attributes: not a Tab stop, no drag, a name with the text", () => {
    const a = paint([piece("area", "variable", 4)], {
      4: info(),
    }).querySelector("a");
    expect(a?.className).toBe("code-link");
    expect(a?.getAttribute("href")).toBe("/tok/src/tree/B.hs#L3");
    expect(a?.getAttribute("tabindex")).toBe("-1");
    expect(a?.getAttribute("draggable")).toBe("false");
    expect(a?.getAttribute("data-link")).toBe("4");
    expect(a?.getAttribute("aria-label")).toBe("area, defined in B.hs line 3");
    expect(a?.hasAttribute("title")).toBe(false);
    // Another file: the target page focuses its line.
    expect(a?.getAttribute("state")).toBe('{"codeJump":true}');
    expect(a?.hasAttribute("target")).toBe(false);
  });

  it("a guess: a class, no title (the hover card says why)", () => {
    const a = paint([piece("x", null, 1)], {
      1: info({ guess: true }),
    }).querySelector("a");
    expect(a?.className).toBe("code-link code-link-guess");
    expect(a?.hasAttribute("title")).toBe(false);
    expect(a?.getAttribute("aria-label")).toBe(
      "x, defined in B.hs line 3, resolved heuristically",
    );
  });

  it("external: a new tab, noopener noreferrer", () => {
    const a = paint([piece("map", null, 1)], {
      1: info({
        external: true,
        href: "https://hoogle.haskell.org/?hoogle=map",
      }),
    }).querySelector("a");
    expect(a?.target).toBe("_blank");
    expect(a?.rel).toBe("noopener noreferrer");
    expect(a?.hasAttribute("state")).toBe(false);
  });

  it("same file: no scroll to the top", () => {
    const a = paint([piece("x", null, 1)], {
      1: info({ here: true }),
    }).querySelector("a");
    expect(a?.hasAttribute("noscroll")).toBe(true);
    expect(a?.hasAttribute("state")).toBe(false);
  });

  it("text is text, never markup", () => {
    const p = paint([piece("<b>x</b>", "variable", 1)], { 1: info() });
    expect(p.querySelector("b")).toBeNull();
    expect(p.textContent).toBe("<b>x</b>");
  });
});

describe("lineNodes, tabbable", () => {
  it("normal Tab stops and no jump state; default: out of the order", () => {
    const ps = [piece("a", "variable", 1)];
    const link = () => info();
    const a = lineNodes(ps, link, { tabbable: true })[0] as HTMLAnchorElement;
    expect(a.getAttribute("tabindex")).toBeNull();
    expect(a.hasAttribute("state")).toBe(false);
    expect(a.hasAttribute("noscroll")).toBe(false);
    const b = lineNodes(ps, link)[0] as HTMLAnchorElement;
    expect(b.getAttribute("tabindex")).toBe("-1");
  });
});
