import { afterEach, beforeEach, describe, expect, it, vi } from "vitest";
import { cbor, cborFixture, fixture, stubFetch } from "../../test/fixtures";
import { mountAppAt, unmountApp } from "../../test/mount";

// Mounts the app at /tok/docs/tree/<ref><hash>, the module API giving
// status and body for apiRef (the ref as the URL has it).
async function open(
  ref: string,
  status: number,
  body: unknown,
  opts: { hash?: string; apiRef?: string } = {},
): Promise<HTMLElement> {
  stubFetch({
    "/tok/api/files/tree": [200, fixture("files")],
    "/tok/api/modules/tree": [200, fixture("modules")],
    [`/tok/api/module/tree/${opts.apiRef ?? ref}`]: [status, body],
  });
  const main = mountAppAt(
    `/tok/docs/tree/${ref}${opts.hash ?? ""}`,
  ).querySelector("main");
  if (main === null) throw new Error("no main");
  await vi.waitFor(() => {
    expect(
      main.querySelector(".decl, .docs-banner, .search-box, .docs-error"),
    ).not.toBeNull();
  });
  return main;
}

const page = () => open("Page", 200, cborFixture("module-page"));

// The section of a declaration, by the text its heading starts with.
function decl(main: HTMLElement, head: string): HTMLElement {
  const s = [...main.querySelectorAll<HTMLElement>("section.decl")].find((el) =>
    el.querySelector(".decl-head .sig")?.textContent.startsWith(head),
  );
  if (s === undefined) throw new Error(`no declaration ${head}`);
  return s;
}

const heads = (main: HTMLElement) =>
  [...main.querySelectorAll("section.decl .decl-head .sig")].map(
    (h) => h.textContent,
  );

let frames: FrameRequestCallback[] = [];

beforeEach(() => {
  frames = [];
  vi.stubGlobal("requestAnimationFrame", (f: FrameRequestCallback) => {
    frames.push(f);
    return frames.length;
  });
  vi.stubGlobal("cancelAnimationFrame", () => undefined);
});

// Runs the pending animation frames (anchors resolve in one).
function flushFrames(): void {
  const fs = frames;
  frames = [];
  for (const f of fs) f(0);
}

afterEach(() => {
  unmountApp();
  vi.unstubAllGlobals();
  vi.restoreAllMocks();
  document.title = "";
});

describe("module page", () => {
  it("header: name, title, doc, language, extensions closed", async () => {
    const main = await page();
    expect(main.querySelector("h1")?.textContent).toBe("Page");
    expect(main.querySelector("h1")?.getAttribute("tabindex")).toBe("-1");
    expect(document.title).toBe("Page — haskell-reviewer");
    const meta = main.querySelector(".docs-meta a");
    expect(meta?.textContent).toBe("Page.hs");
    expect(meta?.getAttribute("href")).toBe("/tok/src/tree/Page.hs");
    const doc = main.querySelector(".doc");
    expect(doc?.textContent).toContain("Every kind of declaration.");
    expect(main.textContent).toContain(
      "Language: GHC2021, with TypeFamilies, TypeData, GADTs",
    );
    const ext = main.querySelector<HTMLDetailsElement>(".docs-extensions");
    expect(ext?.open).toBe(false);
    expect(ext?.querySelector("summary")?.textContent).toBe(
      "All extensions in effect (57)",
    );
  });

  it("declarations in source order, with heads and signatures", async () => {
    const main = await page();
    const hs = heads(main);
    expect(hs.slice(0, 7)).toEqual([
      "data Shape",
      "deriving Eq",
      "deriving Show",
      "newtype Box a",
      "deriving newtype Eq",
      "area :: Shape -> Double",
      "perimeter :: Shape -> Double",
    ]);
    expect(hs).toContain("(<+>) :: Shape -> Shape -> Shape");
    expect(hs).toContain("pattern Unit :: Shape");
    expect(hs).toContain("class Container f");
    expect(hs).toContain("instance Num Bool");
    expect(hs).toContain("type instance Family Shape = Double");
    expect(hs[hs.length - 1]).toBe("$(pure [])");
    // Every top-level declaration: one section each.
    expect(hs).toHaveLength(30);
  });

  it("shared signature: own name, and who it's shared with", async () => {
    const main = await page();
    const p = decl(main, "perimeter");
    expect(p.querySelector(".decl-shared")?.textContent).toBe(
      "Shares its signature with area.",
    );
    expect(p.querySelector(".decl-shared a")?.getAttribute("href")).toBe(
      "/tok/docs/tree/Page#val%3Aarea",
    );
    // The shared doc too.
    expect(p.querySelector(".doc")?.textContent).toBe(
      "Both share one signature.",
    );
    const a = decl(main, "area");
    expect(a.querySelector(".decl-shared")?.textContent).toBe(
      "Shares its signature with perimeter.",
    );
  });

  it("signature parts: cut before top-level arrows", async () => {
    const main = await page();
    const parts = [
      ...decl(main, "(<+>)").querySelectorAll(".decl-head .sig-part"),
    ].map((p) => p.textContent);
    expect(parts).toEqual(["(<+>) :: Shape", "-> Shape", "-> Shape"]);
  });

  it("linked tokens go to the declaration, in this page", async () => {
    const main = await page();
    const a = decl(main, "area").querySelector(".decl-head a");
    expect(a?.textContent).toBe("Shape");
    expect(a?.getAttribute("href")).toBe("/tok/docs/tree/Page#type%3AShape");
    expect(a?.className).toBe("tok-con");
  });

  it("fixity and exported badges", async () => {
    const main = await page();
    const op = decl(main, "(<+>)");
    expect(
      [...op.querySelectorAll(".decl-badge")].map((b) => b.textContent),
    ).toEqual(["infixl 6", "exported"]);
    // Not exported: no label.
    expect(decl(main, "data P2").querySelector(".decl-badge")).toBeNull();
    // Instances: no label (always exported).
    expect(
      decl(main, "instance Num Bool").querySelector(".decl-badge"),
    ).toBeNull();
  });

  it("DEPRECATED on both helpers, shared one resolved", async () => {
    const main = await page();
    for (const h of ["helper ::", "helper2 ::"]) {
      expect(decl(main, h).querySelector(".decl-warning")?.textContent).toBe(
        "Deprecated: Use area",
      );
    }
    expect(
      decl(main, "helper2").querySelector(".decl-shared")?.textContent,
    ).toBe("Shares its signature with helper.");
  });

  it("derived instance: for its type", async () => {
    const main = await page();
    const d = decl(main, "deriving Show");
    expect(d.querySelector(".decl-shared")?.textContent).toBe(
      "Derived for Shape.",
    );
  });

  it("go to source: first code line, not the doc", async () => {
    const main = await page();
    const a = decl(main, "area").querySelector(".decl-actions a");
    expect(a?.getAttribute("href")).toBe("/tok/src/tree/Page.hs#L38");
    expect(a?.textContent).toBe("Go to source: val:area");
    const op = decl(main, "(<+>)").querySelector(".decl-actions a");
    // The fixity line comes first.
    expect(op?.getAttribute("href")).toBe("/tok/src/tree/Page.hs#L42");
  });

  it("rich text in docs: links, names, math, big numbers", async () => {
    const main = await page();
    const doc = main.querySelector(".doc");
    if (doc === null) throw new Error("no doc");
    const hrefs = [...doc.querySelectorAll("a")].map((a) => [
      a.textContent,
      a.getAttribute("href"),
    ]);
    expect(hrefs).toEqual([
      ["area", "/tok/docs/tree/Page#val%3Aarea"],
      ["Sections", "/tok/docs/tree/Sections"],
      ["Sections", "/tok/docs/tree/Sections#v%3Ax"],
      ["the sections", "/tok/docs/tree/Sections"],
      ["the site (opens in a new tab)", "https://example.com/a?b=1"],
    ]);
    expect(doc.textContent).toContain("Missing");
    expect(doc.textContent).toContain("Nowhere");
    expect(doc.textContent).toContain("Math: a + b.");
    expect(doc.querySelector(".doc-number")?.textContent).toBe(
      "9007199254740993.",
    );
    expect(doc.querySelector(".doc-since")?.textContent).toBe(
      "Since: 9007199254740993.2",
    );
  });

  it("anchor: scrolls to the declaration and focuses it", async () => {
    const scroll = vi
      .spyOn(HTMLElement.prototype, "scrollIntoView")
      .mockImplementation(() => undefined);
    const main = await open("Page", 200, cborFixture("module-page"), {
      hash: "#type%3AShape",
    });
    flushFrames();
    const s = decl(main, "data Shape");
    expect(scroll.mock.contexts).toContain(s);
    expect(document.activeElement).toBe(s);
  });

  it("anchor to a nested declaration: its top-level one", async () => {
    vi.spyOn(HTMLElement.prototype, "scrollIntoView").mockImplementation(
      () => undefined,
    );
    const main = await open("Page", 200, cborFixture("module-page"), {
      hash: "#field%3AShape.width",
    });
    flushFrames();
    expect(document.activeElement).toBe(decl(main, "data Shape"));
  });

  it("Haddock anchor: replaced by our id", async () => {
    vi.spyOn(HTMLElement.prototype, "scrollIntoView").mockImplementation(
      () => undefined,
    );
    const main = await open("Page", 200, cborFixture("module-page"), {
      hash: "#v:area",
    });
    flushFrames();
    await vi.waitFor(() => {
      expect(window.location.hash).toBe("#val%3Aarea");
    });
    expect(window.location.pathname).toBe("/tok/docs/tree/Page");
    flushFrames();
    expect(document.activeElement).toBe(decl(main, "area"));
  });

  it("Haddock anchor: replaces the history entry, no push", async () => {
    vi.spyOn(HTMLElement.prototype, "scrollIntoView").mockImplementation(
      () => undefined,
    );
    await open("Page", 200, cborFixture("module-page"), { hash: "#v:area" });
    const push = vi.spyOn(window.history, "pushState");
    const replace = vi.spyOn(window.history, "replaceState");
    flushFrames();
    await vi.waitFor(() => {
      expect(window.location.hash).toBe("#val%3Aarea");
    });
    expect(push).not.toHaveBeenCalled();
    expect(replace).toHaveBeenCalled();
  });

  it.each([
    ["#v:-60--43--62-", "#val%3A%3C%2B%3E"],
    ["#v:%3C%2B%3E", "#val%3A%3C%2B%3E"],
  ])("Haddock anchor %s, escaped or plain", async (hash, ours) => {
    vi.spyOn(HTMLElement.prototype, "scrollIntoView").mockImplementation(
      () => undefined,
    );
    await open("Page", 200, cborFixture("module-page"), { hash });
    flushFrames();
    await vi.waitFor(() => {
      expect(window.location.hash).toBe(ours);
    });
  });

  it("a second click on a link to the current anchor goes there again", async () => {
    vi.spyOn(HTMLElement.prototype, "scrollIntoView").mockImplementation(
      () => undefined,
    );
    const main = await open("Page", 200, cborFixture("module-page"), {
      hash: "#type%3AShape",
    });
    flushFrames();
    const shape = decl(main, "data Shape");
    expect(document.activeElement).toBe(shape);
    const other = decl(main, "area");
    other.focus();
    expect(document.activeElement).toBe(other);
    const link = other.querySelector<HTMLElement>(".decl-head a");
    expect(link?.getAttribute("href")).toBe("/tok/docs/tree/Page#type%3AShape");
    link?.click();
    expect(document.activeElement).toBe(shape);
  });

  it("in-page links don't scroll to the top", async () => {
    vi.spyOn(HTMLElement.prototype, "scrollIntoView").mockImplementation(
      () => undefined,
    );
    const scrollTo = vi
      .spyOn(window, "scrollTo")
      .mockImplementation(() => undefined);
    const main = await page();
    const link = decl(main, "area").querySelector<HTMLElement>(".decl-head a");
    link?.click();
    await vi.waitFor(() => {
      expect(window.location.hash).toBe("#type%3AShape");
    });
    expect(link?.hasAttribute("noscroll")).toBe(true);
    expect(scrollTo).not.toHaveBeenCalled();
  });

  it("sections are labelled by their heading", async () => {
    const main = await page();
    const s = decl(main, "area");
    const id = s.getAttribute("aria-labelledby");
    expect(id).toBeTruthy();
    expect(s.querySelector("h3")?.id).toBe(id);
  });

  it("t: anchors find types and classes", async () => {
    vi.spyOn(HTMLElement.prototype, "scrollIntoView").mockImplementation(
      () => undefined,
    );
    await open("Page", 200, cborFixture("module-page"), {
      hash: "#t:Container",
    });
    flushFrames();
    await vi.waitFor(() => {
      expect(window.location.hash).toBe("#class%3AContainer");
    });
  });

  it("unknown anchor: notice and a prefilled search box", async () => {
    const main = await open("Page", 200, cborFixture("module-page"), {
      hash: "#type%3ANope",
    });
    flushFrames();
    await vi.waitFor(() => {
      expect(main.querySelector(".decl-missing")).not.toBeNull();
    });
    const notice = main.querySelector(".decl-missing");
    expect(notice?.textContent).toContain(
      "No declaration type:Nope in this module, in this snapshot.",
    );
    expect(notice?.querySelector("input")?.value).toBe("Nope");
    // The page is still there.
    expect(main.querySelectorAll("section.decl").length).toBeGreaterThan(0);
  });

  it("bad anchor encoding: notice", async () => {
    const main = await open("Page", 200, cborFixture("module-page"), {
      hash: "#%E0",
    });
    flushFrames();
    await vi.waitFor(() => {
      expect(main.querySelector(".decl-missing")?.textContent).toContain(
        "No declaration %E0",
      );
    });
  });

  it("a hash change doesn't refetch", async () => {
    vi.spyOn(HTMLElement.prototype, "scrollIntoView").mockImplementation(
      () => undefined,
    );
    const main = await page();
    const fetch = vi.mocked(globalThis.fetch);
    const calls = fetch.mock.calls.length;
    decl(main, "area").querySelector<HTMLElement>(".decl-head a")?.click();
    await vi.waitFor(() => {
      expect(window.location.hash).toBe("#type%3AShape");
    });
    flushFrames();
    expect(document.activeElement).toBe(decl(main, "data Shape"));
    expect(fetch.mock.calls.length).toBe(calls);
  });

  it("CPP warnings show as notices", async () => {
    const main = await open(
      "pkg%3Abench%3Aslow%3AMain",
      200,
      cborFixture("module-cpp"),
    );
    expect(main.textContent).toContain("Preprocessor: line 7: #warning slow");
    expect(main.querySelector("h1")?.textContent).toBe("Main");
    expect(document.title).toBe(
      "Main (pkg:bench:slow:Main) — haskell-reviewer",
    );
    expect(main.querySelector(".docs-meta")?.textContent).toBe(
      "pkg 0.1.0 · bench:slow · pkg/bench/Bench.hs",
    );
  });

  it("the ref is kept as typed: in-page links use it", async () => {
    const main = await open("Page.hs", 200, cborFixture("module-page"));
    const a = decl(main, "area").querySelector(".decl-head a");
    expect(a?.getAttribute("href")).toBe("/tok/docs/tree/Page.hs#type%3AShape");
  });
});

describe("text-only pages", () => {
  it.each([
    [
      "module-parse-error",
      "This module doesn't parse: parse error on input `=' (GHC-58481, line 2, column 8). Shown as text.",
      "/tok/src/tree/pkg/app/Main.hs#L2",
    ],
    [
      "module-pragma-error",
      "A pragma at the top of this module is wrong: Unsupported extension: NoSuchThing (GHC-46537, line 1, column 14). Shown as text.",
      "/tok/src/tree/pkg/test/Tests.hs#L1",
    ],
    [
      "module-generated",
      "Generated from this file by alex: shown as text, not indexed.",
      "/tok/src/tree/pkg/src/Lexer.x#L1",
    ],
    [
      "module-preprocessed",
      "Preprocessed by hspec-discover: shown as text, not indexed.",
      "/tok/src/tree/pkg/test/Spec.hs#L1",
    ],
    [
      "module-crashed",
      "The parser crashed on this module: boom. Shown as text.",
      "/tok/src/tree/pkg/src/Foo.hs#L1",
    ],
    [
      "module-too-slow",
      "Parsing this module took too long. Shown as text.",
      "/tok/src/tree/pkg/src/Foo.hs#L1",
    ],
    [
      "module-not-text",
      "This module is not UTF-8 text (10 bytes).",
      "/tok/src/tree/Main.hs#L1",
    ],
    [
      "module-too-large",
      "This module is too large to parse (2,000,000 bytes; the limit is 1 MiB).",
      "/tok/src/tree/src/Data/Deep/Mod.hs#L1",
    ],
    [
      "module-no-source",
      "Listed, but there is no source (file missing).",
      null,
    ],
  ])("%s: banner, View source", async (name, text, href) => {
    const main = await open("M", 200, cborFixture(name));
    const banner = main.querySelector(".docs-banner");
    expect(banner?.querySelector("p")?.textContent).toBe(text);
    const a = banner?.querySelector("a");
    if (href === null) {
      expect(a).toBeNull();
    } else {
      expect(a?.textContent).toBe("View source");
      expect(a?.getAttribute("href")).toBe(href);
    }
    expect(main.querySelector("section.decl")).toBeNull();
  });
});

describe("docs view errors", () => {
  it("404: not found, search box prefilled with the ref", async () => {
    const main = await open("No.Such", 404, fixture("module-not-found"));
    expect(main.querySelector("h1")?.textContent).toBe(
      "Not found in this snapshot",
    );
    expect(main.querySelector("input")?.value).toBe("No.Such");
    expect(document.title).toBe("Not found — haskell-reviewer");
  });

  it("bad payload: an error, the ref as heading", async () => {
    stubFetch({
      "/tok/api/modules/tree": [200, fixture("modules")],
      "/tok/api/module/tree/Foo": [200, cbor({ generation: 1 })],
    });
    const main = mountAppAt("/tok/docs/tree/Foo").querySelector("main");
    await vi.waitFor(() => {
      expect(main?.textContent).toContain("Couldn't load this module.");
    });
    expect(main?.querySelector("h1")?.textContent).toBe("Foo");
  });

  it("HTTP error", async () => {
    stubFetch({
      "/tok/api/module/tree/Foo": [500, "cannot index module"],
    });
    const main = mountAppAt("/tok/docs/tree/Foo").querySelector("main");
    await vi.waitFor(() => {
      expect(main?.textContent).toContain("Couldn't load this module.");
    });
  });

  it.each([
    "/tok/docs/tree",
    "/tok/docs/tree/",
    "/tok/docs/tree/a%2Fb",
    "/tok/docs/tree/%E0",
    "/tok/docs/head/Foo",
  ])("%s: not found, no fetch of a module", async (path) => {
    const fetch = stubFetch({});
    const main = mountAppAt(path).querySelector("main");
    expect(main?.querySelector("h1")?.textContent).toBe(
      "Not found in this snapshot",
    );
    await new Promise((r) => setTimeout(r, 10));
    expect(
      fetch.mock.calls.some(
        ([url]) => typeof url === "string" && url.includes("/api/module/"),
      ),
    ).toBe(false);
  });

  it("loading text only after a delay", async () => {
    vi.stubGlobal(
      "fetch",
      vi.fn(() => new Promise<Response>(() => undefined)),
    );
    const main = mountAppAt("/tok/docs/tree/Foo").querySelector("main");
    expect(main?.textContent).toBe("Foo");
    await vi.waitFor(() => {
      expect(main?.textContent).toBe("FooLoading…");
    });
  });
});
