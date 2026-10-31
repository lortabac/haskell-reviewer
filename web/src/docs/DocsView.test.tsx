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
    el.querySelector(":scope > .decl-head .sig")?.textContent.startsWith(head),
  );
  if (s === undefined) throw new Error(`no declaration ${head}`);
  return s;
}

// The "Instances (k)" list right under a declaration (not a nested
// one's).
function instances(s: HTMLElement): HTMLDetailsElement {
  const d = [
    ...s.querySelectorAll<HTMLDetailsElement>("details.decl-instances"),
  ].find((x) => x.parentElement === s);
  if (d === undefined) throw new Error("no instance list");
  return d;
}

// Opens a closed <details> as a click on its summary would.
function openDetails(d: HTMLDetailsElement): void {
  d.open = true;
  d.dispatchEvent(new Event("toggle"));
}

// The texts of the items of a nested list, by its title.
function items(s: HTMLElement, title: string): string[] {
  const t = [...s.querySelectorAll(".decl-sub-title")].find(
    (x) => x.textContent === title,
  );
  const ul = t?.nextElementSibling;
  if (ul == null) throw new Error(`no list ${title}`);
  return [...ul.children].map(
    (li) =>
      li.querySelector(":scope > .subdecl-head .sig, :scope > .sig")
        ?.textContent ?? "",
  );
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
      "newtype Box a",
      "area, perimeter :: Shape -> Double",
      "perimeter :: Shape -> Double",
      "(<+>) :: Shape -> Shape -> Shape",
      "pattern Unit :: Shape",
      "class Container f",
    ]);
    // Instances are not in the flow: under their homes (closed), or
    // in the Instances block at the end.
    expect(hs).not.toContain("type instance Family Shape = Double");
    expect(hs).not.toContain("deriving Eq");
    expect(hs.slice(-2)).toEqual(["$(pure [])", "instance Num Bool"]);
    // Every other top-level declaration: one section each.
    expect(hs).toHaveLength(21);
    // No headings in the body or the export list: mode C.
    expect([...main.querySelectorAll("h2")].map((h) => h.textContent)).toEqual([
      "Declarations",
      "Instances",
    ]);
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
    // The shared doc: whole under area only, a link here.
    expect(p.querySelector(".doc")).toBeNull();
    expect(
      [...p.querySelectorAll(".decl-shared")].map((x) => x.textContent),
    ).toEqual(["Shares its signature with area.", "Same docs as area."]);
    expect(decl(main, "area").querySelector(".doc")?.textContent).toBe(
      "Both share one signature.",
    );
    // The first shows the signature as written, no link back.
    const a = decl(main, "area");
    expect(a.querySelector(".decl-head .sig")?.textContent).toBe(
      "area, perimeter :: Shape -> Double",
    );
    expect(a.querySelector(".decl-shared")).toBeNull();
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

  it("fixity badges; the export mark is separate (D-203)", async () => {
    const main = await page();
    const op = decl(main, "(<+>)");
    // Exported top-level: no mark at all.
    expect(
      [...op.querySelectorAll(".decl-badge")].map((b) => b.textContent),
    ).toEqual(["infixl 6"]);
    expect(op.querySelector(".decl-mark")).toBeNull();
    // Not exported: a muted "private" mark, not a pill.
    const p2 = decl(main, "data P2");
    expect(p2.querySelector(".decl-badge")).toBeNull();
    expect(p2.querySelector(".decl-mark")?.textContent).toBe("private");
    // Instances: never marked (they always are, D-186).
    const num = decl(main, "instance Num Bool");
    expect(
      [...num.querySelectorAll(".decl-badge")].map((b) => b.textContent),
    ).toEqual(["orphan"]);
    expect(num.querySelector(".decl-mark")).toBeNull();
  });

  it("DEPRECATED on both helpers: whole under the first", async () => {
    const main = await page();
    expect(
      decl(main, "helper, helper2 ::").querySelector(".decl-warning")
        ?.textContent,
    ).toBe("Deprecated: Use area");
    const h2 = decl(main, "helper2");
    expect(h2.querySelector(".decl-warning")).toBeNull();
    expect(
      [...h2.querySelectorAll(".decl-shared")].map((x) => x.textContent),
    ).toEqual(["Shares its signature with helper.", "Same warning as helper."]);
    expect(h2.querySelectorAll(".decl-shared a")[1]?.getAttribute("href")).toBe(
      "/tok/docs/tree/Page#val%3Ahelper",
    );
  });

  it("derived instance under its own type: no 'Derived for'", async () => {
    const main = await page();
    const list = instances(decl(main, "data Shape"));
    openDetails(list);
    const d = decl(list, "deriving Show");
    expect(d.querySelector(".decl-shared")).toBeNull();
  });

  it("go to source: first code line, not the doc", async () => {
    const main = await page();
    const a = decl(main, "area").querySelector(".decl-actions a");
    expect(a?.getAttribute("href")).toBe("/tok/src/tree/Page.hs#L38");
    expect(a?.textContent).toBe("Go to source of area");
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

  it.each([
    ["#field%3AShape.width", "width :: Double"],
    ["#con%3ACircle", "Circle Double"],
    ["#val%3Ainsert", "insert :: Elem f -> f -> f"],
    ["#type%3AElem", "type Elem f :: Type"],
    ["#con%3AGTrue", "GTrue :: Bool -> G Bool"],
  ])("anchor to a nested declaration %s: its own item", async (hash, head) => {
    vi.spyOn(HTMLElement.prototype, "scrollIntoView").mockImplementation(
      () => undefined,
    );
    await open("Page", 200, cborFixture("module-page"), { hash });
    flushFrames();
    const el = document.activeElement;
    expect(el?.className).toBe("subdecl");
    expect(el?.querySelector(".subdecl-head .sig")?.textContent).toBe(head);
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

describe("declaration details", () => {
  it("data type: constructors, record fields with marks, deriving", async () => {
    const main = await page();
    const shape = decl(main, "data Shape");
    expect(items(shape, "Constructors")).toEqual(["Circle Double", "Rect"]);
    const fields = [...shape.querySelectorAll(".subdecl .subdecl .sig")].map(
      (f) => f.textContent,
    );
    expect(fields).toEqual(["width :: Double", "height :: !Double"]);
    expect(items(shape, "Deriving")).toEqual(["deriving (Eq, Show)"]);
    // Exported parent: nested ones say when they aren't (all are here).
    expect(shape.querySelector(".subdecl .decl-badge")).toBeNull();
  });

  it("private nested item of an exported parent (D-203)", async () => {
    const value = structuredClone(fixture("module-page")) as {
      parse: {
        declarations: {
          name: string;
          constructors?: { exported: boolean }[];
        }[];
      };
    };
    const shape = value.parse.declarations.find((d) => d.name === "Shape");
    const circle = shape?.constructors?.[0];
    if (circle === undefined) throw new Error("no Circle");
    circle.exported = false;
    const main = await open("Page", 200, cbor(value));
    const c = decl(main, "data Shape").querySelector(".subdecl");
    expect(c?.querySelector(".decl-mark")?.textContent).toBe("private");
    expect(c?.querySelector(".decl-badge")).toBeNull();
  });

  it("exported nested item under a kept private parent (D-203)", async () => {
    const value = structuredClone(fixture("module-page")) as {
      parse: {
        declarations: { name: string; exported?: boolean }[];
      };
    };
    const container = value.parse.declarations.find(
      (d) => d.name === "Container",
    );
    if (container === undefined) throw new Error("no Container");
    // Unexported itself, but its methods stay exported: the class is
    // kept (exportsNested), and marked "private"; its methods differ
    // from it, so they're marked "exported" (D-203).
    container.exported = false;
    const main = await open("Page", 200, cbor(value));
    const c = decl(main, "class Container f");
    expect(c.querySelector(".decl-head .decl-mark")?.textContent).toBe(
      "private",
    );
    const m = c.querySelector(".subdecl .decl-mark");
    expect(m?.textContent).toBe("exported");
  });

  it("newtype deriving via, GADT shared line, shared field type", async () => {
    const main = await page();
    expect(items(decl(main, "newtype V"), "Deriving")).toEqual([
      "deriving (Eq, Ord) via Int",
    ]);
    const g = decl(main, "data G a where");
    expect(items(g, "Constructors")).toEqual([
      "GInt :: {gval :: Int} -> G Int",
      "GBool, GTrue :: Bool -> G Bool",
      "GTrue :: Bool -> G Bool",
    ]);
    const shared = [...g.querySelectorAll(".subdecl .decl-shared")].map(
      (p) => p.textContent,
    );
    expect(shared).toEqual(["Shares its signature with GBool."]);
    const p2 = decl(main, "data P2");
    expect(
      [...p2.querySelectorAll(".subdecl .subdecl .sig")].map(
        (f) => f.textContent,
      ),
    ).toEqual(["px :: Double", "py :: Double"]);
    // fh's doc is ff's (sent once).
    const fld = decl(main, "data Fld");
    const fh = [...fld.querySelectorAll<HTMLElement>(".subdecl .subdecl")].find(
      (f) => f.querySelector(".sig")?.textContent === "fh :: Int",
    );
    // fh's doc is sent by ff (of FB): a link to it.
    expect(fh?.querySelector(".doc")).toBeNull();
    expect(fh?.querySelector(".decl-shared")?.textContent).toBe(
      "Same docs as ff.",
    );
  });

  it("class: associated types, methods, MINIMAL", async () => {
    const main = await page();
    const c = decl(main, "class Container f");
    expect(items(c, "Associated types")).toEqual(["type Elem f :: Type"]);
    expect(items(c, "Methods")).toEqual([
      "empty :: f",
      "insert :: Elem f -> f -> f",
    ]);
    expect(c.textContent).toContain("{-# MINIMAL empty, insert #-}");
  });

  it("closed family equations, synonym, type data, splice", async () => {
    const main = await page();
    expect(
      items(decl(main, "type family Closed a where"), "Equations"),
    ).toEqual(["Closed Int = Bool", "Closed a = a"]);
    expect(items(decl(main, "type data Nat"), "Constructors")).toEqual([
      "Zero",
      "Succ Nat",
    ]);
    expect(
      decl(main, "type Area = Double").querySelector(".decl-sub-title"),
    ).toBeNull();
    expect(decl(main, "$(pure [])").textContent).toContain(
      "Template Haskell splice.",
    );
  });

  it("pattern synonym: direction", async () => {
    const main = await page();
    expect(
      decl(main, "pattern Unit").querySelector(".decl-note")?.textContent,
    ).toBe("Bidirectional: matches and builds.");
  });
});

describe("instances", () => {
  it("closed lists under types and classes, built when open", async () => {
    const main = await page();
    const counts = (head: string) => {
      const d = instances(decl(main, head));
      expect(d.open).toBe(false);
      // Nothing inside until opened.
      expect(d.querySelector("section")).toBeNull();
      return d.querySelector("summary")?.textContent;
    };
    expect(counts("data Shape")).toBe("Instances (3)");
    expect(counts("newtype Box a")).toBe("Instances (2)");
    expect(counts("class Container f")).toBe("Instances (1)");
    expect(counts("type family Family a")).toBe("Instances (1)");
    const list = instances(decl(main, "data Shape"));
    openDetails(list);
    expect(heads(list)).toEqual([
      "deriving Eq",
      "deriving Show",
      "instance Ord Shape",
    ]);
  });

  it("an instance with an associated type, and the family's list", async () => {
    const main = await page();
    const list = instances(decl(main, "class Container f"));
    openDetails(list);
    const inst = decl(list, "instance Container [a]");
    expect(items(inst, "Associated types")).toEqual(["type Elem [a] = a"]);
    // The associated family lists the type instance too.
    const elem = main.querySelector<HTMLElement>(".subdecl[data-handle='17']");
    if (elem === null) throw new Error("no Elem");
    const elems = instances(elem);
    expect(elems.querySelector("summary")?.textContent).toBe("Instances (1)");
    openDetails(elems);
    expect(heads(elems)).toEqual(["type Elem [a] = a"]);
  });

  it("family instance under its family", async () => {
    const main = await page();
    const list = instances(decl(main, "type family Family a"));
    openDetails(list);
    expect(heads(list)).toEqual(["type instance Family Shape = Double"]);
  });

  it("unattached instance: in the Instances block, orphan", async () => {
    const main = await page();
    const h2 = [...main.querySelectorAll("h2")].find(
      (h) => h.textContent === "Instances",
    );
    const num = decl(main, "instance Num Bool");
    expect(
      h2 !== undefined &&
        h2.compareDocumentPosition(num) & Node.DOCUMENT_POSITION_FOLLOWING,
    ).toBeTruthy();
    expect(num.querySelector("h3")).not.toBeNull();
  });

  it.each([
    ["#inst%3AOrd%3AShape", "data Shape", "instance Ord Shape"],
    [
      "#inst%3AFamily%3AShape",
      "type family Family a",
      "type instance Family Shape = Double",
    ],
    [
      "#inst%3AContainer%3A%5Ba%5D",
      "class Container f",
      "instance Container [a]",
    ],
  ])("instance anchor %s opens its home's list", async (hash, home, head) => {
    const scroll = vi
      .spyOn(HTMLElement.prototype, "scrollIntoView")
      .mockImplementation(() => undefined);
    const main = await open("Page", 200, cborFixture("module-page"), { hash });
    flushFrames();
    const list = instances(decl(main, home));
    expect(list.open).toBe(true);
    const s = decl(list, head);
    expect(document.activeElement).toBe(s);
    expect(scroll.mock.contexts).toContain(s);
  });

  it("associated family instance anchor: in the family's list", async () => {
    vi.spyOn(HTMLElement.prototype, "scrollIntoView").mockImplementation(
      () => undefined,
    );
    const main = await open("Page", 200, cborFixture("module-page"), {
      hash: "#inst%3AElem%3A%5Ba%5D",
    });
    flushFrames();
    const elem = main.querySelector<HTMLElement>(".subdecl[data-handle='17']");
    if (elem === null) throw new Error("no Elem");
    const list = instances(elem);
    expect(list.open).toBe(true);
    expect(document.activeElement).toBe(decl(list, "type Elem [a] = a"));
  });
});

describe("synopsis, pragmas, source", () => {
  it("synopsis: closed, one line per type, class, value, patsyn", async () => {
    const main = await page();
    const syn = main.querySelector<HTMLDetailsElement>(".docs-synopsis");
    if (syn === null) throw new Error("no synopsis");
    expect(syn.open).toBe(false);
    expect(syn.querySelector("summary")?.textContent).toBe("Synopsis (19)");
    expect(syn.querySelector("li")).toBeNull();
    openDetails(syn);
    const links = [...syn.querySelectorAll("li a")];
    expect(
      links.slice(0, 3).map((a) => [a.textContent, a.getAttribute("href")]),
    ).toEqual([
      ["data Shape", "/tok/docs/tree/Page#type%3AShape"],
      ["newtype Box a", "/tok/docs/tree/Page#type%3ABox"],
      ["area, perimeter :: Shape -> Double", "/tok/docs/tree/Page#val%3Aarea"],
    ]);
    expect(links.map((a) => a.textContent)).toContain("pattern Unit :: Shape");
    expect(links.map((a) => a.textContent)).not.toContain("$(pure [])");
  });

  it("pragmas: closed block at the end, 4 entries", async () => {
    const main = await page();
    const p = main.querySelector<HTMLDetailsElement>(".docs-pragmas");
    if (p === null) throw new Error("no pragmas");
    expect(p.open).toBe(false);
    expect(p.querySelector("summary")?.textContent).toBe("Pragmas (4)");
    expect(main.lastElementChild?.lastElementChild).toBe(p);
    openDetails(p);
    expect([...p.querySelectorAll("li")].map((l) => l.textContent)).toEqual([
      '{-# RULES "area/id" forall s. area s = area s #-}',
      '{-# ANN helper "note" #-}',
      "{-# COMPLETE Unit #-}",
      "default (Int)",
    ]);
  });

  it("source toggle: line numbers and a gap row", async () => {
    const main = await page();
    const op = decl(main, "(<+>)");
    const button = op.querySelector<HTMLButtonElement>(".decl-source-toggle");
    if (button === null) throw new Error("no toggle");
    expect(button.getAttribute("aria-expanded")).toBe("false");
    const box = main.querySelector(
      `#${button.getAttribute("aria-controls") ?? ""}`,
    );
    expect(box?.textContent).toBe("");
    button.click();
    expect(button.getAttribute("aria-expanded")).toBe("true");
    expect(button.textContent).toBe("Hide source of <+>");
    expect(box?.querySelector("pre")?.textContent).toBe(
      [
        "42infixl 6 <+>",
        "⋯ line 43",
        "44(<+>) :: Shape -> Shape -> Shape",
        "45a <+> _ = a",
        "",
      ].join("\n"),
    );
    button.click();
    expect(box?.textContent).toBe("");
  });

  it("source of a shared signature: the shared part resolved", async () => {
    const main = await page();
    const p = decl(main, "perimeter");
    p.querySelector<HTMLButtonElement>(".decl-source-toggle")?.click();
    expect(p.querySelector("pre")?.textContent).toContain(
      "area, perimeter :: Shape -> Double",
    );
  });
});

describe("sections", () => {
  it("mode A: body headings, chunks in place", async () => {
    const main = await open(
      "Sections",
      200,
      cborFixture("module-page-sections"),
    );
    const order = [
      ...main.querySelectorAll(
        ".docs-page h2, .docs-page section.decl, .docs-page > .doc",
      ),
    ].map((e) =>
      e.matches("section")
        ? (e.querySelector(".sig")?.textContent ?? "")
        : e.textContent,
    );
    expect(order).toEqual([
      "Sections.",
      "Body heading",
      "A named chunk, about x.",
      "x :: Int",
      "y :: Int",
    ]);
    expect(decl(main, "x ::").querySelector("h3")).not.toBeNull();
  });

  it("mode B: export list order, re-exports, Private", async () => {
    const value = structuredClone(fixture("module-page-sections")) as {
      parse: {
        exports: Record<string, unknown>[];
        docItems: { kind: string }[];
        declarations: { name: string; exported: boolean }[];
      };
    };
    const p = value.parse;
    p.docItems = p.docItems.filter((i) => i.kind !== "heading");
    const y = p.declarations.find((d) => d.name === "y");
    if (y === undefined) throw new Error("no y");
    y.exported = false;
    const span = [1, 1, 1, 2];
    p.exports = p.exports.filter(
      (e) => (e.name as { occ?: string } | undefined)?.occ !== "y",
    );
    p.exports.push(
      {
        kind: "item",
        span,
        name: { span, qualifier: null, occ: "<|>", keyword: null },
        subs: { kind: "none" },
        doc: null,
        warning: null,
      },
      {
        kind: "module",
        name: { span, value: "Data.List" },
        warning: null,
      },
    );
    const main = await open("Sections", 200, cbor(value));
    const order = [
      ...main.querySelectorAll(
        ".docs-page .docs-section, .docs-page section.decl, .docs-page > .doc, .docs-reexport",
      ),
    ].map((e) =>
      e.matches("section")
        ? `decl ${e.querySelector(".sig")?.textContent ?? ""}`
        : e.textContent,
    );
    expect(order).toEqual([
      "Sections.",
      "Values",
      "Some values.",
      "decl x :: Int",
      "More",
      "A named chunk, about x.",
      "(<|>) (re-exported)",
      "module Data.List (re-exported)",
      "Private",
      "decl y :: Int",
    ]);
    // Section levels: h2 for level 1, h3 for level 2; decls one below.
    expect(main.querySelector("h3.docs-section")?.textContent).toBe("More");
    expect(decl(main, "y ::").querySelector("h3")).not.toBeNull();
    expect(decl(main, "x ::").querySelector("h3")).not.toBeNull();
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

describe("exported only toggle (D-204)", () => {
  it("checkbox hides unexported top-level declarations, status counts them", async () => {
    const main = await page();
    const box = main.querySelector<HTMLInputElement>(
      ".docs-toolbar input[type=checkbox]",
    );
    if (box === null) throw new Error("no checkbox");
    expect(box.checked).toBe(false);
    expect(heads(main)).toContain("data P2");
    box.click();
    expect(box.checked).toBe(true);
    expect(heads(main)).not.toContain("data P2");
    const status = main.querySelector(".docs-toolbar-status");
    expect(status?.getAttribute("aria-live")).toBe("polite");
    expect(status?.textContent).toMatch(/^\d+ declarations? hidden$/);
  });

  it("toggling recomputes the current anchor: still shown, no scroll", async () => {
    const scroll = vi
      .spyOn(HTMLElement.prototype, "scrollIntoView")
      .mockImplementation(() => undefined);
    const main = await open("Page", 200, cborFixture("module-page"), {
      hash: "#type%3AShape",
    });
    flushFrames();
    expect(document.activeElement).toBe(decl(main, "data Shape"));
    scroll.mockClear();
    main
      .querySelector<HTMLInputElement>(".docs-toolbar input[type=checkbox]")
      ?.click();
    flushFrames();
    // Shape stays exported and shown: no "hidden by Exported only"
    // notice, and the toggle itself never scrolls (D-204).
    expect(main.querySelector(".decl-missing")).toBeNull();
    expect(scroll).not.toHaveBeenCalled();
    expect(decl(main, "data Shape")).not.toBeNull();
  });

  it("a hidden anchor: notice with its name, Show all reveals and focuses it", async () => {
    const scroll = vi
      .spyOn(HTMLElement.prototype, "scrollIntoView")
      .mockImplementation(() => undefined);
    const main = await open("Page", 200, cborFixture("module-page"), {
      hash: "#type%3AP2",
    });
    flushFrames();
    main
      .querySelector<HTMLInputElement>(".docs-toolbar input[type=checkbox]")
      ?.click();
    flushFrames();
    const notice = main.querySelector(".decl-missing");
    expect(notice?.textContent).toContain("P2");
    expect(notice?.textContent).toContain("is hidden by Exported only.");
    expect(main.querySelector("section.decl[data-handle]")).not.toBeNull();
    scroll.mockClear();
    notice?.querySelector<HTMLButtonElement>("button")?.click();
    flushFrames();
    expect(
      main.querySelector<HTMLInputElement>(".docs-toolbar input[type=checkbox]")
        ?.checked,
    ).toBe(false);
    expect(main.querySelector(".decl-missing")).toBeNull();
    const p2 = decl(main, "data P2");
    expect(document.activeElement).toBe(p2);
    expect(scroll.mock.contexts).toContain(p2);
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
