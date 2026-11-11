import { afterEach, beforeEach, describe, expect, it, vi } from "vitest";
import { cbor, cborFixture, fixture, stubFetch } from "../../test/fixtures";
import { fakeLayout } from "../../test/layout";
import { fallbackRowHeight as h } from "../source/CodeView";
import { mountAppAt, unmountApp } from "../../test/mount";

// Mounts the app at /tok/src/tree/<path>, with the API answering
// status and body for that file. No module tree unless given. No
// highlighting (api/source 404) unless given.
async function open(
  path: string,
  status: number,
  body: unknown,
  modules: [number, unknown] = [404, "not found"],
  source: [number, unknown] = [404, "not found"],
): Promise<HTMLElement> {
  const fetch = stubFetch({
    "/tok/api/files/tree": [200, fixture("files")],
    "/tok/api/modules/tree": modules,
    [`/tok/api/file/tree/${path}`]: [status, body],
    [`/tok/api/source/tree/${path}`]: source,
  });
  const main = mountAppAt(`/tok/src/tree/${path}`).querySelector("main");
  if (main === null) throw new Error("no main");
  await vi.waitFor(() => {
    expect(fetch.mock.calls.map((c) => c[0] as string)).toEqual(
      expect.arrayContaining([
        "/tok/api/files/tree",
        "/tok/api/modules/tree",
        `/tok/api/file/tree/${path}`,
        `/tok/api/source/tree/${path}`,
      ]),
    );
    expect(main.querySelector("h1, p")).not.toBeNull();
  });
  // Let the resource settle.
  await vi.waitFor(() => {
    expect(main.textContent).not.toBe(path);
  });
  return main;
}

beforeEach(() => fakeLayout());

afterEach(() => {
  unmountApp();
  vi.unstubAllGlobals();
  vi.restoreAllMocks();
  document.title = "";
});

// The code of each mounted row.
const rows = (main: HTMLElement) =>
  [...main.querySelectorAll(".code-rows .code-cell")].map((c) => c.textContent);

describe("FileView", () => {
  it("shows text as text, with line numbers", async () => {
    const main = await open("app/Main.hs", 200, cborFixture("file-text"));
    expect(main.querySelector("h1")?.textContent).toBe("app/Main.hs");
    expect(document.title).toBe("app/Main.hs \u2014 haskell-reviewer");
    const grid = main.querySelector('[role="grid"]');
    expect(grid?.getAttribute("aria-label")).toBe("File contents");
    expect(grid?.getAttribute("aria-rowcount")).toBe("5");
    // CRLF: not part of the lines.
    expect(rows(main)).toEqual([
      "module Main (main) where",
      "",
      "-- <script>alert(1)</script>",
      "main :: IO ()",
      'main = putStrLn "h\u00e9llo, w\u00f6rld \u2192 \u2713"',
    ]);
    expect(
      [...main.querySelectorAll('[role="row"]')].map((r) =>
        r.getAttribute("aria-rowindex"),
      ),
    ).toEqual(["1", "2", "3", "4", "5"]);
    expect(
      [...main.querySelectorAll('[role="rowheader"]')].map((r) =>
        r.getAttribute("aria-label"),
      ),
    ).toEqual(["1", "2", "3", "4", "5"]);
    // Text only: no script element, and no node with the whole file.
    expect(document.querySelector("script")).toBeNull();
    expect(main.querySelector(".code-row .code-cell *")).toBeNull();
    expect(
      [...main.querySelectorAll(".code-cell")].some((c) =>
        c.textContent.includes("\n"),
      ),
    ).toBe(false);
    // One Tab stop: the first line.
    const stops = main.querySelectorAll('.code-cell[tabindex="0"]');
    expect(stops).toHaveLength(1);
    expect(stops[0]?.textContent).toBe("module Main (main) where");
  });

  it("an empty file", async () => {
    const main = await open(
      "empty.txt",
      200,
      cbor({ generation: 1, kind: "text", text: "" }),
    );
    expect(main.textContent).toContain("This file is empty.");
    expect(main.querySelector('[role="grid"]')).toBeNull();
  });

  it("decodes the path from the URL", async () => {
    const main = await open(
      "a%20b/%C3%A9%23.hs",
      200,
      cborFixture("file-text"),
    );
    expect(main.querySelector("h1")?.textContent).toBe("a b/é#.hs");
  });

  it("binary", async () => {
    const main = await open("assets/logo.png", 200, cborFixture("file-binary"));
    expect(main.textContent).toContain(
      "This file is not UTF-8 text, so it isn't shown (2,048 bytes).",
    );
    expect(main.querySelector("pre")).toBeNull();
  });

  it("too large", async () => {
    const main = await open("data/big.csv", 200, cborFixture("file-too-large"));
    expect(main.textContent).toContain(
      "This file is too large to show (5,242,880 bytes; the limit is 1 MiB).",
    );
  });

  it("symlink, resolved: links to the target file", async () => {
    const main = await open("docs/guide.md", 200, cborFixture("file-symlink"));
    expect(main.textContent).toContain("Symbolic link to ../shared/guide.md");
    const a = main.querySelector("a");
    expect(a?.getAttribute("href")).toBe("/tok/src/tree/shared/guide.md");
  });

  it("symlink, not resolved: no link", async () => {
    const main = await open(
      "docs/old.md",
      200,
      cbor({
        generation: 1,
        kind: "symlink",
        target: "/etc/passwd",
        resolved: null,
      }),
    );
    expect(main.textContent).toContain("Symbolic link to /etc/passwd");
    expect(main.querySelector("a")).toBeNull();
  });

  it("submodule", async () => {
    const main = await open("vendor/dep", 200, cborFixture("file-submodule"));
    expect(main.textContent).toContain(
      "Submodule at commit 0123456789abcdef0123456789abcdef01234567. Its files aren't shown.",
    );
  });

  it("nested repo", async () => {
    const main = await open(
      "vendor/nested",
      200,
      cbor({ generation: 1, kind: "submodule", commit: null }),
    );
    expect(main.textContent).toContain(
      "Nested git repository. Its files aren't shown.",
    );
  });

  it("404: not found", async () => {
    const main = await open("gone.hs", 404, "not found");
    expect(main.querySelector("h1")?.textContent).toBe(
      "Not found in this snapshot",
    );
    expect(document.title).toBe("Not found — haskell-reviewer");
  });

  it.each([
    [
      "pkg/src/Lexer.x",
      "Generated from this file by alex: shown as text, not indexed.",
    ],
    [
      "pkg/test/Spec.hs",
      "Preprocessed by hspec-discover: shown as text, not indexed.",
    ],
  ])("banner for %s", async (path, text) => {
    const main = await open(path, 200, cborFixture("file-text"), [
      200,
      fixture("modules"),
    ]);
    await vi.waitFor(() => {
      expect(main.querySelector(".file-banner")?.textContent).toBe(text);
    });
    // Right under the heading.
    expect(main.querySelector("h1 + .file-banner")).not.toBeNull();
  });

  it.each([
    ["a plain module", [200, fixture("modules")]],
    ["no module tree", [404, "not found"]],
  ] as [string, [number, unknown]][])("no banner: %s", async (_, modules) => {
    const main = await open(
      "pkg/src/Foo.hs",
      200,
      cborFixture("file-text"),
      modules,
    );
    await vi.waitFor(() => {
      expect(main.querySelector(".code-view")).not.toBeNull();
    });
    await new Promise((r) => setTimeout(r, 20));
    expect(main.querySelector(".file-banner")).toBeNull();
  });

  it("other errors", async () => {
    const main = await open("x.hs", 500, "cannot read file");
    expect(main.textContent).toContain("Couldn't load this file.");
  });

  it("bad payload", async () => {
    const main = await open("x.hs", 200, cbor({ kind: "text" }));
    expect(main.textContent).toContain("Couldn't load this file.");
  });

  it("JSON instead of CBOR", async () => {
    const main = await open("x.hs", 200, fixture("file-text"));
    expect(main.textContent).toContain("Couldn't load this file.");
  });
});

describe("FileView loading", () => {
  it("shows a loading text only after a delay", async () => {
    vi.stubGlobal(
      "fetch",
      vi.fn(() => new Promise<Response>(() => undefined)),
    );
    const main = mountAppAt("/tok/src/tree/a.hs").querySelector("main");
    expect(main?.textContent).toBe("a.hs");
    await vi.waitFor(() => {
      expect(main?.textContent).toBe("a.hsLoading…");
    });
  });
});

describe("FileView navigation", () => {
  // Waits for the main heading to say text.
  async function heading(main: HTMLElement, text: string) {
    await vi.waitFor(() => {
      expect(main.querySelector("h1")?.textContent).toBe(text);
    });
  }

  it("a sidebar link navigates under the token, focus stays", async () => {
    stubFetch({
      "/tok/api/files/tree": [200, fixture("files")],
      "/tok/api/file/tree/app/Main.hs": [200, cborFixture("file-text")],
    });
    const root = mountAppAt("/tok");
    const nav = root.querySelector("nav");
    const main = root.querySelector("main");
    if (nav === null || main === null) throw new Error("no frame");
    await vi.waitFor(() => {
      expect(nav.querySelector(".file-tree button")?.textContent).toBe("app");
    });
    nav.querySelector<HTMLElement>(".file-tree button")?.click();
    const link = nav.querySelector<HTMLElement>(".file-tree a");
    if (link === null) throw new Error("no link");
    link.focus();
    link.click();
    await heading(main, "app/Main.hs");
    expect(window.location.pathname).toBe("/tok/src/tree/app/Main.hs");
    await vi.waitFor(() => {
      expect(main.querySelector(".code-view")).not.toBeNull();
    });
    expect(document.activeElement).toBe(link);
  });

  it("a link in main moves focus to the new heading", async () => {
    stubFetch({
      "/tok/api/files/tree": [200, fixture("files")],
      "/tok/api/file/tree/docs/guide.md": [200, cborFixture("file-symlink")],
      "/tok/api/file/tree/shared/guide.md": [200, cborFixture("file-text")],
    });
    const main = mountAppAt("/tok/src/tree/docs/guide.md").querySelector(
      "main",
    );
    if (main === null) throw new Error("no main");
    await vi.waitFor(() => {
      expect(main.querySelector("a")).not.toBeNull();
    });
    const link = main.querySelector("a");
    link?.focus();
    link?.click();
    await heading(main, "shared/guide.md");
    const h1 = main.querySelector("h1");
    expect(h1?.getAttribute("tabindex")).toBe("-1");
    expect(document.activeElement).toBe(h1);
  });

  it("a late reply for the old file never shows", async () => {
    let late: ((r: Response) => void) | undefined;
    const json = (body: unknown) =>
      new Response(JSON.stringify(body), { status: 200 });
    const ok = (bytes: Uint8Array<ArrayBuffer>) =>
      new Response(bytes, { status: 200 });
    vi.stubGlobal(
      "fetch",
      vi.fn((url: string): Promise<Response> => {
        if (url === "/tok/api/files/tree") {
          return Promise.resolve(json(fixture("files")));
        }
        if (url === "/tok/api/file/tree/app/Main.hs") {
          return new Promise((resolve) => (late = resolve));
        }
        return Promise.resolve(
          ok(cbor({ generation: 1, kind: "text", text: "new file\n" })),
        );
      }),
    );
    const root = mountAppAt("/tok/src/tree/app/Main.hs");
    const main = root.querySelector("main");
    if (main === null) throw new Error("no main");
    await vi.waitFor(() => {
      expect(late).toBeDefined();
      expect(
        root.querySelectorAll("nav .file-tree button").length,
      ).toBeGreaterThan(0);
    });
    // Go to another file, through the router.
    [...root.querySelectorAll<HTMLElement>("nav .file-tree button")]
      .find((b) => b.textContent === "shared")
      ?.click();
    root
      .querySelector<HTMLElement>('nav a[href="/tok/src/tree/shared/guide.md"]')
      ?.click();
    await vi.waitFor(() => {
      expect(rows(main)).toEqual(["new file"]);
    });
    late?.(ok(cborFixture("file-text")));
    await new Promise((r) => setTimeout(r, 50));
    expect(main.querySelector("h1")?.textContent).toBe("shared/guide.md");
    expect(rows(main)).toEqual(["new file"]);
  });
});

describe("FileView #L<n>", () => {
  let frames: FrameRequestCallback[] = [];

  beforeEach(() => {
    frames = [];
    vi.stubGlobal("requestAnimationFrame", (f: FrameRequestCallback) => {
      frames.push(f);
      return frames.length;
    });
    vi.stubGlobal("cancelAnimationFrame", () => undefined);
  });

  function flush(): void {
    const fs = frames;
    frames = [];
    for (const f of fs) f(0);
  }

  const marked = (main: HTMLElement) =>
    [...main.querySelectorAll(".code-row.code-line-target")].map((e) =>
      e.getAttribute("data-line"),
    );
  const status = (main: HTMLElement) =>
    main.querySelector('.file-text [role="status"]')?.textContent;
  const stop = (main: HTMLElement) =>
    main
      .querySelector('.code-cell[tabindex="0"]')
      ?.parentElement?.getAttribute("data-line");

  async function at(hash: string, text?: string): Promise<HTMLElement> {
    stubFetch({
      "/tok/api/files/tree": [200, fixture("files")],
      "/tok/api/file/tree/app/Main.hs": [
        200,
        text === undefined
          ? cborFixture("file-text")
          : cbor({ generation: 1, kind: "text", text }),
      ],
    });
    const main = mountAppAt(`/tok/src/tree/app/Main.hs${hash}`).querySelector(
      "main",
    );
    if (main === null) throw new Error("no main");
    await vi.waitFor(() => {
      expect(main.querySelector(".code-view")).not.toBeNull();
    });
    return main;
  }

  const scrollSpy = () =>
    vi
      .spyOn(HTMLElement.prototype, "scrollIntoView")
      .mockImplementation(() => undefined);

  it("marks the line, makes it the cursor, page scroll after a frame", async () => {
    const scroll = vi
      .spyOn(window, "scrollBy")
      .mockImplementation(() => undefined);
    const main = await at("#L3");
    expect(marked(main)).toEqual(["3"]);
    expect(stop(main)).toBe("3");
    expect(status(main)).toBe("Line 3 marked.");
    // The row is below the window (happy-dom: all rects at 0, so fake it).
    const row = main.querySelector(".code-row.code-line-target");
    if (row === null) throw new Error("no row");
    vi.spyOn(row, "getBoundingClientRect").mockReturnValue(
      new DOMRect(0, window.innerHeight + 50, 100, h),
    );
    expect(scroll).not.toHaveBeenCalled();
    flush();
    expect(scroll).toHaveBeenCalledWith(0, 50 + h);
    // No focus moved.
    expect(document.activeElement?.closest(".code-view")).toBeNull();
  });

  it("a line far down: mounted, and the view scrolls to it", async () => {
    const text = Array.from({ length: 20000 }, (_, i) => `l${i + 1}`).join(
      "\n",
    );
    const main = await at("#L12345", text);
    expect(marked(main)).toEqual(["12345"]);
    expect(stop(main)).toBe("12345");
    const view = main.querySelector<HTMLElement>(".code-view");
    if (view === null) throw new Error("no view");
    // Row 12345 is inside the 600 px box.
    const top = 12344 * h;
    await vi.waitFor(() => {
      flush();
      expect(view.scrollTop).toBeLessThanOrEqual(top);
      expect(view.scrollTop + 600).toBeGreaterThanOrEqual(top + h);
    });
  });

  it("a range; past the end is cut", async () => {
    const main = await at("#L2-L4");
    expect(marked(main)).toEqual(["2", "3", "4"]);
    expect(status(main)).toBe("Lines 2\u20134 marked.");
    unmountApp();
    const cut = await at("#L4-L99");
    expect(marked(cut)).toEqual(["4", "5"]);
    expect(status(cut)).toBe("Lines 4\u20135 marked.");
  });

  // #L0: no line 0; #L6 and #L9: the file has 5 lines.
  it.each(["#L0", "#L6", "#L9", "#L6-L8", "#L4-L2", "#junk"])(
    "%s: nothing marked",
    async (hash) => {
      const scroll = scrollSpy();
      const main = await at(hash);
      flush();
      expect(marked(main)).toEqual([]);
      expect(stop(main)).toBe("1");
      // Only the sidebar scrolls (to the current file), not the view.
      expect(
        scroll.mock.contexts.filter(
          (el) => el instanceof Element && el.closest(".code-view") !== null,
        ),
      ).toEqual([]);
      expect(status(main)).toBe("");
    },
  );

  it("a hash change moves the mark, no refetch", async () => {
    const main = await at("#L1");
    const fetch = vi.mocked(globalThis.fetch);
    const calls = fetch.mock.calls.length;
    go(main, "#L5");
    await vi.waitFor(() => {
      expect(marked(main)).toEqual(["5"]);
    });
    expect(stop(main)).toBe("5");
    expect(fetch.mock.calls.length).toBe(calls);
  });

  it("a hash change with focus on a line: focus moves to the mark", async () => {
    const text = Array.from({ length: 20000 }, (_, i) => `l${i + 1}`).join(
      "\n",
    );
    const main = await at("#L1", text);
    main
      .querySelector<HTMLElement>('.code-row[data-line="1"] .code-cell')
      ?.focus();
    go(main, "#L15000");
    await vi.waitFor(() => {
      expect(marked(main)).toEqual(["15000"]);
    });
    expect(
      document.activeElement?.closest(".code-row")?.getAttribute("data-line"),
    ).toBe("15000");
  });
});

// Follow a link to hash on this page, through the router.
function go(main: HTMLElement, hash: string): void {
  const a = document.createElement("a");
  a.href = `/tok/src/tree/app/Main.hs${hash}`;
  main.append(a);
  a.click();
}

describe("FileView highlighting", () => {
  const lexed = (over: Record<string, unknown> = {}) => ({
    generation: 1,
    path: "app/Main.hs",
    ref: "Main",
    tier: { kind: "syntactic", reason: "no-hie" },
    partial: false,
    highlight: "lexed",
    // module (keyword), Main (typeCon), line 3 excluded (a region run),
    // the string on line 5 (literal; non-ASCII inside).
    runs: [
      1, 1, 0, 7, 0, 0, 0, 8, 0, 12, 1, 0, 2, 1, 1, 1, 7, 0, 2, 17, 0, 35, 4, 0,
    ],
    regions: [1, 3, 3],
    targets: [],
    outline: [
      ["value", "val:main", "main", 4, 4, 5],
      ["instance", "inst:Show:T", "instance Show T", 5, 5, 5],
    ],
    ...over,
  });

  // Mounts app/Main.hs (file-text fixture); api/source answers when
  // answer() is called.
  async function openLate(status: number, body: unknown) {
    let answer: (() => void) | undefined;
    const routes = stubFetch({
      "/tok/api/files/tree": [200, fixture("files")],
      "/tok/api/file/tree/app/Main.hs": [200, cborFixture("file-text")],
    });
    const plain = routes.getMockImplementation()!;
    routes.mockImplementation((input) => {
      if (input === "/tok/api/source/tree/app/Main.hs") {
        return new Promise((resolve) => {
          answer = () =>
            resolve(
              new Response(new Uint8Array(cbor(body)), {
                status,
                headers: { "Content-Type": "application/cbor" },
              }),
            );
        });
      }
      return plain(input);
    });
    const main = mountAppAt("/tok/src/tree/app/Main.hs").querySelector("main");
    if (main === null) throw new Error("no main");
    await vi.waitFor(() => {
      expect(rows(main)).toHaveLength(5);
      expect(answer).toBeDefined();
    });
    return { main, answer: () => answer?.() };
  }

  const spans = (main: HTMLElement, n: number) =>
    [
      ...main.querySelectorAll(`.code-row[data-line="${n}"] .code-cell > span`),
    ].map((s) => [s.className, s.textContent]);
  const row = (main: HTMLElement, n: number) =>
    main.querySelector(`.code-row[data-line="${n}"]`);

  it("plain first, then highlighted, same text", async () => {
    const { main, answer } = await openLate(200, lexed());
    const text = rows(main);
    expect(main.querySelector(".code-cell span")).toBeNull();
    answer();
    await vi.waitFor(() => {
      expect(spans(main, 1)).toEqual([
        ["hl-keyword", "module"],
        ["hl-typeCon", "Main"],
      ]);
    });
    expect(spans(main, 5)).toEqual([["hl-literal", '"héllo, wörld → ✓"']]);
    expect(rows(main)).toEqual(text);
    // Excluded: a class, a label; the whole line greyed.
    expect(row(main, 3)?.classList.contains("code-excluded")).toBe(true);
    expect(
      row(main, 3)?.querySelector(".code-num")?.getAttribute("aria-label"),
    ).toBe("3, excluded by CPP");
    expect(spans(main, 3)).toEqual([
      ["hl-cppExcluded", "-- <script>alert(1)</script>"],
    ]);
    expect(document.querySelector("script")).toBeNull();
    // Outline markers: docs links, not Tab stops.
    const mark = row(main, 4)?.querySelector("a.code-outline-mark");
    expect(mark?.getAttribute("href")).toBe("/tok/docs/tree/Main#val%3Amain");
    expect(mark?.getAttribute("tabindex")).toBe("-1");
    expect(mark?.getAttribute("aria-label")).toBe("Docs: main");
    expect(
      row(main, 5)?.querySelector("a.code-outline-mark")?.getAttribute("title"),
    ).toBe("Docs: instance Show T");
  });

  it("the outline list: docs and line links", async () => {
    const { main, answer } = await openLate(200, lexed());
    expect(main.querySelector(".code-outline")).toBeNull();
    answer();
    await vi.waitFor(() => {
      expect(main.querySelector(".code-outline summary")?.textContent).toBe(
        "Outline (2)",
      );
    });
    const nav = main.querySelector('.code-outline nav[aria-label="Outline"]');
    expect(
      [...(nav?.querySelectorAll("a") ?? [])].map((a) => [
        a.textContent,
        a.getAttribute("href"),
      ]),
    ).toEqual([
      ["main", "/tok/docs/tree/Main#val%3Amain"],
      ["line 4", "/tok/src/tree/app/Main.hs#L4"],
      ["instance Show T", "/tok/docs/tree/Main#inst%3AShow%3AT"],
      ["line 5", "/tok/src/tree/app/Main.hs#L5"],
    ]);
    // After the view: it comes late and must not push the view down.
    const outline = main.querySelector(".code-outline");
    const grid = main.querySelector(".code-view");
    expect(
      outline !== null &&
        grid !== null &&
        outline.compareDocumentPosition(grid) &
          Node.DOCUMENT_POSITION_PRECEDING,
    ).toBeTruthy();
  });

  it("no ref: no docs links, line links stay", async () => {
    const { main, answer } = await openLate(200, lexed({ ref: null }));
    answer();
    await vi.waitFor(() => {
      expect(main.querySelector(".code-outline")).not.toBeNull();
    });
    expect(main.querySelector(".code-outline-mark")).toBeNull();
    expect(
      [...main.querySelectorAll(".code-outline a")].map((a) => a.textContent),
    ).toEqual(["line 4", "line 5"]);
  });

  it.each([
    ["another generation", 200, lexed({ generation: 2 })],
    [
      "plain",
      200,
      {
        generation: 1,
        path: "app/Main.hs",
        ref: "Main",
        tier: { kind: "syntactic", reason: "no-hie" },
        partial: false,
        highlight: "plain",
      },
    ],
    ["not found", 404, { generation: 1, kind: "not-found", what: "file" }],
    ["a server error", 500, "cannot index module"],
    ["a bad body", 200, { generation: 1 }],
  ])("%s: stays plain, no message", async (_, status, body) => {
    const { main, answer } = await openLate(status, body);
    const before = main.textContent;
    answer();
    await new Promise((r) => setTimeout(r, 20));
    expect(main.querySelector(".code-cell span")).toBeNull();
    expect(main.querySelector(".code-outline, .code-excluded")).toBeNull();
    expect(main.textContent).toBe(before);
  });
});
