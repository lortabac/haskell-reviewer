import { afterEach, beforeEach, describe, expect, it, vi } from "vitest";
import { cbor, cborFixture, fixture, stubFetch } from "../../test/fixtures";
import { mountAppAt, unmountApp } from "../../test/mount";
import { lineCount } from "./FileView";

// Mounts the app at /tok/src/tree/<path>, with the API answering
// status and body for that file. No module tree unless given.
async function open(
  path: string,
  status: number,
  body: unknown,
  modules: [number, unknown] = [404, "not found"],
): Promise<HTMLElement> {
  const fetch = stubFetch({
    "/tok/api/files/tree": [200, fixture("files")],
    "/tok/api/modules/tree": modules,
    [`/tok/api/file/tree/${path}`]: [status, body],
  });
  const main = mountAppAt(`/tok/src/tree/${path}`).querySelector("main");
  if (main === null) throw new Error("no main");
  await vi.waitFor(() => {
    expect(fetch).toHaveBeenCalledTimes(3);
    expect(main.querySelector("h1, p")).not.toBeNull();
  });
  // Let the resource settle.
  await vi.waitFor(() => {
    expect(main.textContent).not.toBe(path);
  });
  return main;
}

afterEach(() => {
  unmountApp();
  vi.unstubAllGlobals();
  document.title = "";
});

describe("FileView", () => {
  it("shows text as text, with line numbers", async () => {
    const main = await open("app/Main.hs", 200, cborFixture("file-text"));
    expect(main.querySelector("h1")?.textContent).toBe("app/Main.hs");
    expect(document.title).toBe("app/Main.hs — haskell-reviewer");
    const code = main.querySelector("pre.file-code");
    expect(code?.textContent).toContain("<script>alert(1)</script>");
    expect(code?.textContent).toContain("héllo, wörld → ✓");
    expect(code?.getAttribute("tabindex")).toBe("0");
    expect(code?.getAttribute("aria-label")).toBe("File contents");
    // Text only: no element per line, no script element.
    expect(code?.children).toHaveLength(0);
    expect(document.querySelector("script")).toBeNull();
    const gutter = main.querySelector("pre.file-gutter");
    expect(gutter?.getAttribute("aria-hidden")).toBe("true");
    expect(gutter?.textContent).toBe("1\n2\n3\n4\n5");
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
      expect(main.querySelector(".file-code")).not.toBeNull();
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
      expect(main.querySelector(".file-code")).not.toBeNull();
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
      expect(main.querySelector(".file-code")?.textContent).toBe("new file\n");
    });
    late?.(ok(cborFixture("file-text")));
    await new Promise((r) => setTimeout(r, 50));
    expect(main.querySelector("h1")?.textContent).toBe("shared/guide.md");
    expect(main.querySelector(".file-code")?.textContent).toBe("new file\n");
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
    [...main.querySelectorAll(".file-gutter .file-line-target")].map(
      (e) => e.textContent,
    );

  async function at(hash: string): Promise<HTMLElement> {
    const fetch = stubFetch({
      "/tok/api/files/tree": [200, fixture("files")],
      "/tok/api/file/tree/app/Main.hs": [200, cborFixture("file-text")],
    });
    void fetch;
    const main = mountAppAt(`/tok/src/tree/app/Main.hs${hash}`).querySelector(
      "main",
    );
    if (main === null) throw new Error("no main");
    await vi.waitFor(() => {
      expect(main.querySelector(".file-code")).not.toBeNull();
    });
    return main;
  }

  it("marks the line and scrolls it to the middle", async () => {
    const scroll = vi
      .spyOn(HTMLElement.prototype, "scrollIntoView")
      .mockImplementation(() => undefined);
    const main = await at("#L3");
    expect(marked(main)).toEqual(["3"]);
    flush();
    expect(scroll).toHaveBeenCalledWith({ block: "center" });
    expect(scroll.mock.contexts).toContain(
      main.querySelector(".file-line-target"),
    );
    expect(main.querySelector('.file-text [role="status"]')?.textContent).toBe(
      "Line 3 marked.",
    );
    // Still one text: gutter numbers, code.
    expect(main.querySelector(".file-gutter")?.textContent).toBe(
      "1\n2\n3\n4\n5",
    );
  });

  it("a range; past the end is cut", async () => {
    const main = await at("#L2-L4");
    expect(marked(main)).toEqual(["2", "3", "4"]);
    expect(main.querySelector('[role="status"]')?.textContent).toBe(
      "Lines 2–4 marked.",
    );
    unmountApp();
    const cut = await at("#L4-L99");
    expect(marked(cut)).toEqual(["4", "5"]);
    expect(cut.querySelector('[role="status"]')?.textContent).toBe(
      "Lines 4–5 marked.",
    );
  });

  // #L0: no line 0; #L6 and #L9: the file has 5 lines.
  it.each(["#L0", "#L6", "#L9", "#L6-L8", "#L4-L2", "#junk"])(
    "%s: nothing marked",
    async (hash) => {
      const scroll = vi
        .spyOn(HTMLElement.prototype, "scrollIntoView")
        .mockImplementation(() => undefined);
      const main = await at(hash);
      flush();
      expect(marked(main)).toEqual([]);
      // Only the sidebar scrolls (to the current file), not the gutter.
      expect(
        scroll.mock.contexts.filter(
          (el) => el instanceof Element && el.closest(".file-gutter") !== null,
        ),
      ).toEqual([]);
      expect(main.querySelector('[role="status"]')?.textContent).toBe("");
    },
  );

  it("a hash change moves the mark, no refetch", async () => {
    vi.spyOn(HTMLElement.prototype, "scrollIntoView").mockImplementation(
      () => undefined,
    );
    const main = await at("#L1");
    const fetch = vi.mocked(globalThis.fetch);
    const calls = fetch.mock.calls.length;
    const a = document.createElement("a");
    a.href = "/tok/src/tree/app/Main.hs#L5";
    main.append(a);
    a.click();
    await vi.waitFor(() => {
      expect(marked(main)).toEqual(["5"]);
    });
    expect(fetch.mock.calls.length).toBe(calls);
  });
});

describe("lineCount", () => {
  it.each([
    ["", 0],
    ["a", 1],
    ["a\n", 1],
    ["a\nb", 2],
    ["a\r\nb\r\n", 2],
    ["\n", 1],
    ["\n\n", 2],
  ])("%j has %i lines", (text, n) => {
    expect(lineCount(text)).toBe(n);
  });
});
