import { afterEach, describe, expect, it, vi } from "vitest";
import { fixture, stubFetch } from "../../test/fixtures";
import { mountAppAt, unmountApp } from "../../test/mount";
import { lineCount } from "./FileView";

// Mounts the app at /tok/src/tree/<path>, with the API answering
// status and body for that file.
async function open(
  path: string,
  status: number,
  body: unknown,
): Promise<HTMLElement> {
  const fetch = stubFetch({
    "/tok/api/files/tree": [200, fixture("files")],
    [`/tok/api/file/tree/${path}`]: [status, body],
  });
  const main = mountAppAt(`/tok/src/tree/${path}`).querySelector("main");
  if (main === null) throw new Error("no main");
  await vi.waitFor(() => {
    expect(fetch).toHaveBeenCalledTimes(2);
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
    const main = await open("app/Main.hs", 200, fixture("file-text"));
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
    const main = await open("a%20b/%C3%A9%23.hs", 200, fixture("file-text"));
    expect(main.querySelector("h1")?.textContent).toBe("a b/é#.hs");
  });

  it("binary", async () => {
    const main = await open("assets/logo.png", 200, fixture("file-binary"));
    expect(main.textContent).toContain(
      "This file is not UTF-8 text, so it isn't shown (2,048 bytes).",
    );
    expect(main.querySelector("pre")).toBeNull();
  });

  it("too large", async () => {
    const main = await open("data/big.csv", 200, fixture("file-too-large"));
    expect(main.textContent).toContain(
      "This file is too large to show (5,242,880 bytes; the limit is 1 MiB).",
    );
  });

  it("symlink, resolved: links to the target file", async () => {
    const main = await open("docs/guide.md", 200, fixture("file-symlink"));
    expect(main.textContent).toContain("Symbolic link to ../shared/guide.md");
    const a = main.querySelector("a");
    expect(a?.getAttribute("href")).toBe("/tok/src/tree/shared/guide.md");
  });

  it("symlink, not resolved: no link", async () => {
    const main = await open("docs/old.md", 200, {
      generation: 1,
      kind: "symlink",
      target: "/etc/passwd",
      resolved: null,
    });
    expect(main.textContent).toContain("Symbolic link to /etc/passwd");
    expect(main.querySelector("a")).toBeNull();
  });

  it("submodule", async () => {
    const main = await open("vendor/dep", 200, fixture("file-submodule"));
    expect(main.textContent).toContain(
      "Submodule at commit 0123456789abcdef0123456789abcdef01234567. Its files aren't shown.",
    );
  });

  it("nested repo", async () => {
    const main = await open("vendor/nested", 200, {
      generation: 1,
      kind: "submodule",
      commit: null,
    });
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

  it("other errors", async () => {
    const main = await open("x.hs", 500, "cannot read file");
    expect(main.textContent).toContain("Couldn't load this file.");
  });

  it("bad payload", async () => {
    const main = await open("x.hs", 200, { kind: "text" });
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
      "/tok/api/file/tree/app/Main.hs": [200, fixture("file-text")],
    });
    const root = mountAppAt("/tok");
    const nav = root.querySelector("nav");
    const main = root.querySelector("main");
    if (nav === null || main === null) throw new Error("no frame");
    await vi.waitFor(() => {
      expect(nav.querySelector("button")?.textContent).toBe("app");
    });
    nav.querySelector("button")?.click();
    const link = nav.querySelector("a");
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
      "/tok/api/file/tree/docs/guide.md": [200, fixture("file-symlink")],
      "/tok/api/file/tree/shared/guide.md": [200, fixture("file-text")],
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
          json({ generation: 1, kind: "text", text: "new file\n" }),
        );
      }),
    );
    const root = mountAppAt("/tok/src/tree/app/Main.hs");
    const main = root.querySelector("main");
    if (main === null) throw new Error("no main");
    await vi.waitFor(() => {
      expect(late).toBeDefined();
      expect(root.querySelectorAll("nav button").length).toBeGreaterThan(0);
    });
    // Go to another file, through the router.
    [...root.querySelectorAll<HTMLElement>("nav button")]
      .find((b) => b.textContent === "shared")
      ?.click();
    root
      .querySelector<HTMLElement>('nav a[href="/tok/src/tree/shared/guide.md"]')
      ?.click();
    await vi.waitFor(() => {
      expect(main.querySelector(".file-code")?.textContent).toBe("new file\n");
    });
    late?.(json(fixture("file-text")));
    await new Promise((r) => setTimeout(r, 50));
    expect(main.querySelector("h1")?.textContent).toBe("shared/guide.md");
    expect(main.querySelector(".file-code")?.textContent).toBe("new file\n");
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
