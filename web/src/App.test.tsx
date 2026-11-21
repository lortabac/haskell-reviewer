import { afterEach, beforeEach, describe, expect, it, vi } from "vitest";
import { cborFixture, fixture, stubFetch } from "../test/fixtures";
import { fakeLayout } from "../test/layout";
import { mountAppAt, unmountApp } from "../test/mount";

beforeEach(() => {
  fakeLayout();
  stubFetch({
    "/tok/api/files/tree": [200, fixture("files")],
    "/tok/api/file/tree/a.hs": [200, cborFixture("file-text")],
  });
});

afterEach(() => {
  unmountApp();
  vi.unstubAllGlobals();
  vi.restoreAllMocks();
});

describe("App routing under the token", () => {
  it.each(["/tok", "/tok/"])("%s shows Home", (path) => {
    const main = mountAppAt(path).querySelector("main");
    expect(main?.textContent).toContain(
      "Pick a module or file in the sidebar.",
    );
  });

  it("/tok/src/tree/a.hs shows the file view", async () => {
    const main = mountAppAt("/tok/src/tree/a.hs").querySelector("main");
    expect(main?.querySelector("h1")?.textContent).toBe("a.hs");
    await vi.waitFor(() => {
      expect(main?.querySelector(".code-view")?.textContent).toContain(
        "module Main",
      );
    });
  });

  it("/tok/docs/tree/<ref> shows the docs view", async () => {
    stubFetch({
      "/tok/api/module/tree/Page": [200, cborFixture("module-page")],
    });
    const main = mountAppAt("/tok/docs/tree/Page").querySelector("main");
    await vi.waitFor(() => {
      expect(main?.querySelector("section.decl")).not.toBeNull();
    });
    expect(main?.querySelector("h1")?.textContent).toBe("Page");
  });

  it.each([
    "/tok/changes",
    "/tok/src/tree",
    "/tok/src/tree/a%2Fb",
    "/tok/docs/tree",
    "/tok/docs/head/A",
  ])("%s shows NotFound, with a search box", (path) => {
    const main = mountAppAt(path).querySelector("main");
    expect(main?.textContent).toContain("Not found in this snapshot");
    expect(main?.querySelector('[role="search"] input')).not.toBeNull();
  });

  it.each(["/tok", "/tok/src/tree/a.hs", "/tok/changes"])(
    "%s: the header search combobox is there",
    (path) => {
      const root = mountAppAt(path);
      const box = root.querySelector(".frame-header input[role=combobox]");
      expect(box).not.toBeNull();
    },
  );

  it("the sidebar is in the Project nav", async () => {
    const nav = mountAppAt("/tok").querySelector("nav");
    expect(nav?.getAttribute("aria-label")).toBe("Project");
    await vi.waitFor(() => {
      expect(nav?.querySelector(".file-tree button")?.textContent).toBe("app");
    });
  });
});

const diff = {
  mode: "diff",
  spec: "a..b",
  base: { label: "a", commit: "1" },
  head: { label: "b", commit: "2" },
  mergeBases: null,
} as const;

const urls = (mock: { mock: { calls: unknown[][] } }): string[] =>
  mock.mock.calls.map((c) => String(c[0]));

describe("sides in URLs (D-332)", () => {
  it("diff mode, tree side: not found, a link to head, no fetch", async () => {
    const mock = stubFetch({});
    const root = mountAppAt(
      "/tok/docs/tree/A#v:a",
      undefined,
      undefined,
      null,
      diff,
    );
    const main = root.querySelector("main");
    expect(main?.textContent).toContain("Not found in this snapshot");
    expect(main?.textContent).toContain(
      "This session compares base and head; there is no “tree” side.",
    );
    const link = [...(main?.querySelectorAll("a") ?? [])].find(
      (a) => a.textContent === "Open on head",
    );
    expect(link?.getAttribute("href")).toBe("/tok/docs/head/A#v:a");
    // The search box gets the ref.
    expect(main?.querySelector<HTMLInputElement>("input")?.value).toBe("A");
    await new Promise((r) => setTimeout(r, 20));
    expect(urls(mock).filter((u) => u.includes("/api/module/"))).toEqual([]);
    // The sidebar uses the default side.
    expect(urls(mock)).toContain("/tok/api/modules/head");
    expect(urls(mock)).not.toContain("/tok/api/modules/tree");
  });

  it("diff mode, garbage side: not found too", () => {
    stubFetch({});
    const main = mountAppAt(
      "/tok/src/nope/a.hs",
      undefined,
      undefined,
      null,
      diff,
    ).querySelector("main");
    expect(main?.textContent).toContain("there is no “nope” side.");
  });

  it("browse mode, base side: not in diff mode, a link to tree", () => {
    const main = mountAppAt("/tok/docs/base/A").querySelector("main");
    expect(main?.textContent).toContain("Not found in this snapshot");
    expect(main?.textContent).toContain("Not in diff mode.");
    const link = [...(main?.querySelectorAll("a") ?? [])].find(
      (a) => a.textContent === "Open on tree",
    );
    expect(link?.getAttribute("href")).toBe("/tok/docs/tree/A");
  });

  it("diff mode, head side: fetches head; a side change mounts the page again", async () => {
    // Different pages per side: head has declarations, base none.
    const mock = stubFetch({
      "/tok/api/module/head/Page": [200, cborFixture("module-page")],
      "/tok/api/module/base/Page": [200, cborFixture("module-parsed")],
      "/tok/api/modules/head": [200, fixture("modules")],
      "/tok/api/modules/base": [200, fixture("modules")],
    });
    const main = mountAppAt(
      "/tok/docs/head/Page",
      undefined,
      undefined,
      null,
      diff,
    ).querySelector("main");
    await vi.waitFor(() => {
      expect(main?.querySelector("section.decl")).not.toBeNull();
    });
    const go = (path: string) => {
      window.history.pushState(null, "", path);
      window.dispatchEvent(new PopStateEvent("popstate"));
    };
    go("/tok/docs/base/Page");
    await vi.waitFor(() => {
      expect(urls(mock)).toContain("/tok/api/module/base/Page");
      expect(main?.querySelectorAll("section.decl")).toHaveLength(0);
    });
    go("/tok/docs/head/Page");
    await vi.waitFor(() => {
      expect(main?.querySelector("section.decl")).not.toBeNull();
    });
    // Same ref, other side: a new fetch, not the page kept as it was.
    await vi.waitFor(() => {
      expect(
        urls(mock).filter((u) => u === "/tok/api/module/head/Page"),
      ).toHaveLength(2);
    });
    // The sidebar keeps the last tree of each side.
    expect(
      urls(mock).filter((u) => u === "/tok/api/modules/head"),
    ).toHaveLength(1);
    expect(
      urls(mock).filter((u) => u === "/tok/api/modules/base"),
    ).toHaveLength(1);
  });

  it("links on a base page stay on base", async () => {
    stubFetch({
      "/tok/api/modules/base": [200, fixture("modules")],
      "/tok/api/files/base": [200, fixture("files")],
    });
    const root = mountAppAt(
      "/tok/docs/base/Nope",
      undefined,
      undefined,
      null,
      diff,
    );
    await vi.waitFor(() => {
      expect(root.querySelector("nav a[href]")).not.toBeNull();
    });
    const hrefs = [...root.querySelectorAll("nav a[href]")].map((a) =>
      a.getAttribute("href"),
    );
    expect(hrefs.length).toBeGreaterThan(0);
    for (const h of hrefs) expect(h).toMatch(/^\/tok\/(docs|src|md)\/base\//);
  });
});

// A fetch where some URLs answer only when the test says so.
function slowFetch(routes: Record<string, [number, unknown]>, slow: string[]) {
  const answers = new Map<string, () => void>();
  const fast = stubFetch(routes).getMockImplementation()!;
  const mock = vi.fn((input: RequestInfo | URL) => {
    const url =
      typeof input === "string"
        ? input
        : input instanceof URL
          ? input.pathname
          : input.url;
    if (!slow.includes(url)) return fast(input);
    return new Promise<Response>((resolve) => {
      answers.set(url, () => void fast(input).then(resolve));
    });
  });
  vi.stubGlobal("fetch", mock);
  return { answer: (url: string) => answers.get(url)?.() };
}

const go = (path: string) => {
  window.history.pushState(null, "", path);
  window.dispatchEvent(new PopStateEvent("popstate"));
};

// The old side's answer comes after the new side's: the sidebar shows
// the new side's list and keeps it.
describe("sidebar per side", () => {
  it("FilesPanel: a late answer of the old side is dropped", async () => {
    const list = fixture("files") as Record<string, unknown>;
    const { answer } = slowFetch(
      {
        "/tok/api/files/head": [
          200,
          { ...list, entries: [{ path: "head.txt", kind: "file" }] },
        ],
        "/tok/api/files/base": [
          200,
          { ...list, entries: [{ path: "base.txt", kind: "file" }] },
        ],
      },
      ["/tok/api/files/head"],
    );
    const nav = mountAppAt(
      "/tok/src/head/x",
      undefined,
      undefined,
      null,
      diff,
    ).querySelector('nav[aria-label="Project"]');
    go("/tok/src/base/x");
    await vi.waitFor(() => {
      expect(nav?.textContent).toContain("base.txt");
    });
    answer("/tok/api/files/head");
    await new Promise((r) => setTimeout(r, 20));
    expect(nav?.textContent).toContain("base.txt");
    expect(nav?.textContent).not.toContain("head.txt");
  });

  it("ModulesProvider: a late answer of the old side is dropped", async () => {
    const tree = fixture("modules") as { packages: { name: string }[] };
    const named = (name: string) => ({
      ...tree,
      packages: tree.packages.map((p, i) => (i === 0 ? { ...p, name } : p)),
    });
    const { answer } = slowFetch(
      {
        "/tok/api/modules/head": [200, named("headpkg")],
        "/tok/api/modules/base": [200, named("basepkg")],
      },
      ["/tok/api/modules/head"],
    );
    const nav = mountAppAt(
      "/tok/docs/head/X",
      undefined,
      undefined,
      null,
      diff,
    ).querySelector('nav[aria-label="Project"]');
    go("/tok/docs/base/X");
    await vi.waitFor(() => {
      expect(nav?.textContent).toContain("basepkg");
    });
    answer("/tok/api/modules/head");
    await new Promise((r) => setTimeout(r, 20));
    expect(nav?.textContent).toContain("basepkg");
    expect(nav?.textContent).not.toContain("headpkg");
  });
});
