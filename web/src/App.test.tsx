import { afterEach, beforeEach, describe, expect, it, vi } from "vitest";
import { cborFixture, fixture, stubFetch } from "../test/fixtures";
import { mountAppAt, unmountApp } from "../test/mount";

beforeEach(() => {
  stubFetch({
    "/tok/api/files/tree": [200, fixture("files")],
    "/tok/api/file/tree/a.hs": [200, cborFixture("file-text")],
  });
});

afterEach(() => {
  unmountApp();
  vi.unstubAllGlobals();
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
      expect(main?.querySelector(".file-code")?.textContent).toContain(
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
