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
    expect(main?.textContent).toContain("Pick a file in the sidebar.");
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

  it.each(["/tok/changes", "/tok/src/tree", "/tok/src/tree/a%2Fb"])(
    "%s shows NotFound",
    (path) => {
      const main = mountAppAt(path).querySelector("main");
      expect(main?.textContent).toContain("Not found in this snapshot");
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
