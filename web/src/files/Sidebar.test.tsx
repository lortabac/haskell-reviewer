import { afterEach, describe, expect, it, vi } from "vitest";
import { fixture, stubFetch } from "../../test/fixtures";
import { mountAppAt, unmountApp } from "../../test/mount";

function nav(list: [number, unknown], at = "/tok"): HTMLElement {
  stubFetch({ "/tok/api/files/tree": list });
  const n = mountAppAt(at).querySelector("nav");
  if (n === null) throw new Error("no nav");
  return n;
}

const list = fixture("files") as Record<string, unknown>;

afterEach(() => {
  unmountApp();
  vi.unstubAllGlobals();
});

describe("Sidebar", () => {
  it("shows the tree", async () => {
    const n = nav([200, list]);
    expect(n.textContent).toBe("Loading files…");
    await vi.waitFor(() => {
      expect(n.querySelectorAll("button")).toHaveLength(6);
    });
  });

  it("deep link: the current file is open and marked", async () => {
    const n = nav([200, list], "/tok/src/tree/app/Main.hs");
    await vi.waitFor(() => {
      const a = n.querySelector('[aria-current="page"]');
      expect(a?.textContent).toBe("Main.hs");
      expect(a?.getAttribute("href")).toBe("/tok/src/tree/app/Main.hs");
    });
  });

  it("links are encoded", async () => {
    const n = nav(
      [200, { ...list, entries: [{ path: "a b#.hs", kind: "file" }] }],
      "/tok",
    );
    await vi.waitFor(() => {
      expect(n.querySelector("a")?.getAttribute("href")).toBe(
        "/tok/src/tree/a%20b%23.hs",
      );
    });
  });

  it("empty, truncated, skipped names", async () => {
    const n = nav([
      200,
      { ...list, entries: [], truncated: true, skippedNames: 1234 },
    ]);
    await vi.waitFor(() => {
      expect(n.textContent).toContain("No files under ROOT.");
    });
    expect(n.textContent).toContain("Only the first 100,000 files are listed.");
    expect(n.textContent).toContain(
      "1,234 file names aren't UTF-8 and aren't listed.",
    );
  });

  it("one skipped name", async () => {
    const n = nav([200, { ...list, skippedNames: 1 }]);
    await vi.waitFor(() => {
      expect(n.textContent).toContain(
        "1 file name isn't UTF-8 and isn't listed.",
      );
    });
  });

  it.each([
    [500, "cannot list files"],
    [200, { entries: 1 }],
  ])("error %i", async (status, body) => {
    const n = nav([status, body]);
    await vi.waitFor(() => {
      expect(n.textContent).toBe(
        "Couldn't load the file list. See the terminal for details.",
      );
    });
  });
});
