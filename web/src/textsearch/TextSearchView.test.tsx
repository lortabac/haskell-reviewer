import { afterEach, describe, expect, it, vi } from "vitest";
import { cborFixture, fixture, stubFetch } from "../../test/fixtures";
import { mountAppAt, unmountApp } from "../../test/mount";

const api = "/tok/api/text-search/tree?q=main";
const page = "/tok/search/tree?q=main";
const base = {
  "/tok/api/files/tree": [200, fixture("files")] as [number, unknown],
  "/tok/api/modules/tree": [200, fixture("modules")] as [number, unknown],
};

afterEach(() => {
  unmountApp();
  vi.unstubAllGlobals();
  vi.restoreAllMocks();
  document.title = "";
});

function open(
  routes: Record<string, [number, unknown]>,
  url = page,
  state: unknown = null,
) {
  const fetchMock = stubFetch({ ...base, ...routes });
  const root = mountAppAt(url, undefined, undefined, state);
  const main = root.querySelector("main")!;
  return { main, fetchMock };
}

describe("TextSearchView", () => {
  it("groups by file, links to the line, h1 focused, announced once", async () => {
    const { main } = open({ [api]: [200, fixture("text-search")] });
    await vi.waitFor(() =>
      expect(main.querySelector(".refs-group")).not.toBeNull(),
    );
    const h1 = main.querySelector("h1")!;
    expect(h1.textContent).toBe('Text search: "main"');
    await vi.waitFor(() => expect(document.activeElement).toBe(h1));
    expect(document.title).toContain('Text search: "main"');
    expect(main.querySelector(".refs-summary")?.textContent).toBe(
      "7 matching lines in 5 files.",
    );
    expect(main.textContent).toContain(
      "3 files not searched: 1 binary, 1 too large, 1 unreadable.",
    );
    const groups = main.querySelectorAll(".refs-group");
    expect(groups.length).toBe(5);
    const h2 = groups[0]!.querySelector("h2 a")!;
    expect(h2.textContent).toBe("app/Main.hs");
    expect(h2.getAttribute("href")).toBe("/tok/src/tree/app/Main.hs");
    expect(groups[0]!.textContent).toContain("3 matching lines");
    const a = groups[0]!.querySelector<HTMLAnchorElement>("li a")!;
    expect(a.getAttribute("href")).toBe("/tok/src/tree/app/Main.hs#L1");
    expect(a.textContent).toBe("Line 1 module Main (main) where");
    expect(a.querySelector("mark")?.textContent).toBe("main");
    expect(main.querySelector("[role=status]")?.textContent).toBe(
      "7 matching lines in 5 files.",
    );
  });

  it("capped and stopped", async () => {
    const t = fixture("text-search") as object;
    const { main } = open({
      [api]: [200, { ...t, total: 4145, stopped: true }],
    });
    await vi.waitFor(() =>
      expect(main.querySelector(".refs-summary")).not.toBeNull(),
    );
    expect(main.querySelector(".refs-summary")?.textContent).toBe(
      "Showing the first 7 of 4,145 matching lines.",
    );
    expect(main.textContent).toContain(
      "Search stopped after 1 s: results are incomplete.",
    );
    expect(main.querySelector("[role=status]")?.textContent).toBe(
      "Showing the first 7 of 4,145 matching lines. " +
        "Search stopped after 1 s: results are incomplete.",
    );
  });

  it("error", async () => {
    const { main } = open({ [api]: [500, "cannot read files"] });
    await vi.waitFor(() =>
      expect(main.textContent).toContain("Couldn't search."),
    );
    expect(main.querySelector("[role=status]")?.textContent).toBe(
      "Couldn't search.",
    );
  });

  it("no q: not found", async () => {
    const { main } = open({}, "/tok/search/tree");
    await vi.waitFor(() =>
      expect(main.textContent).toContain("No text to search for."),
    );
  });

  it("q is read by hand: a raw + stays +", async () => {
    const { fetchMock } = open(
      {
        "/tok/api/text-search/tree?q=a%2Bb": [200, fixture("text-search")],
      },
      "/tok/search/tree?q=a+b",
    );
    await vi.waitFor(() =>
      expect(fetchMock.mock.calls.map((c) => c[0])).toContain(
        "/tok/api/text-search/tree?q=a%2Bb",
      ),
    );
  });

  it("a click marks the entry; Back focuses that row", async () => {
    const { main } = open({ [api]: [200, fixture("text-search")] }, page, {
      _depth: 3,
    });
    await vi.waitFor(() =>
      expect(main.querySelector(".refs-item a")).not.toBeNull(),
    );
    const a = main.querySelectorAll<HTMLElement>(".refs-item a")[1]!;
    const key = a.dataset.hitKey!;
    expect(key).toBe("app/Main.hs:4");
    a.click();
    const st = window.history.state as Record<string, unknown>;
    expect(st.textFollowed).toBe(key);
    expect(st._depth).toBeDefined();
    unmountApp();
    const m2 = open({ [api]: [200, fixture("text-search")] }, page, {
      _depth: 2,
      textFollowed: key,
    }).main;
    await vi.waitFor(() =>
      expect(document.activeElement).toBe(
        m2.querySelector(`[data-hit-key="${key}"]`),
      ),
    );
  });
});

describe("TextSearchView, real navigation", () => {
  it("Back focuses the followed row; a new q from the header focuses its h1", async () => {
    const t = fixture("text-search") as Record<string, unknown>;
    const { main } = open({
      [api]: [200, t],
      "/tok/api/text-search/tree?q=where": [200, { ...t, query: "where" }],
      "/tok/api/file/tree/app/Main.hs": [200, cborFixture("file-text")],
    });
    await vi.waitFor(() =>
      expect(main.querySelector(".refs-item a")).not.toBeNull(),
    );
    const a = main.querySelectorAll<HTMLElement>(".refs-item a")[1]!;
    const key = a.dataset.hitKey!;
    a.click();
    await vi.waitFor(() =>
      expect(main.querySelector("h1")?.textContent).toBe("app/Main.hs"),
    );
    window.history.back();
    await vi.waitFor(() =>
      expect(document.activeElement).toBe(
        main.querySelector(`[data-hit-key="${key}"]`),
      ),
    );
    // A new query from the header box: its own page, its h1.
    const box = document.querySelector<HTMLInputElement>(
      ".search-box-header input",
    )!;
    box.value = '"where"';
    box.dispatchEvent(new Event("input", { bubbles: true }));
    box.dispatchEvent(
      new KeyboardEvent("keydown", {
        key: "Enter",
        bubbles: true,
        cancelable: true,
      }),
    );
    await vi.waitFor(() =>
      expect(main.querySelector("h1")?.textContent).toBe(
        'Text search: "where"',
      ),
    );
    await vi.waitFor(() =>
      expect(document.activeElement).toBe(main.querySelector("h1")),
    );
  });

  it("Back with a row that is gone: the h1", async () => {
    const t = fixture("text-search") as Record<string, unknown>;
    const { main } = open({ [api]: [200, t] }, page, {
      _depth: 2,
      textFollowed: "gone.hs:9",
    });
    await vi.waitFor(() =>
      expect(document.activeElement).toBe(main.querySelector("h1")),
    );
  });

  it("q over 128 code points: a note, no request", async () => {
    const q = "a".repeat(129);
    const { main, fetchMock } = open({}, `/tok/search/tree?q=${q}`);
    await vi.waitFor(() =>
      expect(main.textContent).toContain(
        "Text too long: at most 128 characters.",
      ),
    );
    expect(main.querySelector("[role=status]")?.textContent).toBe(
      "Text too long: at most 128 characters.",
    );
    expect(
      fetchMock.mock.calls.filter(
        (c) => typeof c[0] === "string" && c[0].includes("text-search"),
      ),
    ).toEqual([]);
  });
});
