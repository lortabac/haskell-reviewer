import { afterEach, beforeEach, describe, expect, it, vi } from "vitest";
import { cbor, stubFetch } from "../../test/fixtures";
import { fakeLayout } from "../../test/layout";
import { mountAppAt, unmountApp } from "../../test/mount";
import { diffSession } from "../../test/sessions";
import { texts } from "./DiffView";

// 21 lines a side; line 11 differs. Rows: 10 equal, 1 removed, 1
// added, 10 equal.
const same = Array.from({ length: 10 }, (_, i) => `e${i + 1}`);
const after = Array.from({ length: 10 }, (_, i) => `e${i + 12}`);
const baseText = [...same, "xold foo", ...after].join("\n") + "\n";
const headText = [...same, "new bar", ...after].join("\n") + "\n";

function diffBody(over: Record<string, unknown> = {}) {
  return cbor({
    generation: 1,
    path: "src/A.hs",
    oldPath: null,
    status: "modified",
    similarity: null,
    note: null,
    ignoreWhitespace: false,
    base: { kind: "text" },
    head: { kind: "text" },
    diff: {
      kind: "lines",
      runs: [
        ["equal", 10],
        ["delete", 1],
        ["insert", 1],
        ["equal", 10],
      ],
      crlfLines: 0,
      bom: null,
    },
    ...over,
  });
}

const file = (text: string, generation = 1) =>
  cbor({ generation, filtered: false, kind: "text", text });

// Base's line 11: "old" (from column 1) links to its own line 1 (a
// local), "foo" to src/B.hs line 3.
const baseSource = cbor({
  generation: 1,
  path: "src/A.hs",
  ref: "A",
  tier: { kind: "syntactic", reason: "no-hie" },
  partial: false,
  filtered: false,
  highlight: "lexed",
  runs: [11, 2, 0, 5, 2, 2, 0, 6, 0, 9, 2, 1],
  regions: [],
  targets: [
    ["decl", "B", "src/B.hs", 3, "val:foo", [], 1, null, false],
    ["local", 1, 1],
  ],
  outline: [],
});

function open(
  url: string,
  routes: Record<string, [number, unknown]> = {},
  browse = false,
) {
  const fetch = stubFetch({
    "/tok/api/diff/src/A.hs": [200, diffBody()],
    "/tok/api/file/base/src/A.hs": [200, file(baseText)],
    "/tok/api/file/head/src/A.hs": [200, file(headText)],
    "/tok/api/source/base/src/A.hs": [200, baseSource],
    ...routes,
  });
  const root = mountAppAt(
    url,
    undefined,
    undefined,
    null,
    browse ? undefined : diffSession,
  );
  const main = root.querySelector("main");
  if (main === null) throw new Error("no main");
  return { main, fetch };
}

const urls = (fetch: ReturnType<typeof stubFetch>) =>
  fetch.mock.calls.map((c) => c[0] as string);

const grid = (main: HTMLElement) =>
  main.querySelector<HTMLElement>('[role="grid"]');

async function ready(main: HTMLElement): Promise<HTMLElement> {
  await vi.waitFor(() => expect(grid(main)).not.toBeNull());
  return grid(main)!;
}

const labels = (main: HTMLElement) =>
  [...main.querySelectorAll('[role="row"]')].map(
    (r) =>
      r.querySelector('[role="rowheader"]')?.getAttribute("aria-label") ??
      r.querySelector(".diff-gap-text")?.textContent,
  );

beforeEach(() => fakeLayout());

afterEach(() => {
  unmountApp();
  vi.unstubAllGlobals();
  vi.restoreAllMocks();
});

describe("DiffView", () => {
  it("browse mode: a note, no fetch", async () => {
    const { main, fetch } = open("/tok/diff/src/A.hs", {}, true);
    await vi.waitFor(() => expect(main.textContent).toContain(texts.notDiff));
    expect(urls(fetch).some((u) => u.startsWith("/tok/api/diff"))).toBe(false);
  });

  it("rows from the runs, with gaps and names (G23)", async () => {
    const { main } = open("/tok/diff/src/A.hs");
    const g = await ready(main);
    expect(g.getAttribute("aria-label")).toBe("Diff of src/A.hs");
    expect(g.getAttribute("aria-rowcount")).toBe("10");
    expect(labels(main)).toEqual([
      "7 lines hidden, 1 to 7",
      "line 8, unchanged",
      "line 9, unchanged",
      "line 10, unchanged",
      "removed line 11",
      "added line 11",
      "line 12, unchanged",
      "line 13, unchanged",
      "line 14, unchanged",
      "7 lines hidden, 15 to 21",
    ]);
    const cells = [...main.querySelectorAll(".diff-row .code-cell")].map(
      (c) => c.textContent,
    );
    expect(cells.slice(3, 5)).toEqual(["xold foo", "new bar"]);
    expect(
      [...main.querySelectorAll('[role="row"]')].map((r) =>
        r.getAttribute("aria-rowindex"),
      ),
    ).toEqual(["1", "2", "3", "4", "5", "6", "7", "8", "9", "10"]);
    // Numbers come from CSS.
    const removed = main.querySelector(".diff-delete .code-num");
    expect(removed?.getAttribute("data-b")).toBe("11");
    expect(removed?.getAttribute("data-h")).toBe("");
    expect(removed?.textContent).toBe("");
    // One Tab stop.
    expect(main.querySelectorAll('[role="grid"] [tabindex="0"]')).toHaveLength(
      1,
    );
  });

  it("links on a removed line go to base; same-file ones to the diff hash", async () => {
    const { main, fetch } = open("/tok/diff/src/A.hs");
    await ready(main);
    await vi.waitFor(() =>
      expect(main.querySelectorAll(".diff-delete a.code-link")).toHaveLength(2),
    );
    const hrefs = [...main.querySelectorAll(".diff-delete a.code-link")].map(
      (a) => a.getAttribute("href"),
    );
    expect(hrefs).toEqual([
      "/tok/diff/src/A.hs#base-L1",
      "/tok/src/base/src/B.hs#L3",
    ]);
    // Head has no highlight here: plain.
    expect(main.querySelector(".diff-insert a")).toBeNull();
    expect(urls(fetch)).toEqual(
      expect.arrayContaining([
        "/tok/api/source/base/src/A.hs",
        "/tok/api/source/head/src/A.hs",
      ]),
    );
  });

  it("a gap opens; whole file shows every row", async () => {
    const { main } = open("/tok/diff/src/A.hs");
    const g = await ready(main);
    const button = main.querySelector<HTMLButtonElement>(".diff-gap-button");
    expect(button?.textContent).toBe("Show 7");
    expect(button?.getAttribute("aria-label")).toBe("Show 7 lines, 1 to 7");
    button?.click();
    await vi.waitFor(() => expect(g.getAttribute("aria-rowcount")).toBe("16"));
    // Focus on the first line shown.
    expect(
      document.activeElement?.closest(".code-row")?.getAttribute("data-key"),
    ).toBe("0");
    const whole = [...main.querySelectorAll("button")].find(
      (b) => b.textContent === "Show whole file",
    );
    expect(whole?.getAttribute("aria-pressed")).toBe("false");
    whole?.click();
    await vi.waitFor(() => expect(g.getAttribute("aria-rowcount")).toBe("22"));
    expect(whole?.getAttribute("aria-pressed")).toBe("true");
    whole?.click();
    // Back to what was open.
    await vi.waitFor(() => expect(g.getAttribute("aria-rowcount")).toBe("16"));
  });

  it("a hash on a hidden line opens its gap and marks the row", async () => {
    const { main } = open("/tok/diff/src/A.hs#head-L18");
    const g = await ready(main);
    await vi.waitFor(() =>
      expect(main.querySelector(".code-line-target")).not.toBeNull(),
    );
    const marked = main.querySelector(".code-line-target");
    expect(marked?.querySelector(".code-num")?.getAttribute("aria-label")).toBe(
      "line 18, unchanged",
    );
    // Rows 14..20 (lines 15..21) shown: the gap is gone.
    expect(g.getAttribute("aria-rowcount")).toBe("16");
    expect(main.querySelector('[role="status"]')?.textContent).toBe(
      "Head line 18 marked.",
    );
  });

  it("an equal row answers to the base hash too; past the end marks none", async () => {
    const { main } = open("/tok/diff/src/A.hs#base-L9");
    await ready(main);
    await vi.waitFor(() =>
      expect(
        main
          .querySelector(".code-line-target .code-num")
          ?.getAttribute("aria-label"),
      ).toBe("line 9, unchanged"),
    );
    unmountApp();
    const again = open("/tok/diff/src/A.hs#base-L99");
    await ready(again.main);
    expect(again.main.querySelector(".code-line-target")).toBeNull();
  });

  it("keys: rows, then the buttons of a gap", async () => {
    const { main } = open("/tok/diff/src/A.hs");
    const g = await ready(main);
    const stop = g.querySelector<HTMLElement>('[tabindex="0"]')!;
    stop.focus();
    expect(stop.classList.contains("diff-gap-cell")).toBe(true);
    const key = (k: string) =>
      document.activeElement?.dispatchEvent(
        new KeyboardEvent("keydown", { key: k, bubbles: true }),
      );
    key("ArrowRight");
    expect(document.activeElement?.textContent).toBe("Show 7");
    key("ArrowDown");
    expect(
      document.activeElement?.closest(".code-row")?.getAttribute("data-key"),
    ).toBe("7");
    key("End");
    expect(document.activeElement?.classList.contains("diff-gap-cell")).toBe(
      true,
    );
  });

  it("texts that don't fit the runs: a note", async () => {
    const warn = vi.spyOn(console, "warn").mockImplementation(() => {});
    const { main } = open("/tok/diff/src/A.hs", {
      "/tok/api/file/head/src/A.hs": [200, file(headText + "more\n")],
    });
    await vi.waitFor(() => expect(main.textContent).toContain(texts.mismatch));
    expect(warn).toHaveBeenCalledOnce();
    expect(grid(main)).toBeNull();
  });

  it("404 and 500", async () => {
    let { main } = open("/tok/diff/src/A.hs", {
      "/tok/api/diff/src/A.hs": [404, { error: "not found" }],
    });
    await vi.waitFor(() => expect(main.textContent).toContain(texts.notFound));
    unmountApp();
    ({ main } = open("/tok/diff/src/A.hs", {
      "/tok/api/diff/src/A.hs": [500, "boom"],
    }));
    await vi.waitFor(() => expect(main.textContent).toContain(texts.failed));
  });

  it("an added file: base is empty and not fetched", async () => {
    const { main, fetch } = open("/tok/diff/src/A.hs", {
      "/tok/api/diff/src/A.hs": [
        200,
        diffBody({
          status: "added",
          base: { kind: "absent" },
          diff: {
            kind: "lines",
            runs: [["insert", 21]],
            crlfLines: 0,
            bom: null,
          },
        }),
      ],
    });
    const g = await ready(main);
    expect(g.getAttribute("aria-rowcount")).toBe("21");
    expect(urls(fetch)).not.toContain("/tok/api/file/base/src/A.hs");
  });

  it("not text: a placeholder line", async () => {
    const { main } = open("/tok/diff/src/A.hs", {
      "/tok/api/diff/src/A.hs": [
        200,
        diffBody({
          base: { kind: "binary", size: 3 },
          head: { kind: "binary", size: 4 },
          diff: { kind: "not-text" },
        }),
      ],
    });
    await vi.waitFor(() => expect(main.textContent).toContain(texts.noLines));
  });

  it("ws=ignore asks for it; a ws change fetches the diff only", async () => {
    const { main, fetch } = open("/tok/diff/src/A.hs", {
      "/tok/api/diff/src/A.hs?ws=ignore": [
        200,
        diffBody({ ignoreWhitespace: true }),
      ],
    });
    await ready(main);
    const texts0 = urls(fetch).filter((u) =>
      u.startsWith("/tok/api/file/"),
    ).length;
    expect(texts0).toBe(2);
    window.history.pushState(null, "", "/tok/diff/src/A.hs?ws=ignore");
    window.dispatchEvent(new PopStateEvent("popstate"));
    await vi.waitFor(() =>
      expect(urls(fetch)).toContain("/tok/api/diff/src/A.hs?ws=ignore"),
    );
    await ready(main);
    expect(
      urls(fetch).filter((u) => u.startsWith("/tok/api/file/")).length,
    ).toBe(2);
  });

  it("↑ onto a gap lands on its cell, not a button", async () => {
    const { main } = open("/tok/diff/src/A.hs");
    const g = await ready(main);
    // "old" starts at column 1, like the gap's first button.
    await vi.waitFor(() =>
      expect(main.querySelector(".diff-delete a.code-link")).not.toBeNull(),
    );
    main.querySelector<HTMLElement>(".diff-delete a.code-link")!.focus();
    const key = (k: string) =>
      document.activeElement?.dispatchEvent(
        new KeyboardEvent("keydown", { key: k, bubbles: true }),
      );
    for (let i = 0; i < 4; i++) key("ArrowUp");
    expect(document.activeElement?.classList.contains("diff-gap-cell")).toBe(
      true,
    );
    // The goal column stays: back down, on the link at column 1.
    for (let i = 0; i < 4; i++) key("ArrowDown");
    expect(document.activeElement?.textContent).toBe("old");
    expect(g.contains(document.activeElement)).toBe(true);
  });

  it("head text from another snapshot than the diff: a note", async () => {
    const warn = vi.spyOn(console, "warn").mockImplementation(() => {});
    const { main } = open("/tok/diff/src/A.hs", {
      "/tok/api/file/head/src/A.hs": [200, file(headText, 2)],
    });
    await vi.waitFor(() => expect(main.textContent).toContain(texts.mismatch));
    expect(warn).toHaveBeenCalledOnce();
    expect(grid(main)).toBeNull();
  });

  it.each([
    [{ base: { kind: "absent" } }, texts.emptyAdded],
    [{ head: { kind: "absent" } }, texts.emptyRemoved],
    [{}, texts.empty],
  ])("no rows: a note, no toolbar (%j)", async (sides, note) => {
    const { main } = open("/tok/diff/src/A.hs", {
      "/tok/api/diff/src/A.hs": [
        200,
        diffBody({
          ...sides,
          diff: { kind: "lines", runs: [], crlfLines: 0, bom: null },
        }),
      ],
      "/tok/api/file/base/src/A.hs": [200, file("")],
      "/tok/api/file/head/src/A.hs": [200, file("")],
    });
    await vi.waitFor(() => expect(main.textContent).toContain(note));
    expect(main.querySelector(".code-toolbar")).toBeNull();
  });

  it("head text older than the diff: fetched again, then shown", async () => {
    let heads = 0;
    const routes: Record<string, [number, unknown]> = {
      "/tok/api/diff/src/A.hs": [200, diffBody({ generation: 2 })],
      "/tok/api/file/base/src/A.hs": [200, file(baseText)],
    };
    Object.defineProperty(routes, "/tok/api/file/head/src/A.hs", {
      enumerable: true,
      get: () => [200, file(headText, ++heads === 1 ? 1 : 2)],
    });
    stubFetch(routes);
    const root = mountAppAt(
      "/tok/diff/src/A.hs",
      undefined,
      undefined,
      null,
      diffSession,
    );
    const main = root.querySelector("main")!;
    const g = await ready(main);
    expect(heads).toBe(2);
    expect(g.getAttribute("aria-rowcount")).toBe("10");
    expect(main.textContent).not.toContain(texts.mismatch);
  });
});
