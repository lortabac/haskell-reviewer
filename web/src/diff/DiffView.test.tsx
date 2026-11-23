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

  it("not text: the note says why", async () => {
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
    await vi.waitFor(() =>
      expect(main.querySelector(".file-note")?.textContent).toBe(
        "Binary file changed (3 bytes → 4 bytes).",
      ),
    );
    expect(main.querySelector(".code-toolbar")).toBeNull();
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

  it("too slow: fetched once, again only on Try again", async () => {
    const { main, fetch } = open("/tok/diff/src/A.hs", {
      "/tok/api/diff/src/A.hs": [200, diffBody({ diff: { kind: "too-slow" } })],
    });
    await vi.waitFor(() =>
      expect(main.querySelector(".file-note")?.textContent).toBe(
        "This diff took over 2 s, so it isn't shown.",
      ),
    );
    const diffs = () =>
      urls(fetch).filter((u) => u === "/tok/api/diff/src/A.hs").length;
    await new Promise((r) => setTimeout(r, 50));
    expect(diffs()).toBe(1);
    const links = [...main.querySelectorAll(".diff-retry a")].map((a) =>
      a.getAttribute("href"),
    );
    expect(links).toEqual(["/tok/src/base/src/A.hs", "/tok/src/head/src/A.hs"]);
    const again = [...main.querySelectorAll("button")].find(
      (b) => b.textContent === texts.tryAgain,
    );
    again?.click();
    await vi.waitFor(() => expect(diffs()).toBe(2));
    // No text fetch for a diff with no lines.
    expect(urls(fetch).some((u) => u.startsWith("/tok/api/file/"))).toBe(false);
  });

  it("the ws toggle: a new entry, the hash kept", async () => {
    const { main, fetch } = open("/tok/diff/src/A.hs#head-L12", {
      "/tok/api/diff/src/A.hs?ws=ignore": [
        200,
        diffBody({ ignoreWhitespace: true }),
      ],
    });
    await ready(main);
    const button = () =>
      [...main.querySelectorAll("button")].find(
        (b) => b.textContent === texts.ws,
      );
    expect(button()?.getAttribute("aria-pressed")).toBe("false");
    const n = window.history.length;
    button()?.focus();
    button()?.click();
    await vi.waitFor(() =>
      expect(urls(fetch)).toContain("/tok/api/diff/src/A.hs?ws=ignore"),
    );
    const l = window.location;
    expect(l.pathname + l.search + l.hash).toBe(
      "/tok/diff/src/A.hs?ws=ignore#head-L12",
    );
    expect(window.history.length).toBe(n + 1);
    await ready(main);
    expect(button()?.getAttribute("aria-pressed")).toBe("true");
    // The same button: focus stayed on it.
    expect(document.activeElement).toBe(button());
    await vi.waitFor(() =>
      expect(main.querySelector(".code-line-target")).not.toBeNull(),
    );
  });

  // Blocks at rows 0, 4 and 15; hunks at 0 and 15.
  const steps = {
    "/tok/api/diff/src/A.hs": [
      200,
      diffBody({
        diff: {
          kind: "lines",
          runs: [
            ["delete", 1],
            ["insert", 1],
            ["equal", 2],
            ["delete", 1],
            ["insert", 1],
            ["equal", 9],
            ["delete", 1],
            ["insert", 1],
            ["equal", 3],
          ],
          crlfLines: 0,
          bom: null,
        },
      }),
    ],
    "/tok/api/file/base/src/A.hs": [
      200,
      file(Array.from({ length: 17 }, (_, i) => `b${i + 1}\n`).join("")),
    ],
    "/tok/api/file/head/src/A.hs": [
      200,
      file(Array.from({ length: 17 }, (_, i) => `h${i + 1}\n`).join("")),
    ],
  } satisfies Record<string, [number, unknown]>;

  const press = (key: string, code = `Key${key.toUpperCase()}`) =>
    (document.activeElement ?? document.body).dispatchEvent(
      new KeyboardEvent("keydown", {
        key,
        code,
        bubbles: true,
        cancelable: true,
      }),
    );
  const focusedKey = () =>
    document.activeElement?.closest(".code-row")?.getAttribute("data-key");
  const status = () => document.querySelector(".keys-status")?.textContent;

  it("j / k: hunks; n / p: change blocks; no more at the ends", async () => {
    const { main } = open("/tok/diff/src/A.hs", steps);
    await ready(main);
    // From outside the grid: the cursor's own row counts.
    press("j");
    expect(focusedKey()).toBe("0");
    press("j");
    expect(focusedKey()).toBe("15");
    press("j");
    expect(focusedKey()).toBe("15");
    expect(status()).toBe(texts.noMore);
    press("k");
    expect(focusedKey()).toBe("0");
    press("n");
    expect(focusedKey()).toBe("4");
    press("n");
    expect(focusedKey()).toBe("15");
    press("p");
    expect(focusedKey()).toBe("4");
    press("p");
    press("p");
    expect(focusedKey()).toBe("0");
    expect(status()?.trim()).toBe(texts.noMore);
  });

  it("2 and 1: source and docs at the cursor's side and line; Back first", async () => {
    const { main } = open("/tok/diff/src/A.hs");
    const g = await ready(main);
    // The removed row: base's line 11.
    g.querySelector<HTMLElement>(
      '.code-row[data-key="10"] .code-cell',
    )?.click();
    const cell = g.querySelector<HTMLElement>(
      '.code-row[data-key="10"] .code-cell',
    );
    cell?.dispatchEvent(new MouseEvent("mouseup", { bubbles: true }));
    await vi.waitFor(() => expect(focusedKey()).toBe("10"));
    const push = vi.spyOn(window.history, "pushState");
    press("2", "Digit2");
    await vi.waitFor(() =>
      expect(window.location.pathname + window.location.hash).toBe(
        "/tok/src/base/src/A.hs#L11",
      ),
    );
    // The origin entry first, then the router's.
    expect(push.mock.calls[0]?.[0]).toEqual({ origin: { col: 0 } });
    expect(push.mock.calls[0]?.[2]).toBe("/tok/diff/src/A.hs#base-L11");
    unmountApp();
    const again = open("/tok/diff/src/A.hs#base-L11");
    await ready(again.main);
    await vi.waitFor(() =>
      expect(
        again.main.querySelector(".code-line-target")?.getAttribute("data-key"),
      ).toBe("10"),
    );
    // Wait for base's highlight: docs need its module.
    await vi.waitFor(() =>
      expect(
        again.main.querySelector(".diff-delete a.code-link"),
      ).not.toBeNull(),
    );
    press("1", "Digit1");
    await vi.waitFor(() =>
      expect(window.location.pathname).toBe("/tok/docs/base/A"),
    );
  });

  it("Back to an origin: focus on the saved column, once links come", async () => {
    stubFetch({
      "/tok/api/diff/src/A.hs": [200, diffBody()],
      "/tok/api/file/base/src/A.hs": [200, file(baseText)],
      "/tok/api/file/head/src/A.hs": [200, file(headText)],
      "/tok/api/source/base/src/A.hs": [200, baseSource],
    });
    const root = mountAppAt(
      "/tok/diff/src/A.hs#base-L11",
      undefined,
      undefined,
      { origin: { col: 1 } },
      diffSession,
    );
    await vi.waitFor(() =>
      expect(document.activeElement?.textContent).toBe("old"),
    );
    expect(
      root.querySelector(".code-line-target")?.getAttribute("data-key"),
    ).toBe("10");
  });

  it("Back to an equal row by base's hash: lands again once head's links come", async () => {
    // Head's line 9 ("e9"): "9" links to line 1. Head's source comes
    // after base's.
    const headSource = cbor({
      generation: 1,
      path: "src/A.hs",
      ref: "A",
      tier: { kind: "syntactic", reason: "no-hie" },
      partial: false,
      filtered: false,
      highlight: "lexed",
      runs: [9, 2, 0, 3, 2, 1],
      regions: [],
      targets: [["local", 1, 1]],
      outline: [],
    });
    const fetch = stubFetch({
      "/tok/api/diff/src/A.hs": [200, diffBody()],
      "/tok/api/file/base/src/A.hs": [200, file(baseText)],
      "/tok/api/file/head/src/A.hs": [200, file(headText)],
      "/tok/api/source/base/src/A.hs": [200, baseSource],
      "/tok/api/source/head/src/A.hs": [200, headSource],
    });
    let release!: () => void;
    const gate = new Promise<void>((r) => (release = r));
    vi.stubGlobal("fetch", (u: string) =>
      u === "/tok/api/source/head/src/A.hs"
        ? gate.then(() => fetch(u))
        : fetch(u),
    );
    const root = mountAppAt(
      "/tok/diff/src/A.hs#base-L9",
      undefined,
      undefined,
      { origin: { col: 1 } },
      diffSession,
    );
    await vi.waitFor(() =>
      expect(root.querySelectorAll(".diff-delete a.code-link")).toHaveLength(2),
    );
    expect(
      document.activeElement?.closest(".code-row")?.getAttribute("data-key"),
    ).toBe("8");
    release();
    await vi.waitFor(() =>
      expect(document.activeElement?.textContent).toBe("9"),
    );
  });

  const button = (main: HTMLElement, text: string) =>
    [...main.querySelectorAll("button")].find((b) => b.textContent === text);

  it("1 on a removed line before base is lexed: no docs, not head's", async () => {
    const { main } = open("/tok/diff/src/A.hs", {
      "/tok/api/source/base/src/A.hs": [404, { error: "not found" }],
      "/tok/api/source/head/src/A.hs": [200, baseSource],
    });
    const g = await ready(main);
    await vi.waitFor(() =>
      expect(main.querySelector(".diff-insert a.code-link")).not.toBeNull(),
    );
    g.querySelector<HTMLElement>(
      '.code-row[data-key="10"] .code-cell',
    )?.focus();
    await vi.waitFor(() => expect(focusedKey()).toBe("10"));
    press("1", "Digit1");
    await vi.waitFor(() => expect(status()).toBe(texts.noDocs));
    expect(window.location.pathname).toBe("/tok/diff/src/A.hs");
  });

  it("change buttons: the cursor moves, focus stays, the row is said", async () => {
    const { main } = open("/tok/diff/src/A.hs", steps);
    await ready(main);
    const next = button(main, texts.nextChange)!;
    const prev = button(main, texts.prevChange)!;
    next.focus();
    next.click();
    expect(document.activeElement).toBe(next);
    expect(status()).toBe("removed line 1");
    next.click();
    expect(status()).toBe("removed line 4");
    next.click();
    next.click();
    expect(status()).toBe(texts.noMore);
    prev.click();
    expect(status()).toBe("removed line 4");
    // The cursor is there: Tab into the grid lands on it.
    const stop = main.querySelector<HTMLElement>(
      '[role="grid"] [tabindex="0"]',
    );
    expect(stop?.closest(".code-row")?.getAttribute("data-key")).toBe("4");
  });

  it("j, then focus leaves the grid, j again: the next hunk", async () => {
    const { main } = open("/tok/diff/src/A.hs", steps);
    await ready(main);
    press("j");
    expect(focusedKey()).toBe("0");
    const whole = button(main, texts.wholeFile)!;
    whole.focus();
    whole.click();
    press("j");
    expect(focusedKey()).toBe("15");
  });

  it("too slow with -w on: the ws toggle stays, to turn it off", async () => {
    const { main } = open("/tok/diff/src/A.hs?ws=ignore", {
      "/tok/api/diff/src/A.hs?ws=ignore": [
        200,
        diffBody({ ignoreWhitespace: true, diff: { kind: "too-slow" } }),
      ],
    });
    await vi.waitFor(() =>
      expect(button(main, texts.ws)?.getAttribute("aria-pressed")).toBe("true"),
    );
    expect(button(main, texts.wholeFile)).toBeUndefined();
  });

  it("Try again: still too slow is said; then the grid takes focus", async () => {
    let calls = 0;
    const routes: Record<string, [number, unknown]> = {
      "/tok/api/file/base/src/A.hs": [200, file(baseText)],
      "/tok/api/file/head/src/A.hs": [200, file(headText)],
    };
    Object.defineProperty(routes, "/tok/api/diff/src/A.hs", {
      enumerable: true,
      get: () => [
        200,
        ++calls <= 2 ? diffBody({ diff: { kind: "too-slow" } }) : diffBody(),
      ],
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
    await vi.waitFor(() =>
      expect(button(main, texts.tryAgain)).not.toBeUndefined(),
    );
    button(main, texts.tryAgain)?.focus();
    button(main, texts.tryAgain)?.click();
    await vi.waitFor(() => expect(status()).toBe(texts.stillSlow));
    button(main, texts.tryAgain)?.click();
    const g = await ready(main);
    await vi.waitFor(() =>
      expect(g.contains(document.activeElement)).toBe(true),
    );
  });

  it("Back to a gap: the gap gets focus, still closed", async () => {
    stubFetch({
      "/tok/api/diff/src/A.hs": [200, diffBody()],
      "/tok/api/file/base/src/A.hs": [200, file(baseText)],
      "/tok/api/file/head/src/A.hs": [200, file(headText)],
    });
    const root = mountAppAt(
      "/tok/diff/src/A.hs#head-L1",
      undefined,
      undefined,
      { origin: { col: 0, gap: true } },
      diffSession,
    );
    const g = await ready(root.querySelector("main")!);
    await vi.waitFor(() =>
      expect(document.activeElement?.classList.contains("diff-gap-cell")).toBe(
        true,
      ),
    );
    expect(g.getAttribute("aria-rowcount")).toBe("10");
  });

  it("Try again, then no grid: a later grid takes no focus", async () => {
    vi.spyOn(console, "warn").mockImplementation(() => {});
    let calls = 0;
    // After Try again: runs that don't fit the texts (no grid).
    const bad = diffBody({
      diff: {
        kind: "lines",
        runs: [
          ["equal", 11],
          ["delete", 1],
          ["insert", 1],
          ["equal", 10],
        ],
        crlfLines: 0,
        bom: null,
      },
    });
    const routes: Record<string, [number, unknown]> = {
      "/tok/api/file/base/src/A.hs": [200, file(baseText)],
      "/tok/api/file/head/src/A.hs": [200, file(headText)],
      "/tok/api/diff/src/A.hs?ws=ignore": [
        200,
        diffBody({ ignoreWhitespace: true }),
      ],
    };
    Object.defineProperty(routes, "/tok/api/diff/src/A.hs", {
      enumerable: true,
      get: () => [
        200,
        ++calls === 1 ? diffBody({ diff: { kind: "too-slow" } }) : bad,
      ],
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
    await vi.waitFor(() =>
      expect(button(main, texts.tryAgain)).not.toBeUndefined(),
    );
    button(main, texts.tryAgain)?.click();
    await vi.waitFor(() => expect(main.textContent).toContain(texts.mismatch));
    const copy = main.querySelector<HTMLButtonElement>(".copy-host button")!;
    copy.focus();
    window.history.pushState(null, "", "/tok/diff/src/A.hs?ws=ignore");
    window.dispatchEvent(new PopStateEvent("popstate"));
    await ready(main);
    await new Promise((r) => setTimeout(r, 50));
    expect(document.activeElement).toBe(copy);
  });

  describe("side by side (D-344)", () => {
    // The side of the focused cell, and its row's key.
    const at = () => {
      const c = document.activeElement?.closest<HTMLElement>(".code-cell");
      return {
        side: c?.dataset.side,
        key: c?.closest(".code-row")?.getAttribute("data-key"),
      };
    };
    const key = (k: string) =>
      document.activeElement?.dispatchEvent(
        new KeyboardEvent("keydown", { key: k, bubbles: true }),
      );
    const cellOf = (main: HTMLElement, k: number, side: string) =>
      main.querySelector<HTMLElement>(
        `.code-row[data-key="${k}"] .code-cell[data-side="${side}"]`,
      )!;

    it("four cells a row; removed and added lines paired", async () => {
      const { main } = open("/tok/diff/src/A.hs?layout=split");
      const g = await ready(main);
      // Gap, 3 equal, the pair, 3 equal, gap.
      expect(g.getAttribute("aria-rowcount")).toBe("9");
      expect(g.getAttribute("aria-colcount")).toBe("4");
      const pair = main.querySelector('.code-row[data-key="10"]')!;
      expect(
        [...pair.querySelectorAll(".code-num")].map((n) =>
          n.getAttribute("aria-label"),
        ),
      ).toEqual(["removed line 11", "added line 11"]);
      expect(
        [...pair.querySelectorAll(".code-cell")].map((c) => c.textContent),
      ).toEqual(["xold foo", "new bar"]);
      // The code cell points to its number.
      const base = cellOf(main, 10, "base");
      expect(base.getAttribute("aria-describedby")).toBe("diff-b-10");
      expect(main.querySelector("#diff-b-10")?.getAttribute("aria-label")).toBe(
        "removed line 11",
      );
      expect(
        main
          .querySelector('.code-row[data-key="9"] .code-num')
          ?.getAttribute("aria-label"),
      ).toBe("base line 10, unchanged");
      expect(
        main.querySelector(".diff-gap-cell")?.getAttribute("aria-colspan"),
      ).toBe("4");
      // Same-file links keep the layout.
      await vi.waitFor(() =>
        expect(base.querySelectorAll("a.code-link")).toHaveLength(2),
      );
      expect(
        [...base.querySelectorAll("a.code-link")].map((a) =>
          a.getAttribute("href"),
        ),
      ).toEqual([
        "/tok/diff/src/A.hs?layout=split#base-L1",
        "/tok/src/base/src/B.hs#L3",
      ]);
    });

    it("← and → cross sides at the ends; ↓ keeps the side", async () => {
      const { main } = open("/tok/diff/src/A.hs?layout=split");
      await ready(main);
      const base = cellOf(main, 10, "base");
      await vi.waitFor(() =>
        expect(base.querySelectorAll("a.code-link")).toHaveLength(2),
      );
      base.focus();
      key("ArrowRight");
      expect(document.activeElement?.textContent).toBe("old");
      key("ArrowRight");
      expect(document.activeElement?.textContent).toBe("foo");
      key("ArrowRight");
      expect(at()).toEqual({ side: "head", key: "10" });
      key("ArrowLeft");
      expect(at()).toEqual({ side: "base", key: "10" });
      expect(document.activeElement).toBe(base);
      key("ArrowDown");
      expect(at()).toEqual({ side: "base", key: "11" });
    });

    // .diff-text's width, and a way to change it (the observers run,
    // the measure comes in a frame).
    const fakeWidth = (initial: number) => {
      let width = initial;
      vi.spyOn(
        HTMLElement.prototype,
        "getBoundingClientRect",
      ).mockImplementation(function (this: HTMLElement) {
        if (this.classList.contains("diff-rem-probe"))
          return new DOMRect(0, 0, 16, 0);
        if (this.classList.contains("diff-text"))
          return new DOMRect(0, 0, width, 600);
        return Element.prototype.getBoundingClientRect.call(this);
      });
      const observers: ResizeObserverCallback[] = [];
      vi.stubGlobal(
        "ResizeObserver",
        class {
          constructor(cb: ResizeObserverCallback) {
            observers.push(cb);
          }
          observe(): void {}
          unobserve(): void {}
          disconnect(): void {}
        },
      );
      return (w: number) => {
        width = w;
        for (const cb of observers) cb([], {} as ResizeObserver);
      };
    };
    const frame = () => new Promise((r) => requestAnimationFrame(r));

    it("too narrow: unified and a note; wide again: split, focus kept", async () => {
      const resize = fakeWidth(1000);
      const { main } = open("/tok/diff/src/A.hs?layout=split");
      await ready(main);
      expect(main.querySelector(".diff-half")).not.toBeNull();
      cellOf(main, 10, "base").focus();
      resize(900);
      await vi.waitFor(() =>
        expect(main.textContent).toContain(texts.tooNarrow),
      );
      expect(main.querySelector(".diff-text .file-note")?.textContent).toBe(
        "Side by side needs a wider window: showing unified.",
      );
      expect(main.querySelector(".diff-half")).toBeNull();
      // Unified row 10 is the removed line.
      await vi.waitFor(() => expect(at()).toEqual({ side: "base", key: "10" }));
      // Not back before 62rem.
      resize(980);
      await frame();
      await frame();
      expect(main.querySelector(".diff-half")).toBeNull();
      resize(1000);
      await vi.waitFor(() =>
        expect(main.querySelector(".diff-half")).not.toBeNull(),
      );
      expect(main.textContent).not.toContain(texts.tooNarrow);
      await vi.waitFor(() => expect(at()).toEqual({ side: "base", key: "10" }));
    });

    it("a switch takes no focus the grid didn't have", async () => {
      const resize = fakeWidth(1000);
      const { main } = open("/tok/diff/src/A.hs?layout=split");
      await ready(main);
      const whole = [...main.querySelectorAll("button")].find(
        (b) => b.textContent === texts.wholeFile,
      )!;
      whole.focus();
      resize(900);
      await vi.waitFor(() =>
        expect(main.querySelector(".diff-half")).toBeNull(),
      );
      await frame();
      expect(document.activeElement).toBe(whole);
    });

    it("a switch keeps the cursor row's place on screen", async () => {
      const lines = (p: string) =>
        file(Array.from({ length: 200 }, (_, i) => `${p}${i + 1}\n`).join(""));
      const resize = fakeWidth(900);
      const { main } = open("/tok/diff/src/A.hs?layout=split", {
        "/tok/api/diff/src/A.hs": [
          200,
          diffBody({
            diff: {
              kind: "lines",
              runs: [
                ["delete", 200],
                ["insert", 200],
              ],
              crlfLines: 0,
              bom: null,
            },
          }),
        ],
        "/tok/api/file/base/src/A.hs": [200, lines("b")],
        "/tok/api/file/head/src/A.hs": [200, lines("h")],
      });
      const g = await ready(main);
      // Unified: row 250 (head's line 51) 100 px under the top.
      expect(main.querySelector(".diff-half")).toBeNull();
      g.scrollTop = 250 * 21 - 100;
      g.dispatchEvent(new Event("scroll"));
      await vi.waitFor(() =>
        expect(main.querySelector('.code-row[data-key="250"]')).not.toBeNull(),
      );
      main
        .querySelector<HTMLElement>('.code-row[data-key="250"] .code-cell')!
        .focus();
      resize(1000);
      await vi.waitFor(() =>
        expect(main.querySelector(".diff-half")).not.toBeNull(),
      );
      // Split row 50 holds it: 100 px under the top again.
      expect(grid(main)!.scrollTop).toBe(50 * 21 - 100);
      await vi.waitFor(() => expect(at()).toEqual({ side: "head", key: "50" }));
    });

    it("Back: the hashed row's other half is a new entry", async () => {
      const { main } = open("/tok/diff/src/A.hs?layout=split#base-L9");
      await ready(main);
      await vi.waitFor(() =>
        expect(
          main.querySelector('.code-row[data-key="8"] .code-line-target'),
        ).not.toBeNull(),
      );
      cellOf(main, 8, "head").focus();
      const push = vi.spyOn(window.history, "pushState");
      press("2", "Digit2");
      await vi.waitFor(() =>
        expect(window.location.pathname + window.location.hash).toBe(
          "/tok/src/head/src/A.hs#L9",
        ),
      );
      expect(push.mock.calls[0]?.[2]).toBe(
        "/tok/diff/src/A.hs?layout=split#head-L9",
      );
    });

    it("n / p never stay on the cursor's split row", async () => {
      // Split rows: 0 (blocks' first pair), 1-2, 3 (second pair), ...
      const { main } = open("/tok/diff/src/A.hs?layout=split", steps);
      await ready(main);
      cellOf(main, 3, "head").focus();
      press("p");
      expect(at()).toEqual({ side: "base", key: "0" });
      cellOf(main, 3, "base").focus();
      press("n");
      expect(at()).toEqual({ side: "base", key: "13" });
      press("p");
      expect(at()).toEqual({ side: "base", key: "3" });
    });

    it("an added-first block: p from base leaves the row; lands on head", async () => {
      const { main } = open("/tok/diff/src/A.hs?layout=split", {
        "/tok/api/diff/src/A.hs": [
          200,
          diffBody({
            diff: {
              kind: "lines",
              runs: [
                ["insert", 1],
                ["delete", 1],
                ["equal", 2],
                ["insert", 1],
                ["delete", 1],
                ["equal", 3],
              ],
              crlfLines: 0,
              bom: null,
            },
          }),
        ],
        "/tok/api/file/base/src/A.hs": [
          200,
          file("b1\nb2\nb3\nb4\nb5\nb6\nb7\n"),
        ],
        "/tok/api/file/head/src/A.hs": [
          200,
          file("h1\nh2\nh3\nh4\nh5\nh6\nh7\n"),
        ],
      });
      await ready(main);
      cellOf(main, 3, "base").focus();
      press("p");
      expect(at()).toEqual({ side: "head", key: "0" });
      press("n");
      expect(at()).toEqual({ side: "head", key: "3" });
    });
  });
});
