// The source view in diff mode (D-348): signs, n / p, Show diff here.
import { afterEach, beforeEach, describe, expect, it, vi } from "vitest";
import { cbor, stubFetch } from "../../test/fixtures";
import { fakeLayout } from "../../test/layout";
import { mountAppAt, unmountApp } from "../../test/mount";
import { diffSession } from "../../test/sessions";
import { defaultPrefs } from "../api/prefs";

const text = (n: number) =>
  Array.from({ length: n }, (_, i) => `l${i + 1}\n`).join("");

const file = (n: number, generation = 1) =>
  cbor({ generation, filtered: false, kind: "text", text: text(n) });

const change = (
  path: string,
  status: string,
  oldPath: string | null = null,
) => ({
  path,
  oldPath,
  status,
  similarity: status === "renamed" ? 90 : null,
  note: null,
});

const diffFiles = (files: unknown[], truncated = false) => ({
  generation: 1,
  base: { label: "main", commit: "a".repeat(40) },
  head: { label: "feature", commit: "b".repeat(40) },
  files,
  outside: null,
  renameLimitHit: false,
  truncated,
});

const diffBody = (runs: unknown[], over: Record<string, unknown> = {}) =>
  cbor({
    generation: 1,
    path: "src/A.hs",
    oldPath: null,
    status: "modified",
    similarity: null,
    note: null,
    ignoreWhitespace: false,
    base: { kind: "text" },
    head: { kind: "text" },
    diff: { kind: "lines", runs, crlfLines: 0, bom: null },
    ...over,
  });

// Head: 6 lines, 3-4 modified. Base: 5 lines.
const modifiedRuns = [
  ["equal", 2],
  ["delete", 1],
  ["insert", 2],
  ["equal", 2],
];

function open(
  url: string,
  routes: Record<string, [number, unknown]> = {},
  browse = false,
) {
  const fetch = stubFetch({
    "/tok/api/diff-files": [
      200,
      diffFiles([
        change("src/A.hs", "modified"),
        change("src/New.hs", "added"),
        change("src/Old.hs", "removed"),
        change("src/R2.hs", "renamed", "src/R1.hs"),
      ]),
    ],
    "/tok/api/prefs": [200, defaultPrefs],
    "/tok/api/file/head/src/A.hs": [200, file(6)],
    "/tok/api/file/base/src/A.hs": [200, file(5)],
    "/tok/api/diff/src/A.hs": [200, diffBody(modifiedRuns)],
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

const labels = (main: HTMLElement) =>
  [...main.querySelectorAll('[role="rowheader"]')].map((r) =>
    r.getAttribute("aria-label"),
  );

async function ready(main: HTMLElement) {
  await vi.waitFor(() =>
    expect(main.querySelector(".code-view")).not.toBeNull(),
  );
}

const button = (main: HTMLElement, name: string) =>
  [...main.querySelectorAll("button")].find((b) =>
    b.textContent?.startsWith(name),
  );

const press = (key: string, code = `Key${key.toUpperCase()}`) =>
  (document.activeElement ?? document.body).dispatchEvent(
    new KeyboardEvent("keydown", {
      key,
      code,
      bubbles: true,
      cancelable: true,
    }),
  );

const status = () => document.querySelector(".keys-status")?.textContent;

beforeEach(() => fakeLayout());

afterEach(() => {
  unmountApp();
  vi.unstubAllGlobals();
  vi.restoreAllMocks();
});

describe("source view signs", () => {
  it("head: modified lines get the word, one diff fetch", async () => {
    const { main, fetch } = open("/tok/src/head/src/A.hs");
    await ready(main);
    await vi.waitFor(() =>
      expect(labels(main)).toEqual([
        "1",
        "2",
        "3, modified",
        "4, modified",
        "5",
        "6",
      ]),
    );
    expect(main.querySelector(".code-view-changes")).not.toBeNull();
    expect(
      urls(fetch).filter((u) => u === "/tok/api/diff/src/A.hs"),
    ).toHaveLength(1);
  });

  it("base: a pure insert says added below on the line before", async () => {
    const { main } = open("/tok/src/base/src/A.hs", {
      "/tok/api/diff/src/A.hs": [
        200,
        diffBody([
          ["equal", 2],
          ["insert", 1],
          ["equal", 3],
        ]),
      ],
    });
    await ready(main);
    await vi.waitFor(() =>
      expect(labels(main)[1]).toBe("2, lines added below"),
    );
  });

  it("added and removed files: all lines, no fetch", async () => {
    const a = open("/tok/src/head/src/New.hs", {
      "/tok/api/file/head/src/New.hs": [200, file(2)],
    });
    await ready(a.main);
    await vi.waitFor(() =>
      expect(labels(a.main)).toEqual(["1, added", "2, added"]),
    );
    expect(urls(a.fetch).some((u) => u.startsWith("/tok/api/diff/"))).toBe(
      false,
    );
    unmountApp();
    const r = open("/tok/src/base/src/Old.hs", {
      "/tok/api/file/base/src/Old.hs": [200, file(2)],
    });
    await ready(r.main);
    await vi.waitFor(() =>
      expect(labels(r.main)).toEqual(["1, removed", "2, removed"]),
    );
    expect(urls(r.fetch).some((u) => u.startsWith("/tok/api/diff/"))).toBe(
      false,
    );
  });

  it("base of a rename: fetches the head path", async () => {
    const { main, fetch } = open("/tok/src/base/src/R1.hs", {
      "/tok/api/file/base/src/R1.hs": [200, file(3)],
      "/tok/api/diff/src/R2.hs": [
        200,
        diffBody(
          [
            ["equal", 1],
            ["delete", 1],
            ["equal", 1],
          ],
          {
            path: "src/R2.hs",
            oldPath: "src/R1.hs",
            status: "renamed",
          },
        ),
      ],
    });
    await ready(main);
    await vi.waitFor(() => expect(labels(main)[1]).toBe("2, removed"));
    expect(urls(fetch)).toContain("/tok/api/diff/src/R2.hs");
  });

  it("unchanged file: no fetch, no signs, the button says why", async () => {
    const { main, fetch } = open("/tok/src/head/src/U.hs", {
      "/tok/api/file/head/src/U.hs": [200, file(3)],
    });
    await ready(main);
    await vi.waitFor(() =>
      expect(urls(fetch)).toContain("/tok/api/diff-files"),
    );
    expect(labels(main)).toEqual(["1", "2", "3"]);
    expect(urls(fetch).some((u) => u.startsWith("/tok/api/diff/"))).toBe(false);
    button(main, "Show diff here")!.click();
    expect(status()).toBe("No changes in this file.");
    expect(button(main, "Next change")).toBeUndefined();
    press("n");
    expect(status()).toBeDefined();
  });

  it("a generation or count mismatch: no signs", async () => {
    const warn = vi.spyOn(console, "warn").mockImplementation(() => undefined);
    const a = open("/tok/src/head/src/A.hs", {
      "/tok/api/diff/src/A.hs": [
        200,
        diffBody(modifiedRuns, { generation: 2 }),
      ],
    });
    await ready(a.main);
    await vi.waitFor(() => expect(warn).toHaveBeenCalled());
    expect(labels(a.main)).toEqual(["1", "2", "3", "4", "5", "6"]);
    unmountApp();
    warn.mockClear();
    const b = open("/tok/src/head/src/A.hs", {
      "/tok/api/file/head/src/A.hs": [200, file(9)],
    });
    await ready(b.main);
    await vi.waitFor(() => expect(warn).toHaveBeenCalled());
    expect(b.main.querySelector(".code-chg")).toBeNull();
  });

  it("base keeps its marks when the diff has another generation", async () => {
    // api/diff carries head's generation; base's text has its own.
    const { main } = open("/tok/src/base/src/A.hs", {
      "/tok/api/diff/src/A.hs": [
        200,
        diffBody(modifiedRuns, { generation: 2 }),
      ],
    });
    await ready(main);
    await vi.waitFor(() => expect(labels(main)[2]).toBe("3, modified"));
  });

  it("an empty added file: n says so", async () => {
    const { main } = open("/tok/src/head/src/New.hs", {
      "/tok/api/file/head/src/New.hs": [200, file(0)],
    });
    await vi.waitFor(() =>
      expect(main.textContent).toContain("This file is empty."),
    );
    await vi.waitFor(() => {
      press("n");
      expect(status()).toBe("Empty file added.");
    });
  });

  it("too slow: a note, no signs", async () => {
    const { main } = open("/tok/src/head/src/A.hs", {
      "/tok/api/diff/src/A.hs": [
        200,
        diffBody([], { diff: { kind: "too-slow" } }),
      ],
    });
    await ready(main);
    await vi.waitFor(() =>
      expect(main.querySelector(".file-note")?.textContent).toBe(
        "The diff took over 2 s, so changes aren't marked.",
      ),
    );
    expect(main.querySelector(".code-chg")).toBeNull();
  });

  it("browse mode: no column, no button, no diff fetch", async () => {
    const { main, fetch } = open(
      "/tok/src/tree/src/A.hs",
      {
        "/tok/api/file/tree/src/A.hs": [200, file(6)],
      },
      true,
    );
    await ready(main);
    expect(main.querySelector(".code-view-changes")).toBeNull();
    expect(button(main, "Show diff here")).toBeUndefined();
    expect(button(main, "Next change")).toBeUndefined();
    expect(urls(fetch).some((u) => u.includes("/api/diff"))).toBe(false);
  });
});

describe("n / p and the buttons", () => {
  it("keys step between blocks and say when there are no more", async () => {
    const { main } = open("/tok/src/head/src/A.hs");
    await ready(main);
    await vi.waitFor(() => expect(labels(main)[2]).toBe("3, modified"));
    press("n");
    expect(
      document.activeElement?.closest(".code-row")?.getAttribute("data-line"),
    ).toBe("3");
    press("n");
    expect(status()).toBe("No more changes.");
    press("p");
    expect(status()?.trim()).toBe("No more changes.");
  });

  it("a change on line 1 is found by the first n", async () => {
    const { main } = open("/tok/src/head/src/A.hs", {
      "/tok/api/diff/src/A.hs": [
        200,
        diffBody([
          ["insert", 1],
          ["equal", 5],
        ]),
      ],
    });
    await ready(main);
    await vi.waitFor(() => expect(labels(main)[0]).toBe("1, added"));
    press("n");
    expect(
      document.activeElement?.closest(".code-row")?.getAttribute("data-line"),
    ).toBe("1");
  });

  it("buttons move the cursor, keep focus and say where", async () => {
    const { main } = open("/tok/src/head/src/A.hs");
    await ready(main);
    await vi.waitFor(() => expect(button(main, "Next change")).toBeDefined());
    const next = button(main, "Next change")!;
    next.focus();
    next.click();
    expect(document.activeElement).toBe(next);
    expect(status()).toBe("modified line 3");
    expect(
      main.querySelector(".code-cursor-line")?.getAttribute("data-line"),
    ).toBe("3");
    next.click();
    expect(status()).toBe("No more changes.");
  });
});

describe("Show diff here", () => {
  const where = () => `${location.pathname}${location.hash}`;

  it("the button and 3 go to the diff at the cursor's line", async () => {
    const { main } = open("/tok/src/head/src/A.hs#L4");
    await ready(main);
    await vi.waitFor(() => expect(labels(main)[3]).toBe("4, modified"));
    button(main, "Show diff here")!.click();
    await vi.waitFor(() => expect(where()).toBe("/tok/diff/src/A.hs#head-L4"));
  });

  it("3 from the base side of a rename", async () => {
    const { main } = open("/tok/src/base/src/R1.hs#L2", {
      "/tok/api/file/base/src/R1.hs": [200, file(3)],
      "/tok/api/diff/src/R2.hs": [
        200,
        diffBody(
          [
            ["equal", 1],
            ["delete", 1],
            ["equal", 1],
          ],
          {
            path: "src/R2.hs",
            oldPath: "src/R1.hs",
            status: "renamed",
          },
        ),
      ],
    });
    await ready(main);
    await vi.waitFor(() => expect(labels(main)[1]).toBe("2, removed"));
    press("3", "Digit3");
    await vi.waitFor(() => expect(where()).toBe("/tok/diff/src/R2.hs#base-L2"));
  });

  it("the header has a Diff item in diff mode only", async () => {
    const a = open("/tok/src/head/src/A.hs");
    await ready(a.main);
    const nav = () => document.querySelector(".view-switch");
    await vi.waitFor(() => expect(nav()?.textContent).toContain("Diff"));
    expect(
      [...nav()!.querySelectorAll("a")]
        .find((l) => l.textContent === "Diff")
        ?.getAttribute("href"),
    ).toBe("/tok/diff/src/A.hs");
    unmountApp();
    const b = open(
      "/tok/src/tree/src/A.hs",
      {
        "/tok/api/file/tree/src/A.hs": [200, file(6)],
      },
      true,
    );
    await ready(b.main);
    expect(nav()?.textContent).not.toContain("Diff");
    press("3", "Digit3");
    expect(status()).toBe("Not in diff mode.");
  });

  it("unchanged file: the Diff item is muted", async () => {
    const { main } = open("/tok/src/head/src/U.hs", {
      "/tok/api/file/head/src/U.hs": [200, file(3)],
    });
    await ready(main);
    await vi.waitFor(() =>
      expect(
        [...document.querySelectorAll(".view-switch .view-missing")].map(
          (e) => e.textContent,
        ),
      ).toContain("Diff (not available)"),
    );
    press("3", "Digit3");
    expect(status()).toBe("No changes in this file.");
  });
});
