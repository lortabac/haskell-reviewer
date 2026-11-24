// Find in the diff view (D-346): the page, the bar and the grid.
import { afterEach, beforeEach, describe, expect, it, vi } from "vitest";
import { cbor, stubFetch } from "../../test/fixtures";
import { fakeLayout } from "../../test/layout";
import { mountAppAt, unmountApp } from "../../test/mount";
import { diffSession } from "../../test/sessions";
import { defaultPrefs } from "../api/prefs";
import { typingDelay } from "../source/FindBar";

// 21 lines a side; line 11 differs. Rows: 10 equal, 1 removed, 1
// added, 10 equal. "e2" is on lines 2, 20 and 21 (all equal, in gaps).
const same = Array.from({ length: 10 }, (_, i) => `e${i + 1}`);
const after = Array.from({ length: 10 }, (_, i) => `e${i + 12}`);
const baseText = [...same, "old e2x", ...after].join("\n") + "\n";
const headText = [...same, "new", ...after].join("\n") + "\n";

const file = (text: string) =>
  cbor({ generation: 1, filtered: false, kind: "text", text });

function open(url: string) {
  stubFetch({
    "/tok/api/diff/src/A.hs": [
      200,
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
      }),
    ],
    "/tok/api/file/base/src/A.hs": [200, file(baseText)],
    "/tok/api/file/head/src/A.hs": [200, file(headText)],
    "/tok/api/source/base/src/A.hs": [404, null],
    "/tok/api/source/head/src/A.hs": [404, null],
    "/tok/api/prefs": [200, defaultPrefs],
  });
  const root = mountAppAt(url, undefined, undefined, null, diffSession);
  const main = root.querySelector("main");
  if (main === null) throw new Error("no main");
  return main;
}

const grid = (main: HTMLElement) =>
  main.querySelector<HTMLElement>('[role="grid"]');
async function ready(main: HTMLElement): Promise<HTMLElement> {
  await vi.waitFor(() => expect(grid(main)).not.toBeNull());
  return grid(main)!;
}
const input = (main: HTMLElement) =>
  main.querySelector<HTMLInputElement>(".code-find-bar input")!;
const live = (main: HTMLElement) =>
  main.querySelector(".code-find-status")?.textContent;
const press = (key: string) =>
  (document.activeElement ?? document.body).dispatchEvent(
    new KeyboardEvent("keydown", {
      key,
      code: `Key${key.toUpperCase()}`,
      bubbles: true,
      cancelable: true,
    }),
  );
const enter = (el: Element, shiftKey = false) =>
  el.dispatchEvent(
    new KeyboardEvent("keydown", { key: "Enter", shiftKey, bubbles: true }),
  );
function type(main: HTMLElement, s: string): void {
  const i = input(main);
  i.value = s;
  i.dispatchEvent(new Event("input", { bubbles: true }));
}
const said = async (main: HTMLElement, text: string) => {
  await new Promise((r) => setTimeout(r, typingDelay + 20));
  expect(live(main)).toBe(text);
};
const rowOf = (main: HTMLElement, label: string) =>
  [...main.querySelectorAll<HTMLElement>(".code-row")].find(
    (r) => r.querySelector(`[aria-label="${label}"]`) !== null,
  );

beforeEach(() => fakeLayout());

afterEach(() => {
  unmountApp();
  vi.unstubAllGlobals();
  vi.restoreAllMocks();
});

describe("find in the diff view", () => {
  it("the Find button first, FindBar's hint, no old hint", async () => {
    const main = open("/tok/diff/src/A.hs");
    await ready(main);
    const first = main.querySelector(".code-toolbar button");
    expect(first?.textContent).toBe("Find");
    expect(main.querySelector(".code-hint")?.textContent).toContain(
      "use Find (f) to search the whole file",
    );
    expect(main.textContent).not.toContain("Browser find sees only the lines");
  });

  it("f opens the bar; hits in gaps count and open on a move", async () => {
    const main = open("/tok/diff/src/A.hs");
    await ready(main);
    expect(rowOf(main, "line 2, unchanged")).toBeUndefined();
    press("f");
    expect(document.activeElement).toBe(input(main));
    type(main, "e2");
    // Line 2 (head), removed line 11, lines 20 and 21.
    await said(main, "Match 1 of 4, line 2, unchanged");
    await vi.waitFor(() =>
      expect(rowOf(main, "line 2, unchanged")).toBeDefined(),
    );
    enter(input(main));
    expect(live(main)).toBe("Match 2 of 4, removed line 11");
    enter(input(main));
    expect(live(main)).toBe("Match 3 of 4, line 20, unchanged");
    await vi.waitFor(() =>
      expect(rowOf(main, "line 20, unchanged")).toBeDefined(),
    );
    expect(document.activeElement).toBe(input(main));
  });

  it("Esc in the bar focuses the hit; the gap stays open", async () => {
    const main = open("/tok/diff/src/A.hs");
    await ready(main);
    press("f");
    type(main, "e2");
    enter(input(main));
    enter(input(main));
    input(main).dispatchEvent(
      new KeyboardEvent("keydown", { key: "Escape", bubbles: true }),
    );
    const row = rowOf(main, "line 20, unchanged");
    await vi.waitFor(() =>
      expect(document.activeElement).toBe(row?.querySelector(".code-cell")),
    );
    expect(main.querySelector(".code-find-bar")?.hasAttribute("hidden")).toBe(
      true,
    );
    expect(rowOf(main, "line 20, unchanged")).toBeDefined();
  });

  it("the current hit's row is marked", async () => {
    const main = open("/tok/diff/src/A.hs");
    await ready(main);
    press("f");
    type(main, "e2x");
    await vi.waitFor(() =>
      expect(
        main.querySelector(".code-find-line [role=rowheader]"),
      ).not.toBeNull(),
    );
    expect(
      main
        .querySelector(".code-find-line [role=rowheader]")
        ?.getAttribute("aria-label"),
    ).toBe("removed line 11");
  });

  it("split: equal rows twice; a switch searches again", async () => {
    const main = open("/tok/diff/src/A.hs?layout=split");
    await ready(main);
    expect(main.querySelector(".diff-half")).not.toBeNull();
    press("f");
    type(main, "e2");
    await said(main, "Match 1 of 7, base line 2, unchanged");
    enter(input(main));
    expect(live(main)).toBe("Match 2 of 7, line 2, unchanged");
    enter(input(main));
    expect(live(main)).toBe("Match 3 of 7, removed line 11");
    const unified = [...main.querySelectorAll("button")].find(
      (b) => b.textContent === "Unified",
    );
    unified?.click();
    await vi.waitFor(() => expect(main.querySelector(".diff-half")).toBeNull());
    await said(main, "Match 2 of 4, removed line 11");
  });
});
