import { createSignal } from "solid-js";
import type { Indexing } from "../api/events";
import { afterEach, beforeEach, describe, expect, it, vi } from "vitest";
import { cbor, cborFixture, fixture, stubFetch } from "../../test/fixtures";
import { mountAppAt, unmountApp } from "../../test/mount";
import { clearSourceCache, lastLine } from "./pageSource";

// The Page fixture: area, perimeter on line 38, "area _ = 0" on 39.
const target = [
  "decl",
  "Other.Mod",
  "Other.hs",
  10,
  "val:foo",
  [],
  1,
  "foo :: Int",
  false,
];
const guess = [
  "decl",
  "Page",
  "Page.hs",
  38,
  "val:area",
  ["ambiguous"],
  2,
  null,
  true,
];
const lexed = (over: Record<string, unknown> = {}) => ({
  generation: 1,
  path: "Page.hs",
  ref: "Page",
  tier: { kind: "syntactic", reason: "no-hie" },
  partial: false,
  highlight: "lexed",
  // Line 38: "area" (variable, link 1), line 39: "area" (link 2) and
  // "_" is a local (no link).
  runs: [38, 1, 0, 5, 2, 1, 1, 1, 0, 5, 2, 2, 0, 6, 0, 7, 2, 3],
  regions: [],
  targets: [target, guess, ["local", 39, 6]],
  outline: [],
  ...over,
});

const base = {
  "/tok/api/files/tree": [200, fixture("files")],
  "/tok/api/modules/tree": [200, fixture("modules")],
  "/tok/api/module/tree/Page": [200, cborFixture("module-page")],
} satisfies Record<string, [number, unknown]>;

function open(source: [number, unknown] = [200, cbor(lexed())]) {
  const mock = stubFetch({ ...base, "/tok/api/source/tree/Page.hs": source });
  const main = mountAppAt("/tok/docs/tree/Page").querySelector("main");
  if (main === null) throw new Error("no main");
  return { main, mock };
}

const calls = (mock: ReturnType<typeof stubFetch>) =>
  mock.mock.calls.filter((c) => String(c[0] as string).includes("/api/source/"))
    .length;

async function toggle(main: HTMLElement, label: string): Promise<HTMLElement> {
  const b = await vi.waitFor(() => {
    const x = [...main.querySelectorAll("button.decl-source-toggle")].find(
      (e) => e.textContent.includes(label),
    );
    expect(x).toBeDefined();
    return x as HTMLButtonElement;
  });
  b.click();
  const src = b.closest("section")?.querySelector<HTMLElement>(".src");
  if (src == null) throw new Error("no source");
  return src;
}

beforeEach(() => clearSourceCache());
afterEach(() => {
  unmountApp();
  vi.unstubAllGlobals();
  vi.restoreAllMocks();
});

describe("inline source", () => {
  it("nothing asked while closed; plain text first", async () => {
    const { main, mock } = open();
    await vi.waitFor(() => expect(main.querySelector(".decl")).not.toBeNull());
    expect(calls(mock)).toBe(0);
    const src = await toggle(main, "of area");
    expect(src.textContent).toContain("38area, perimeter :: Shape -> Double");
    expect(src.querySelector("a")).toBeNull();
  });

  it("highlighted and linked when the answer comes; one request", async () => {
    const { main, mock } = open();
    await vi.waitFor(() => expect(main.querySelector(".decl")).not.toBeNull());
    const src = await toggle(main, "of area");
    await vi.waitFor(() =>
      expect(src.querySelector(".hl-variable")).not.toBeNull(),
    );
    const row = src.querySelector('.src-row[data-line="38"]');
    const a = row?.querySelector("a.code-link");
    expect(a?.textContent).toBe("area");
    expect(a?.getAttribute("href")).toBe("/tok/docs/tree/Other.Mod#val%3Afoo");
    expect(a?.getAttribute("tabindex")).toBeNull();
    expect(a?.getAttribute("aria-label")).toBe(
      "area, documentation in Other.Mod",
    );
    // Same module: as typed in the URL; a guess says so.
    const b = src.querySelector('.src-row[data-line="39"] a.code-link');
    expect(b?.getAttribute("href")).toBe("/tok/docs/tree/Page#val%3Aarea");
    expect(b?.classList.contains("code-link-guess")).toBe(true);
    // A local: no link.
    expect(src.querySelectorAll('.src-row[data-line="39"] a').length).toBe(1);
    // The text is the same.
    expect(src.textContent).toContain("39area _ = 0");
    // A second block: no new request.
    await toggle(main, "of <+>");
    expect(calls(mock)).toBe(1);
  });

  it("another snapshot: plain", async () => {
    const { main, mock } = open([200, cbor(lexed({ generation: 2 }))]);
    await vi.waitFor(() => expect(main.querySelector(".decl")).not.toBeNull());
    const src = await toggle(main, "of area");
    await vi.waitFor(() => expect(calls(mock)).toBe(1));
    await new Promise((r) => setTimeout(r, 20));
    expect(src.querySelector("a, .hl-variable")).toBeNull();
  });

  it("a failed answer: plain", async () => {
    const { main, mock } = open([500, "no"]);
    await vi.waitFor(() => expect(main.querySelector(".decl")).not.toBeNull());
    const src = await toggle(main, "of area");
    await vi.waitFor(() => expect(calls(mock)).toBe(1));
    await new Promise((r) => setTimeout(r, 20));
    expect(src.querySelector("a")).toBeNull();
    expect(src.textContent).toContain("area _ = 0");
  });

  it("a page seen again: no new request", async () => {
    const first = open();
    await vi.waitFor(() =>
      expect(first.main.querySelector(".decl")).not.toBeNull(),
    );
    const src = await toggle(first.main, "of area");
    await vi.waitFor(() => expect(src.querySelector("a")).not.toBeNull());
    unmountApp();
    const second = open();
    await vi.waitFor(() =>
      expect(second.main.querySelector(".decl")).not.toBeNull(),
    );
    const again = await toggle(second.main, "of area");
    await vi.waitFor(() => expect(again.querySelector("a")).not.toBeNull());
    expect(calls(second.mock)).toBe(0);
  });
});

describe("refetch while indexing", () => {
  const at = (ready: number) => ({
    generation: 1,
    total: 2,
    ready,
    failed: 0,
  });

  it("a changed answer: focus goes back to the link at the same spot", async () => {
    let now = 1000;
    vi.spyOn(Date, "now").mockImplementation(() => now);
    const changed = lexed({
      partial: true,
      targets: [
        ["decl", "Third", "Third.hs", 1, "val:foo", [], 1, null, false],
        guess,
        ["local", 39, 6],
      ],
    });
    const answers = [lexed({ partial: true }), changed];
    let calls = 0;
    const plain = stubFetch(base).getMockImplementation()!;
    vi.stubGlobal(
      "fetch",
      vi.fn((input: RequestInfo | URL) => {
        if (input !== "/tok/api/source/tree/Page.hs") return plain(input);
        const body = answers[Math.min(calls++, answers.length - 1)];
        return Promise.resolve(
          new Response(new Uint8Array(cbor(body)), {
            status: 200,
            headers: { "Content-Type": "application/cbor" },
          }),
        );
      }),
    );
    const [ix, setIx] = createSignal<Indexing>();
    // eslint-disable-next-line solid/reactivity -- read by the page
    const main = mountAppAt("/tok/docs/tree/Page", undefined, ix).querySelector(
      "main",
    )!;
    await vi.waitFor(() => expect(main.querySelector(".decl")).not.toBeNull());
    const src = await toggle(main, "of area");
    const find = () =>
      src.querySelector<HTMLElement>('.src-row[data-line="38"] a.code-link');
    await vi.waitFor(() => expect(find()).not.toBeNull());
    const old = find()!;
    old.focus();
    old.dispatchEvent(new FocusEvent("focusin", { bubbles: true }));
    expect(document.activeElement).toBe(old);
    now = 3000;
    setIx(at(1));
    await vi.waitFor(() => expect(calls).toBe(2));
    await vi.waitFor(() =>
      expect(find()?.getAttribute("href")).toContain("Third"),
    );
    expect(find()).not.toBe(old);
    expect(document.activeElement).toBe(find());
  });
});

describe("a done answer equal to a partial one", () => {
  it("is kept for the next visit", async () => {
    let now = 1000;
    vi.spyOn(Date, "now").mockImplementation(() => now);
    const answers = [lexed({ partial: true }), lexed()];
    let calls = 0;
    const plain = stubFetch(base).getMockImplementation()!;
    vi.stubGlobal(
      "fetch",
      vi.fn((input: RequestInfo | URL) => {
        if (input !== "/tok/api/source/tree/Page.hs") return plain(input);
        const body = answers[Math.min(calls++, answers.length - 1)];
        return Promise.resolve(
          new Response(new Uint8Array(cbor(body)), {
            status: 200,
            headers: { "Content-Type": "application/cbor" },
          }),
        );
      }),
    );
    const [ix, setIx] = createSignal<Indexing>();
    // eslint-disable-next-line solid/reactivity -- read by the page
    const main = mountAppAt("/tok/docs/tree/Page", undefined, ix).querySelector(
      "main",
    )!;
    await vi.waitFor(() => expect(main.querySelector(".decl")).not.toBeNull());
    await toggle(main, "of area");
    await vi.waitFor(() => expect(calls).toBe(1));
    now = 3000;
    // Done: 2 of 2.
    setIx({ generation: 1, total: 2, ready: 2, failed: 0 });
    await vi.waitFor(() => expect(calls).toBe(2));
    await new Promise((r) => setTimeout(r, 20));
    unmountApp();
    const again = mountAppAt("/tok/docs/tree/Page").querySelector("main")!;
    await vi.waitFor(() => expect(again.querySelector(".decl")).not.toBeNull());
    const src = await toggle(again, "of area");
    await vi.waitFor(() => expect(src.querySelector("a")).not.toBeNull());
    expect(calls).toBe(2);
  });
});

describe("links out of signatures and names", () => {
  it("a token that links out: Hoogle, marks, short name", async () => {
    const { main } = open();
    await vi.waitFor(() => expect(main.querySelector(".decl")).not.toBeNull());
    const sig = [...main.querySelectorAll(".decl-head .sig")].find((s) =>
      s.textContent.startsWith("area, perimeter"),
    );
    const a = [...(sig?.querySelectorAll("a") ?? [])].find(
      (x) => x.textContent === "Double",
    );
    expect(a?.getAttribute("href")).toBe(
      "https://hoogle.haskell.org/?hoogle=Double%20%2Bmodule%3APrelude",
    );
    expect(a?.getAttribute("target")).toBe("_blank");
    expect(a?.getAttribute("rel")).toBe("noopener noreferrer");
    expect(a?.className).toContain("link-guess");
    expect(a?.getAttribute("aria-label")).toBe(
      "Double, resolved heuristically, opens in a new tab",
    );
  });
});

describe("lastLine", () => {
  it("runs and regions", () => {
    const s = lexed({ regions: [1, 3, 90] });
    expect(lastLine(s as never)).toBe(90);
    expect(lastLine(lexed({ runs: [], regions: [] }) as never)).toBe(0);
    expect(lastLine(lexed() as never)).toBe(39);
  });
});
