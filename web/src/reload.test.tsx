// Reload in the client (D-354): the button and r, the POST errors, the
// apply (the page mounts again, providers refetch), the docs page's
// place, and gone -> Home.
import { createRoot, createSignal } from "solid-js";
import { render } from "solid-js/web";
import { afterEach, beforeEach, describe, expect, it, vi } from "vitest";
import { cbor, cborFixture, fixture, stubFetch } from "../test/fixtures";
import { fakeLayout } from "../test/layout";
import { mountAppAt, unmountApp } from "../test/mount";
import { diffSession, worktreeSession } from "../test/sessions";
import { defaultPrefs } from "./api/prefs";
import type { Session } from "./api/session";
import { App } from "./App";
import { ConnectionContext } from "./events/ConnectionNotice";
import {
  createGenerationState,
  GenerationContext,
  type GenerationState,
} from "./events/generation";
import type { Status } from "./events/protocol";
import { reloadTexts } from "./header/ReloadButton";
import { fallbackRowHeight } from "./source/virtual";

const pageAt = (generation: number) =>
  cbor({ ...(fixture("module-page") as object), generation });

let routes: Record<string, [number, unknown]>;
let gets: ReturnType<typeof stubFetch>;
// POST api/reload: answered by hand.
let posts: { resolve: (r: Response) => void }[];
let dispose: (() => void) | undefined;

function stub(more: Record<string, [number, unknown]> = {}) {
  routes = { ...routes, ...more };
  gets = stubFetch(routes);
  vi.stubGlobal("fetch", (input: RequestInfo | URL, init?: RequestInit) => {
    if (init?.method === "POST") {
      return new Promise<Response>((resolve) => posts.push({ resolve }));
    }
    return gets(input);
  });
}

const json = (status: number, body: unknown) =>
  new Response(JSON.stringify(body), {
    status,
    headers: { "Content-Type": "application/json" },
  });

beforeEach(() => {
  fakeLayout();
  posts = [];
  routes = {
    "/tok/api/files/tree": [200, fixture("files")],
    "/tok/api/modules/tree": [200, fixture("modules")],
    "/tok/api/module/tree/Page": [200, pageAt(1)],
    "/tok/api/prefs": [200, defaultPrefs],
  };
  stub();
});

afterEach(() => {
  dispose?.();
  dispose = undefined;
  unmountApp();
  vi.unstubAllGlobals();
  vi.restoreAllMocks();
});

const button = () => document.querySelector<HTMLElement>(".reload-button");
const status = () => document.querySelector(".keys-status")?.textContent;
const calls = (url: string) =>
  gets.mock.calls.filter(([u]) => u === url).length;
const press = (key: string) =>
  document.body.dispatchEvent(
    new KeyboardEvent("keydown", {
      key,
      code: `Key${key.toUpperCase()}`,
      bubbles: true,
      cancelable: true,
    }),
  );
const flush = () => new Promise((r) => setTimeout(r, 0));

// The app with a connection status we set, and a generation state.
function mountWith(
  path: string,
  session?: Session,
  gen: GenerationState = createRoot(() => createGenerationState()),
) {
  const [conn, setConn] = createSignal<Status>("open");
  window.history.replaceState(null, "", path);
  const root = document.createElement("div");
  document.body.append(root);
  dispose = render(
    () => (
      <ConnectionContext.Provider
        value={{ status: conn, fallback: () => false }}
      >
        <GenerationContext.Provider value={gen}>
          <App session={session} />
        </GenerationContext.Provider>
      </ConnectionContext.Provider>
    ),
    root,
  );
  return { gen, setConn };
}

describe("the Reload button (D-354)", () => {
  it.each([
    ["browse mode", undefined, true],
    ["--diff REV", worktreeSession, true],
    ["a commit range", diffSession, false],
  ])("%s: shown %s", (_, session, shown) => {
    mountAppAt("/tok/", undefined, undefined, null, session);
    expect(button() !== null).toBe(shown);
  });

  it("is first in the header controls, with the key hint", () => {
    mountAppAt("/tok/");
    const b = button();
    expect(b?.textContent).toBe("Reload (r)");
    expect(b?.parentElement?.querySelector("button")).toBe(b);
  });

  it("one POST at a time; aria-disabled and said while it waits", async () => {
    const { gen } = mountWith("/tok/");
    gen.seen(1);
    button()?.click();
    button()?.click();
    press("r");
    expect(posts).toHaveLength(1);
    expect(button()?.getAttribute("aria-disabled")).toBe("true");
    expect(status()).toBe(reloadTexts.pending);
    posts[0]?.resolve(json(200, { generation: 2 }));
    await vi.waitFor(() => expect(status()).toBe(reloadTexts.done));
    expect(button()?.getAttribute("aria-disabled")).toBeNull();
    expect(gen.current()).toBe(2);
  });

  it.each([
    [500, reloadTexts.listing],
    [503, reloadTexts.busy],
    [404, reloadTexts.unavailable],
    [400, "Couldn't reload: HTTP 400."],
  ])("HTTP %i: says so", async (code, words) => {
    mountWith("/tok/");
    button()?.click();
    posts[0]?.resolve(json(code, "no"));
    await vi.waitFor(() => expect(status()).toBe(words));
    expect(button()?.getAttribute("aria-disabled")).toBeNull();
  });

  it("a network error: says so", async () => {
    mountWith("/tok/");
    vi.stubGlobal("fetch", () => Promise.reject(new Error("boom")));
    button()?.click();
    await vi.waitFor(() =>
      expect(status()).toBe("Couldn't reload: Error: boom."),
    );
  });

  it("session ended: aria-disabled, the reason said, no POST", async () => {
    const { setConn } = mountWith("/tok/");
    setConn("ended");
    await flush();
    expect(button()?.getAttribute("aria-disabled")).toBe("true");
    const id = button()?.getAttribute("aria-describedby") ?? "";
    expect(document.getElementById(id)?.textContent).toBe(reloadTexts.ended);
    button()?.click();
    expect(posts).toHaveLength(0);
    expect(status()).toBe(reloadTexts.ended);
  });

  it("r with no reload in the session: says so", async () => {
    mountAppAt("/tok/", undefined, undefined, null, diffSession);
    await flush();
    press("r");
    expect(posts).toHaveLength(0);
    expect(status()).toBe(reloadTexts.unavailable);
  });

  it("r with shortcuts off: nothing", () => {
    mountAppAt("/tok/", { ...defaultPrefs, singleKeyShortcuts: false });
    expect(button()?.textContent).toBe("Reload");
    press("r");
    expect(posts).toHaveLength(0);
  });
});

describe("applying a reload (D-354)", () => {
  const decl = () => document.querySelector<HTMLElement>("section.decl");

  it("a rise: the page and the providers fetch again, no new entry", async () => {
    const { gen } = mountWith("/tok/docs/tree/Page");
    await vi.waitFor(() => expect(decl()).not.toBeNull());
    expect(gen.current()).toBe(1);
    const page = "/tok/api/module/tree/Page";
    expect(calls(page)).toBe(1);
    expect(calls("/tok/api/modules/tree")).toBe(1);
    expect(calls("/tok/api/files/tree")).toBe(1);
    const old = decl();
    const entries = history.length;
    stub({ [page]: [200, pageAt(2)] });
    gen.seen(2);
    await vi.waitFor(() => expect(status()).toBe(reloadTexts.done));
    await vi.waitFor(() => expect(decl()).not.toBeNull());
    expect(decl()).not.toBe(old);
    expect(calls(page)).toBe(1);
    expect(calls("/tok/api/modules/tree")).toBe(1);
    expect(calls("/tok/api/files/tree")).toBe(1);
    expect(history.length).toBe(entries);
    // One rise, one apply: nothing more comes.
    await flush();
    expect(calls(page)).toBe(1);
    expect(gen.applied()).toEqual({ n: 1, notice: true });
  });

  it("the docs page keeps the focused declaration, by its id", async () => {
    const { gen } = mountWith("/tok/docs/tree/Page");
    await vi.waitFor(() => expect(decl()).not.toBeNull());
    const all = [...document.querySelectorAll<HTMLElement>("section.decl")];
    const target = all[1] ?? all[0];
    target?.focus();
    const handle = target?.dataset.handle;
    stub({ "/tok/api/module/tree/Page": [200, pageAt(2)] });
    gen.seen(1);
    gen.seen(2);
    await vi.waitFor(() => expect(location.hash).not.toBe(""));
    expect(location.pathname).toBe("/tok/docs/tree/Page");
    // The new page lands on it, and takes focus (focus was in main).
    await vi.waitFor(() => {
      const f = document.activeElement as HTMLElement | null;
      expect(f?.matches("section.decl")).toBe(true);
      expect(f?.dataset.handle).toBe(handle);
      expect(f).not.toBe(target);
    });
  });

  it("focus outside main stays there", async () => {
    const { gen } = mountWith("/tok/docs/tree/Page");
    await vi.waitFor(() => expect(decl()).not.toBeNull());
    gen.seen(1);
    button()?.focus();
    stub({ "/tok/api/module/tree/Page": [200, pageAt(2)] });
    gen.seen(2);
    await vi.waitFor(() => expect(status()).toBe(reloadTexts.done));
    await vi.waitFor(() => expect(decl()).not.toBeNull());
    await new Promise((r) => requestAnimationFrame(r));
    expect(document.activeElement).toBe(button());
  });

  it("an older answer: fetched again, with no notice", async () => {
    const gen = createRoot(() => createGenerationState());
    // The worker said 2 before the page's first answer, of 1.
    gen.seen(2);
    mountWith("/tok/docs/tree/Page", undefined, gen);
    await vi.waitFor(() =>
      expect(gen.applied()).toEqual({ n: 1, notice: false }),
    );
    await vi.waitFor(() => expect(calls("/tok/api/module/tree/Page")).toBe(2));
    expect(status()).toBe("");
  });

  it("a module gone: Home, with a notice", async () => {
    const { gen } = mountWith("/tok/docs/tree/Page");
    await vi.waitFor(() => expect(decl()).not.toBeNull());
    stub({ "/tok/api/module/tree/Page": [404, "no"] });
    gen.seen(2);
    await vi.waitFor(() => expect(location.pathname).toMatch(/^\/tok\/?$/));
    await vi.waitFor(() =>
      expect(document.querySelector(".reload-gone")?.textContent).toBe(
        "Page is not in the files any more.",
      ),
    );
    // Said too: focus may be on the button, far from the notice.
    await vi.waitFor(() =>
      expect(status()).toBe("Page is not in the files any more."),
    );
    // The next reload: Home again, with no notice.
    gen.seen(3);
    await vi.waitFor(() => expect(status()).toBe(reloadTexts.done));
    await flush();
    expect(document.querySelector(".reload-gone")).toBeNull();
  });

  it("no baseline (the listing failed): the POST answer applies", async () => {
    stub({
      "/tok/api/files/tree": [500, "cannot list files"],
      "/tok/api/modules/tree": [500, "cannot list files"],
    });
    const { gen } = mountWith("/tok/");
    await vi.waitFor(() => expect(calls("/tok/api/modules/tree")).toBe(1));
    expect(gen.current()).toBeUndefined();
    stub({
      "/tok/api/files/tree": [200, fixture("files")],
      "/tok/api/modules/tree": [200, fixture("modules")],
    });
    button()?.click();
    posts[0]?.resolve(json(200, { generation: 2 }));
    await vi.waitFor(() => expect(status()).toBe(reloadTexts.done));
    await vi.waitFor(() => {
      expect(calls("/tok/api/modules/tree")).toBe(1);
      expect(calls("/tok/api/files/tree")).toBe(1);
    });
  });

  it("refs: a 404 after a reload goes to the docs page", async () => {
    const refs = "/tok/api/refs/tree?module=R.Shape&decl=val%3Anope";
    stub({ [refs]: [200, fixture("refs")] });
    const { gen } = mountWith("/tok/refs/tree/R.Shape?decl=val%3Anope");
    await vi.waitFor(() =>
      expect(document.querySelector(".refs-group")).not.toBeNull(),
    );
    stub({
      [refs]: [404, fixture("refs-not-found")],
      "/tok/api/module/tree/R.Shape": [200, pageAt(2)],
    });
    gen.seen(2);
    await vi.waitFor(() =>
      expect(location.pathname + location.hash).toBe(
        "/tok/docs/tree/R.Shape#val%3Anope",
      ),
    );
    await vi.waitFor(() =>
      expect(document.querySelector("main .decl-missing")).not.toBeNull(),
    );
  });

  it("sidebar auto: no flip to Modules while a reload refetches", async () => {
    const none = {
      generation: 1,
      notices: [],
      packages: [],
      broken: [],
      other: [],
      warnings: [],
    };
    stub({ "/tok/api/modules/tree": [200, none] });
    const { gen } = mountWith("/tok/");
    const files = () => document.querySelector<HTMLElement>(".files-panel");
    await vi.waitFor(() => expect(files()?.hidden).toBe(false));
    gen.seen(2);
    expect(files()?.hidden).toBe(false);
    await vi.waitFor(() => expect(status()).toBe(reloadTexts.done));
    await flush();
    expect(files()?.hidden).toBe(false);
    expect(calls("/tok/api/modules/tree")).toBeGreaterThan(1);
  });

  it("without a reload, not found stays a not-found page", async () => {
    stub({ "/tok/api/module/tree/Page": [404, "no"] });
    mountWith("/tok/docs/tree/Page");
    await vi.waitFor(() =>
      expect(document.querySelector("main h1")?.textContent).toBe(
        "Not found in this snapshot",
      ),
    );
    expect(location.pathname).toBe("/tok/docs/tree/Page");
  });

  it("a source page lands at the cursor's line", async () => {
    stub({
      "/tok/api/file/tree/a.hs": [200, cborFixture("file-text")],
    });
    const { gen } = mountWith("/tok/src/tree/a.hs#L2");
    await vi.waitFor(() =>
      expect(document.querySelector(".code-view")).not.toBeNull(),
    );
    gen.seen(1);
    gen.seen(2);
    await vi.waitFor(() => expect(status()).toBe(reloadTexts.done));
    expect(location.pathname + location.hash).toBe("/tok/src/tree/a.hs#L2");
    expect(calls("/tok/api/file/tree/a.hs")).toBe(2);
  });
});

// The code line kept (D-356): by declaration in the source view.
describe("reload: the same code line (D-356)", () => {
  // tail: more lines at the end, for a page that scrolls.
  const lines = (above: number, tail = 0) =>
    [
      "module A where",
      ...Array<string>(above).fill("-- new"),
      "",
      "f :: Int",
      "f =",
      "  1",
      "",
      "g = 2",
      ...Array<string>(tail).fill("-- end"),
    ].join("\n") + "\n";
  const at = (
    generation: number,
    above: number,
    tail = 0,
  ): Record<string, [number, unknown]> => ({
    "/tok/api/file/tree/a.hs": [
      200,
      cbor({
        generation,
        kind: "text",
        text: lines(above, tail),
        filtered: false,
      }),
    ],
    "/tok/api/source/tree/a.hs": [
      200,
      cbor({
        generation,
        path: "a.hs",
        ref: "A",
        tier: { kind: "syntactic", reason: "no-hie" },
        partial: false,
        filtered: false,
        highlight: "lexed",
        runs: [],
        regions: [],
        targets: [],
        outline: [
          ["value", "val:f", "f", 3 + above, 3 + above, 5 + above],
          ["value", "val:g", "g", 7 + above, 7 + above, 7 + above],
        ],
      }),
    ],
  });
  const cursorCell = () =>
    document.querySelector<HTMLElement>('.code-cell[tabindex="0"]');
  const cursorLine = () =>
    cursorCell()?.closest<HTMLElement>(".code-row")?.dataset.line;
  const box = () => document.querySelector<HTMLElement>(".code-view");
  const outlineIn = () =>
    vi.waitFor(() =>
      expect(document.querySelector(".code-outline")).not.toBeNull(),
    );

  // The page at generation 1, with its outline.
  async function mountAt(url: string, tail = 0) {
    stub(at(1, 0, tail));
    const { gen } = mountWith(url);
    await outlineIn();
    return gen;
  }
  // Generation 2: above new lines at the top.
  async function reload(gen: GenerationState, above: number, tail = 0) {
    stub(at(2, above, tail));
    gen.seen(1);
    gen.seen(2);
    await vi.waitFor(() => expect(calls("/tok/api/source/tree/a.hs")).toBe(1));
  }
  // The same, but api/source waits for answer().
  async function reloadLate(gen: GenerationState, above: number, tail = 0) {
    stub(at(2, above, tail));
    let answer: (() => void) | undefined;
    vi.stubGlobal("fetch", (input: RequestInfo | URL) => {
      if (input === "/tok/api/source/tree/a.hs") {
        return new Promise<Response>((resolve) => {
          answer = () => resolve(gets(input));
        });
      }
      return gets(input);
    });
    gen.seen(1);
    gen.seen(2);
    await vi.waitFor(() => expect(answer).toBeDefined());
    return async () => {
      answer?.();
      await outlineIn();
      await flush();
    };
  }

  it("lines put in above: the cursor's line moves with its declaration", async () => {
    await reload(await mountAt("/tok/src/tree/a.hs#L4"), 3);
    await vi.waitFor(() => expect(location.hash).toBe("#L7"));
    expect(cursorLine()).toBe("7");
    // Fixing the line keeps the words.
    expect(status()).toBe(reloadTexts.done);
  });

  it("a range: both ends kept", async () => {
    await reload(await mountAt("/tok/src/tree/a.hs#L3-L5"), 2);
    await vi.waitFor(() => expect(location.hash).toBe("#L5-L7"));
    expect(cursorLine()).toBe("5");
  });

  it("no hash: none after the reload either (T17a review N3)", async () => {
    await reload(await mountAt("/tok/src/tree/a.hs"), 2);
    await outlineIn();
    await flush();
    expect(location.hash).toBe("");
    expect(cursorLine()).toBe("1");
  });

  it("no hash, scrolled: the same top line, by declaration", async () => {
    const gen = await mountAt("/tok/src/tree/a.hs", 200);
    const b = box()!;
    // Line 41 at the top (rows of fallbackRowHeight px).
    b.scrollTop = 40 * fallbackRowHeight;
    b.dispatchEvent(new Event("scroll"));
    await flush();
    await reload(gen, 3, 200);
    await outlineIn();
    await vi.waitFor(() =>
      expect(box()?.scrollTop).toBe(43 * fallbackRowHeight),
    );
    expect(location.hash).toBe("");
  });

  it("the cursor moved before the outline came: it stays", async () => {
    const answer = await reloadLate(await mountAt("/tok/src/tree/a.hs#L4"), 3);
    expect(location.hash).toBe("#L4");
    // Down one line, by key.
    cursorCell()?.dispatchEvent(
      new KeyboardEvent("keydown", { key: "ArrowDown", bubbles: true }),
    );
    await vi.waitFor(() => expect(cursorLine()).toBe("5"));
    await answer();
    expect(location.hash).toBe("#L4");
    expect(cursorLine()).toBe("5");
  });

  it("the outline late, focus gone out of the view: the line moves, focus stays out", async () => {
    const gen = await mountAt("/tok/src/tree/a.hs#L4");
    cursorCell()?.focus();
    const answer = await reloadLate(gen, 3);
    await vi.waitFor(() =>
      expect(box()?.contains(document.activeElement)).toBe(true),
    );
    const h1 = document.querySelector<HTMLElement>("main h1")!;
    h1.focus();
    await answer();
    expect(location.hash).toBe("#L7");
    expect(cursorLine()).toBe("7");
    expect(document.activeElement).toBe(h1);
  });

  it("the outline late, focus still in the view: it goes with the line", async () => {
    const gen = await mountAt("/tok/src/tree/a.hs#L4");
    cursorCell()?.focus();
    const answer = await reloadLate(gen, 3);
    await vi.waitFor(() =>
      expect(box()?.contains(document.activeElement)).toBe(true),
    );
    await answer();
    expect(location.hash).toBe("#L7");
    await vi.waitFor(() => expect(document.activeElement).toBe(cursorCell()));
    expect(cursorLine()).toBe("7");
  });

  it("the outline late, the view scrolled meanwhile: no scroll back", async () => {
    const gen = await mountAt("/tok/src/tree/a.hs#L4", 200);
    cursorCell()?.focus();
    const answer = await reloadLate(gen, 3, 200);
    const b = box()!;
    b.scrollTop = 100 * fallbackRowHeight;
    b.dispatchEvent(new Event("scroll"));
    await flush();
    await answer();
    await vi.waitFor(() => expect(location.hash).toBe("#L7"));
    await flush();
    expect(box()?.scrollTop).toBe(100 * fallbackRowHeight);
    expect(cursorLine()).toBe("7");
  });

  it("another reload before the outline came: still by declaration", async () => {
    const gen = await mountAt("/tok/src/tree/a.hs#L4");
    await reloadLate(gen, 3);
    expect(location.hash).toBe("#L4");
    // A third generation (r again, or another tab), the same text;
    // api/source answers now.
    stub(at(3, 3));
    gen.seen(3);
    await vi.waitFor(() => expect(location.hash).toBe("#L7"));
    expect(cursorLine()).toBe("7");
  });

  it("the declaration gone: the same line, clamped to the file", async () => {
    const gen = await mountAt("/tok/src/tree/a.hs#L7");
    stub({
      "/tok/api/file/tree/a.hs": [
        200,
        cbor({ generation: 2, kind: "text", text: "x\ny\n", filtered: false }),
      ],
      "/tok/api/source/tree/a.hs": [404, "not found"],
    });
    gen.seen(1);
    gen.seen(2);
    await vi.waitFor(() => expect(location.hash).toBe("#L2"));
    expect(cursorLine()).toBe("2");
  });
});

// The diff view: base lines stay, head lines go through base (D-356).
describe("reload: the same diff line (D-356)", () => {
  const same = Array.from({ length: 10 }, (_, i) => `e${i + 1}`);
  const after = Array.from({ length: 10 }, (_, i) => `e${i + 12}`);
  const baseText = [...same, "old", ...after].join("\n") + "\n";
  const headText = (above: number) =>
    [...Array<string>(above).fill("new top"), ...same, "new", ...after].join(
      "\n",
    ) + "\n";
  const diffAt = (generation: number, above: number) =>
    cbor({
      generation,
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
          ...(above > 0 ? [["insert", above]] : []),
          ["equal", 10],
          ["delete", 1],
          ["insert", 1],
          ["equal", 10],
        ],
        crlfLines: 0,
        bom: null,
      },
    });
  const file = (text: string, generation: number) =>
    cbor({ generation, filtered: false, kind: "text", text });
  const at = (
    generation: number,
    above: number,
  ): Record<string, [number, unknown]> => ({
    "/tok/api/diff/src/A.hs": [200, diffAt(generation, above)],
    "/tok/api/file/base/src/A.hs": [200, file(baseText, 1)],
    "/tok/api/file/head/src/A.hs": [200, file(headText(above), generation)],
  });

  async function reloadFrom(url: string) {
    stub(at(1, 0));
    const { gen } = mountWith(url, diffSession);
    await vi.waitFor(() =>
      expect(document.querySelector('[role="grid"]')).not.toBeNull(),
    );
    stub(at(2, 3));
    gen.seen(1);
    gen.seen(2);
    await vi.waitFor(() => expect(calls("/tok/api/diff/src/A.hs")).toBe(1));
  }

  it("a head line below new lines: the same code", async () => {
    await reloadFrom("/tok/diff/src/A.hs#head-L15");
    await vi.waitFor(() => expect(location.hash).toBe("#head-L18"));
    await vi.waitFor(() => expect(status()).toBe(reloadTexts.done));
  });

  it("a base line stays", async () => {
    await reloadFrom("/tok/diff/src/A.hs#base-L11");
    await vi.waitFor(() =>
      expect(document.querySelector('[role="grid"]')).not.toBeNull(),
    );
    await flush();
    expect(location.hash).toBe("#base-L11");
  });
});
