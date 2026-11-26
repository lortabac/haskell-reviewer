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
