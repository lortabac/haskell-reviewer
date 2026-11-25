// ] and [, and the Previous / Next file buttons (D-350).
import { Route, Router, useLocation, useNavigate } from "@solidjs/router";
import { createSignal, For, Show } from "solid-js";
import { render } from "solid-js/web";
import { afterEach, beforeEach, describe, expect, it, vi } from "vitest";
import { cbor, stubFetch } from "../../test/fixtures";
import { fakeLayout } from "../../test/layout";
import { mountAppAt, unmountApp } from "../../test/mount";
import { diffSession } from "../../test/sessions";
import { defaultPrefs } from "../api/prefs";
import {
  KeysProvider,
  KeysStatus,
  useFileSteps,
  useViewLinks,
} from "../keys/KeysProvider";
import { PrefsProvider } from "../prefs/store";
import { SessionContext } from "../session";
import { FileStepButtons } from "./FileStepButtons";

const file = (n: number) =>
  cbor({
    generation: 1,
    filtered: false,
    kind: "text",
    text: Array.from({ length: n }, (_, i) => `l${i + 1}\n`).join(""),
  });

const change = (path: string, status: string) => ({
  path,
  oldPath: null,
  status,
  similarity: null,
  note: null,
});

const diffBody = cbor({
  generation: 1,
  path: "src/B.hs",
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
      ["equal", 1],
      ["delete", 1],
      ["insert", 1],
    ],
    crlfLines: 0,
    bom: null,
  },
});

// Head's own files: src/B.hs, z.txt; src/A.hs is only on base.
function open(url: string, browse = false) {
  stubFetch({
    "/tok/api/diff-files": [
      200,
      {
        generation: 1,
        base: { label: "main", commit: "a".repeat(40) },
        head: { label: "feature", commit: "b".repeat(40) },
        files: [
          change("src/A.hs", "removed"),
          change("src/B.hs", "modified"),
          change("z.txt", "added"),
        ],
        outside: null,
        renameLimitHit: false,
        truncated: false,
      },
    ],
    "/tok/api/prefs": [200, defaultPrefs],
    "/tok/api/file/head/src/B.hs": [200, file(2)],
    "/tok/api/file/base/src/B.hs": [200, file(2)],
    "/tok/api/file/head/z.txt": [200, file(2)],
    "/tok/api/file/tree/src/B.hs": [200, file(2)],
    "/tok/api/diff/src/B.hs": [200, diffBody],
    "/tok/api/diff/src/B.hs?ws=ignore": [200, diffBody],
  });
  mountAppAt(url, undefined, undefined, null, browse ? undefined : diffSession);
}

const settle = () => new Promise((r) => setTimeout(r, 20));
const status = () => document.querySelector(".keys-status")?.textContent;
const here = () => location.pathname + location.search;
const press = (key: string) =>
  document.body.dispatchEvent(
    new KeyboardEvent("keydown", {
      key,
      code: key === "]" ? "BracketRight" : "BracketLeft",
      bubbles: true,
      cancelable: true,
    }),
  );
const button = (name: string) =>
  [...document.querySelectorAll("button")].find((b) =>
    b.textContent?.startsWith(name),
  );

beforeEach(() => fakeLayout());

afterEach(() => {
  unmountApp();
  vi.unstubAllGlobals();
  vi.restoreAllMocks();
});

describe("] and [ in the app", () => {
  it("diff: the buttons; Next file keeps ws and layout", async () => {
    open("/tok/diff/src/B.hs?ws=ignore&layout=unified");
    await vi.waitFor(() =>
      expect(document.querySelector('[role="grid"]')).not.toBeNull(),
    );
    expect(button("Next file")?.textContent).toBe("Next file (])");
    expect(button("Previous file")?.textContent).toBe("Previous file ([)");
    // The list is in by now.
    await new Promise((r) => setTimeout(r, 20));
    button("Next file")?.click();
    await vi.waitFor(() =>
      expect(here()).toBe("/tok/diff/z.txt?ws=ignore&layout=unified"),
    );
    await vi.waitFor(() =>
      expect(status()).toBe("z.txt, changed file 3 of 3."),
    );
  });

  it("diff: removed files count, the end says so", async () => {
    open("/tok/diff/src/B.hs");
    await vi.waitFor(() => expect(button("Next file")).toBeDefined());
    await new Promise((r) => setTimeout(r, 20));
    press("[");
    await vi.waitFor(() => expect(here()).toBe("/tok/diff/src/A.hs"));
    await vi.waitFor(() =>
      expect(status()).toBe("src/A.hs, changed file 1 of 3."),
    );
    press("[");
    expect(status()).toBe("No more changed files.");
  });

  it("source on head: base's removed file is skipped", async () => {
    open("/tok/src/head/src/B.hs");
    await vi.waitFor(() =>
      expect(document.querySelector(".code-num")).not.toBeNull(),
    );
    await new Promise((r) => setTimeout(r, 20));
    expect(button("Next file")).toBeDefined();
    press("[");
    expect(status()).toBe("No more changed files.");
    press("]");
    await vi.waitFor(() => expect(here()).toBe("/tok/src/head/z.txt"));
    await vi.waitFor(() =>
      expect(status()).toBe("z.txt, changed file 2 of 2."),
    );
  });

  it("Home: says where it works", async () => {
    open("/tok/");
    await new Promise((r) => setTimeout(r, 20));
    press("]");
    expect(status()).toBe("Changed files: from the diff, source or docs view.");
  });

  it("browse mode: no buttons; ] says why", async () => {
    open("/tok/src/tree/src/B.hs", true);
    await vi.waitFor(() =>
      expect(document.querySelector(".code-num")).not.toBeNull(),
    );
    expect(button("Next file")).toBeUndefined();
    press("]");
    expect(status()).toBe("Not in diff mode.");
  });
});

describe("stepFile", () => {
  let dispose: (() => void) | undefined;
  afterEach(() => {
    dispose?.();
    dispose = undefined;
  });

  function mount(Page: () => ReturnType<typeof FileStepButtons>) {
    const root = document.createElement("div");
    document.body.append(root);
    dispose = render(
      () => (
        <SessionContext.Provider value={diffSession}>
          <PrefsProvider
            initial={{ ...defaultPrefs, singleKeyShortcuts: false }}
          >
            <Router
              root={(p) => (
                <KeysProvider>
                  <KeysStatus />
                  <main id="main">{p.children}</main>
                </KeysProvider>
              )}
            >
              <Route path="*" component={Page} />
            </Router>
          </PrefsProvider>
        </SessionContext.Provider>
      ),
      root,
    );
  }

  it("leave, then navigate, then the words", async () => {
    history.replaceState(null, "", "/a");
    const seen: string[] = [];
    mount(() => {
      const loc = useLocation();
      useViewLinks({
        current: "diff",
        docs: () => undefined,
        source: () => undefined,
        leave: () => seen.push(`leave at ${loc.pathname}`),
      });
      useFileSteps((dir) =>
        loc.pathname === "/a" && dir === 1
          ? { href: "/b", words: "Words." }
          : "No more changed files.",
      );
      return <FileStepButtons />;
    });
    await settle();
    // No key hints with shortcuts off.
    const next = button("Next file")!;
    expect(next.textContent).toBe("Next file");
    next.click();
    expect(seen).toEqual(["leave at /a"]);
    await vi.waitFor(() => expect(location.pathname).toBe("/b"));
    await vi.waitFor(() => expect(status()).toBe("Words."));
    // At the end, no move.
    button("Next file")!.click();
    expect(status()).toBe("No more changed files.");
    expect(location.pathname).toBe("/b");
  });

  // Pages /a -> /b -> /c. shown: where the buttons are; they mount
  // again on each page. ready: false holds them back (a slow page).
  const [ready, setReady] = createSignal(true);
  function pages(shown: (path: string) => boolean) {
    return () => {
      const loc = useLocation();
      const nav = useNavigate();
      useFileSteps((dir) => {
        const to = { "/a": "/b", "/b": "/c" }[loc.pathname];
        return dir === 1 && to !== undefined
          ? { href: to, words: "W." }
          : "No more changed files.";
      });
      return (
        <>
          <h1 tabindex="-1">Page</h1>
          <button type="button" id="go-c" onClick={() => nav("/c")}>
            Link
          </button>
          <Show when={ready() && shown(loc.pathname)}>
            {/* A new page, new buttons. */}
            <For each={[loc.pathname]}>{() => <FileStepButtons />}</For>
          </Show>
        </>
      );
    };
  }
  const active = () => document.activeElement;

  it("the button on the next page takes focus back", async () => {
    history.replaceState(null, "", "/a");
    setReady(true);
    mount(pages(() => true));
    await settle();
    button("Next file")!.focus();
    button("Next file")!.click();
    await vi.waitFor(() => expect(location.pathname).toBe("/b"));
    await vi.waitFor(() => expect(active()).toBe(button("Next file")));
    // The page's heading took it first: still the button.
    (document.querySelector("#main h1") as HTMLElement).focus();
    button("Next file")!.click();
    await vi.waitFor(() => expect(location.pathname).toBe("/c"));
    await vi.waitFor(() => expect(active()).toBe(button("Next file")));
  });

  it("a page with no buttons, then a link: no focus move", async () => {
    history.replaceState(null, "", "/a");
    setReady(true);
    mount(pages((p) => p !== "/b"));
    await settle();
    button("Next file")!.focus();
    button("Next file")!.click();
    await vi.waitFor(() => expect(location.pathname).toBe("/b"));
    expect(button("Next file")).toBeUndefined();
    document.body.focus();
    // A navigation by code, no click or key: only the page change
    // drops the press.
    (document.getElementById("go-c") as HTMLButtonElement).click();
    await vi.waitFor(() => expect(location.pathname).toBe("/c"));
    await vi.waitFor(() => expect(button("Next file")).toBeDefined());
    await settle();
    expect(active()).not.toBe(button("Next file"));
  });

  it("away and back before the buttons come: no focus move", async () => {
    history.replaceState(null, "", "/a");
    setReady(true);
    mount(pages(() => true));
    await settle();
    button("Next file")!.focus();
    button("Next file")!.click();
    setReady(false);
    await vi.waitFor(() => expect(location.pathname).toBe("/b"));
    (document.getElementById("go-c") as HTMLButtonElement).click();
    await vi.waitFor(() => expect(location.pathname).toBe("/c"));
    history.back();
    await vi.waitFor(() => expect(location.pathname).toBe("/b"));
    document.body.focus();
    setReady(true);
    await vi.waitFor(() => expect(button("Next file")).toBeDefined());
    await settle();
    expect(active()).not.toBe(button("Next file"));
  });

  it("a slow page, a click in the text meanwhile: no focus move", async () => {
    history.replaceState(null, "", "/a");
    setReady(true);
    mount(pages(() => true));
    await settle();
    button("Next file")!.focus();
    button("Next file")!.click();
    setReady(false);
    await vi.waitFor(() => expect(location.pathname).toBe("/b"));
    document.body.dispatchEvent(
      new PointerEvent("pointerdown", { bubbles: true }),
    );
    setReady(true);
    await vi.waitFor(() => expect(button("Next file")).toBeDefined());
    await settle();
    expect(active()).not.toBe(button("Next file"));
  });
});
