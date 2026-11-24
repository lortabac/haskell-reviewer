// View on base / head and b (D-349).
import { Route, Router, useLocation } from "@solidjs/router";
import { render } from "solid-js/web";
import { afterEach, beforeEach, describe, expect, it, vi } from "vitest";
import { cbor, fixture, stubFetch } from "../../test/fixtures";
import { fakeLayout } from "../../test/layout";
import { mountAppAt, unmountApp } from "../../test/mount";
import { diffSession } from "../../test/sessions";
import { defaultPrefs } from "../api/prefs";
import { PrefsProvider } from "../prefs/store";
import { SessionContext } from "../session";
import { KeysProvider, KeysStatus, useSideTarget } from "./KeysProvider";

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

// Head: 6 lines, 3-4 replace base's 3. Base: 5 lines.
const diffBody = cbor({
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
      ["equal", 2],
      ["delete", 1],
      ["insert", 2],
      ["equal", 2],
    ],
    crlfLines: 0,
    bom: null,
  },
});

function open(url: string, browse = false) {
  stubFetch({
    "/tok/api/diff-files": [
      200,
      {
        generation: 1,
        base: { label: "main", commit: "a".repeat(40) },
        head: { label: "feature", commit: "b".repeat(40) },
        files: [change("src/A.hs", "modified"), change("src/New.hs", "added")],
        outside: null,
        renameLimitHit: false,
        truncated: false,
      },
    ],
    "/tok/api/prefs": [200, defaultPrefs],
    "/tok/api/file/head/src/A.hs": [200, file(6)],
    "/tok/api/file/base/src/A.hs": [200, file(5)],
    "/tok/api/file/head/src/New.hs": [200, file(2)],
    "/tok/api/file/tree/src/A.hs": [200, file(6)],
    "/tok/api/diff/src/A.hs": [200, diffBody],
  });
  mountAppAt(url, undefined, undefined, null, browse ? undefined : diffSession);
}

const item = () => document.querySelector<HTMLElement>(".side-switch");
const status = () => document.querySelector(".keys-status")?.textContent;
const here = () => location.pathname + location.hash;
const press = (key: string) =>
  (document.activeElement ?? document.body).dispatchEvent(
    new KeyboardEvent("keydown", {
      key,
      code: `Key${key.toUpperCase()}`,
      bubbles: true,
      cancelable: true,
    }),
  );

beforeEach(() => fakeLayout());

afterEach(() => {
  unmountApp();
  vi.unstubAllGlobals();
  vi.restoreAllMocks();
});

describe("SideSwitch", () => {
  it("a link to the other side's top; a click maps the line", async () => {
    open("/tok/src/head/src/A.hs#L4");
    await vi.waitFor(() => expect(item()?.tagName).toBe("A"));
    const a = item()!;
    expect(a.textContent).toBe("View on base");
    expect(a.getAttribute("href")).toBe("/tok/src/base/src/A.hs");
    // Not in a nav (the link crawl in e2e/sides.spec.ts).
    expect(a.closest("nav")).toBeNull();
    await vi.waitFor(() =>
      expect(document.querySelector(".code-num")).not.toBeNull(),
    );
    a.click();
    await vi.waitFor(() => expect(here()).toBe("/tok/src/base/src/A.hs#L3"));
    await vi.waitFor(() =>
      expect(status()).toBe("Line 4 changed: base line 3 shown."),
    );
  });

  it("only on head: aria-disabled, focusable, the reason said", async () => {
    open("/tok/src/head/src/New.hs");
    // "Loading changes…" first.
    await vi.waitFor(() =>
      expect(item()?.getAttribute("data-tip")).toBe("Only on head."),
    );
    const b = item()!;
    expect(b.tagName).toBe("BUTTON");
    expect(b.getAttribute("aria-disabled")).toBe("true");
    expect(b.hasAttribute("disabled")).toBe(false);
    expect(b.getAttribute("data-tip")).toBe("Only on head.");
    const desc = document.getElementById(b.getAttribute("aria-describedby")!);
    expect(desc?.textContent).toBe("Only on head.");
    // The name is the label only.
    expect(b.textContent).toBe("View on base");
    b.focus();
    expect(document.activeElement).toBe(b);
    b.click();
    expect(status()).toBe("Only on head.");
    expect(here()).toBe("/tok/src/head/src/New.hs");
    document.body.focus();
    press("b");
    expect(status()).toBe("Only on head. ");
  });

  it("browse mode: none; b says why", async () => {
    open("/tok/src/tree/src/A.hs", true);
    await vi.waitFor(() =>
      expect(document.querySelector(".code-view")).not.toBeNull(),
    );
    expect(item()).toBeNull();
    press("b");
    expect(status()).toBe("Not in diff mode.");
  });

  it("Home: none; b says there is no other side", async () => {
    open("/tok/");
    await new Promise((r) => setTimeout(r, 20));
    expect(item()).toBeNull();
    press("b");
    expect(status()).toBe("No other side for this page.");
  });
});

// The other side targets of Markdown, refs and text search pages.
describe("SideSwitch on other pages", () => {
  const diffFiles = {
    generation: 1,
    base: { label: "main", commit: "a".repeat(40) },
    head: { label: "feature", commit: "b".repeat(40) },
    files: [],
    outside: null,
    renameLimitHit: false,
    truncated: false,
  };
  function openPage(url: string) {
    stubFetch({
      "/tok/api/diff-files": [200, diffFiles],
      "/tok/api/prefs": [200, defaultPrefs],
      "/tok/api/modules/head": [200, fixture("modules")],
      "/tok/api/modules/base": [200, fixture("modules")],
      "/tok/api/refs/head?module=R.Shape&decl=val%3Aarea": [
        200,
        fixture("refs"),
      ],
    });
    mountAppAt(url, undefined, undefined, null, diffSession);
  }

  it("Markdown: the hash is kept", async () => {
    openPage("/tok/md/head/README.md#intro");
    // A link only once the list is in (while it loads: "Loading
    // changes…"), and README.md isn't changed.
    await vi.waitFor(() =>
      expect(item()?.getAttribute("href")).toBe("/tok/md/base/README.md"),
    );
    item()!.click();
    await vi.waitFor(() => expect(here()).toBe("/tok/md/base/README.md#intro"));
  });

  it("refs: the declaration is kept", async () => {
    openPage("/tok/refs/head/R.Shape?decl=val%3Aarea");
    await vi.waitFor(() =>
      expect(item()?.getAttribute("href")).toBe(
        "/tok/refs/base/R.Shape?decl=val%3Aarea",
      ),
    );
    expect(item()?.textContent).toBe("View on base");
  });

  it("text search: the same query on the other side", async () => {
    openPage("/tok/search/base?q=main");
    await vi.waitFor(() =>
      expect(item()?.getAttribute("href")).toBe("/tok/search/head?q=main"),
    );
    item()!.click();
    await vi.waitFor(() =>
      expect(location.pathname + location.search).toBe(
        "/tok/search/head?q=main",
      ),
    );
  });
});

describe("goSide", () => {
  let dispose: (() => void) | undefined;
  afterEach(() => {
    dispose?.();
    dispose = undefined;
  });

  it("the place, leave, then navigate, then the words", async () => {
    history.replaceState(null, "", "/a");
    const seen: string[] = [];
    const root = document.createElement("div");
    document.body.append(root);
    function Page() {
      const loc = useLocation();
      useSideTarget(() => ({
        kind: "go",
        side: "base",
        top: "/top",
        at: () => {
          seen.push("at");
          return { href: "/b", state: { codeJump: true }, words: "Words." };
        },
        leave: () => seen.push(`leave at ${loc.pathname}`),
      }));
      return <div tabindex="0" id="cell" />;
    }
    dispose = render(
      () => (
        <SessionContext.Provider value={diffSession}>
          <PrefsProvider initial={defaultPrefs}>
            <Router
              root={(p) => (
                <KeysProvider>
                  <KeysStatus />
                  {p.children}
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
    await new Promise((r) => setTimeout(r, 20));
    document.getElementById("cell")?.focus();
    press("b");
    expect(seen).toEqual(["at", "leave at /a"]);
    await vi.waitFor(() => expect(location.pathname).toBe("/b"));
    expect(history.state).toMatchObject({ codeJump: true });
    await vi.waitFor(() => expect(status()).toBe("Words."));
  });
});
