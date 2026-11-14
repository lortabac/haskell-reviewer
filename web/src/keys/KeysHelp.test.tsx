import { afterEach, describe, expect, it, vi } from "vitest";
import { cborFixture, fixture, stubFetch } from "../../test/fixtures";
import { mountAppAt, unmountApp } from "../../test/mount";
import { defaultPrefs } from "../api/prefs";
import { keymap } from "./keymap";

afterEach(() => {
  unmountApp();
  vi.restoreAllMocks();
});

const codes: Record<string, string> = {
  "?": "Slash",
  "/": "Slash",
  "1": "Digit1",
};

function press(key: string, init: KeyboardEventInit = {}): void {
  const target = document.activeElement ?? document.body;
  target.dispatchEvent(
    new KeyboardEvent("keydown", {
      key,
      code: codes[key] ?? `Key${key.toUpperCase()}`,
      bubbles: true,
      cancelable: true,
      ...init,
    }),
  );
}

const dialog = () => document.querySelector<HTMLDialogElement>("dialog");
const toggle = () =>
  document.querySelector<HTMLInputElement>("dialog input[type=checkbox]");
const button = () =>
  document.querySelector<HTMLButtonElement>(".shortcuts-button");

function mount(path = "/tok") {
  const fetch = stubFetch({
    "/tok/api/modules/tree": [200, fixture("modules")],
    "/tok/api/files/tree": [200, fixture("files")],
    "/tok/api/file/tree/pkg/app/Main.hs": [200, cborFixture("file-text")],
  });
  mountAppAt(path, defaultPrefs);
  return fetch;
}

describe("the ? overlay", () => {
  it("? opens it, focus on the switch; the table is the keymap", () => {
    mount();
    press("?", { shiftKey: true });
    expect(dialog()?.open).toBe(true);
    expect(document.activeElement).toBe(toggle());
    expect(toggle()?.checked).toBe(true);
    expect(dialog()?.querySelector("h2")?.textContent).toBe(
      "Keyboard shortcuts",
    );
    const rows = dialog()?.querySelectorAll("tbody tr") ?? [];
    expect(rows.length).toBe(keymap.length);
    expect(dialog()?.textContent).toContain("Go to definition");
  });

  it("the header button opens it; Close puts focus back", () => {
    mount();
    button()?.focus();
    button()?.click();
    expect(dialog()?.open).toBe(true);
    const close = [...(dialog()?.querySelectorAll("button") ?? [])].find(
      (b) => b.textContent === "Close",
    );
    close?.click();
    expect(dialog()?.open).toBe(false);
    expect(document.activeElement).toBe(button());
  });

  it("opened by a click that didn't focus the button (WebKit): focus goes to it", () => {
    mount();
    (document.activeElement as HTMLElement | null)?.blur();
    button()?.click();
    expect(dialog()?.open).toBe(true);
    dialog()?.close();
    expect(document.activeElement).toBe(button());
  });

  it("opened by ? with nothing focused: focus goes to main", () => {
    mount();
    (document.activeElement as HTMLElement | null)?.blur();
    press("?", { shiftKey: true });
    dialog()?.close();
    expect(document.activeElement?.id).toBe("main");
  });

  it("Tab wraps inside", () => {
    mount();
    button()?.click();
    const close = [...(dialog()?.querySelectorAll("button") ?? [])].at(-1);
    close?.focus();
    press("Tab");
    expect(document.activeElement).toBe(toggle());
    press("Tab", { shiftKey: true });
    expect(document.activeElement).toBe(close);
  });

  it("the switch saves the pref and turns the keys off", () => {
    const fetch = mount();
    button()?.click();
    toggle()?.click();
    const calls = fetch.mock.calls as unknown as [string, RequestInit][];
    const post = calls.find(([url]) => url === "/tok/api/prefs");
    expect(JSON.parse(post?.[1].body as string)).toEqual({
      singleKeyShortcuts: false,
    });
    expect(dialog()?.textContent).toContain("Off:");
    dialog()?.close();
    // Off: / no longer goes to the search box.
    document.body.focus();
    press("/");
    expect(document.activeElement?.closest(".search-box")).toBeNull();
  });

  it("/ focuses the header search box", () => {
    mount();
    press("/");
    expect(
      document.activeElement?.closest(".search-box-header"),
    ).not.toBeNull();
  });
});

describe("the view switch", () => {
  it("none on the home page", () => {
    mount();
    expect(document.querySelector("nav.view-switch")).toBeNull();
  });

  it("a source page: Source is current, Docs links to its module", async () => {
    mount("/tok/src/tree/pkg/app/Main.hs");
    await vi.waitFor(() => {
      const docs = document.querySelector<HTMLAnchorElement>(
        "nav.view-switch a:not([aria-current])",
      );
      expect(docs?.textContent).toBe("Docs");
      expect(docs?.getAttribute("href")).toBe(
        "/tok/docs/tree/pkg%3Aexe%3Aapp%3AMain",
      );
    });
    const cur = document.querySelector('nav.view-switch [aria-current="page"]');
    expect(cur?.textContent).toBe("Source");
    expect(
      document.querySelector("nav.view-switch")?.getAttribute("aria-label"),
    ).toBe("View");
  });

  it("the current view's link: a click does nothing", async () => {
    mount("/tok/src/tree/pkg/app/Main.hs#L2");
    const cur = await vi.waitFor(() => {
      const el = document.querySelector<HTMLAnchorElement>(
        'nav.view-switch [aria-current="page"]',
      );
      if (el === null) throw new Error("no link");
      return el;
    });
    const e = new MouseEvent("click", {
      bubbles: true,
      cancelable: true,
      button: 0,
    });
    cur.dispatchEvent(e);
    expect(e.defaultPrevented).toBe(true);
    expect(window.location.pathname + window.location.hash).toBe(
      "/tok/src/tree/pkg/app/Main.hs#L2",
    );
  });

  it("the status: said, then cleared on a new page", async () => {
    stubFetch({
      "/tok/api/modules/tree": [200, fixture("modules")],
      "/tok/api/files/tree": [200, fixture("files")],
      "/tok/api/file/tree/README.md": [200, cborFixture("file-text")],
    });
    mountAppAt("/tok/src/tree/README.md", defaultPrefs);
    const status = () => document.querySelector(".keys-status")?.textContent;
    await vi.waitFor(() => {
      expect(document.querySelector(".view-missing")).not.toBeNull();
    });
    press("1");
    expect(status()).toBe("No docs for this page.");
    const a = await vi.waitFor(() => {
      const el = document.querySelector<HTMLAnchorElement>(
        "nav.frame-sidebar a[href]",
      );
      if (el === null) throw new Error("no sidebar link");
      return el;
    });
    a.click();
    await vi.waitFor(() => {
      expect(status()).toBe("");
    });
  });

  it("a file with no module: Docs is muted, said not available", async () => {
    stubFetch({
      "/tok/api/modules/tree": [200, fixture("modules")],
      "/tok/api/files/tree": [200, fixture("files")],
      "/tok/api/file/tree/README.md": [200, cborFixture("file-text")],
    });
    mountAppAt("/tok/src/tree/README.md", defaultPrefs);
    await vi.waitFor(() => {
      expect(document.querySelector(".view-missing")?.textContent).toBe(
        "Docs (not available)",
      );
    });
  });
});
