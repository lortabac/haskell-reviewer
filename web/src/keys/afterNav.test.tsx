import { Route, Router, useNavigate } from "@solidjs/router";
import { render } from "solid-js/web";
import { afterEach, describe, expect, it, vi } from "vitest";
import { defaultPrefs } from "../api/prefs";
import { PrefsProvider } from "../prefs/store";
import {
  type KeyHandler,
  KeysProvider,
  KeysStatus,
  useKeyActions,
} from "./KeysProvider";

// KeyHandler's afterNav: said once the URL has changed, dropped after
// 1 s or by any other key.
let dispose: (() => void) | undefined;
let nav: ((to: string, o?: object) => void) | undefined;

afterEach(() => {
  dispose?.();
  dispose = undefined;
  document.body.replaceChildren();
});

// u runs `layout`, y runs `copy`.
function mount(layout: KeyHandler, copy: KeyHandler = () => false) {
  history.replaceState(null, "", "/a?x=1");
  const root = document.createElement("div");
  document.body.append(root);
  function Page() {
    nav = useNavigate();
    useKeyActions({ layout, copy });
    return <div tabindex="0" id="cell" />;
  }
  dispose = render(
    () => (
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
    ),
    root,
  );
}

const press = (key: string, code: string) =>
  document.getElementById("cell")?.dispatchEvent(
    new KeyboardEvent("keydown", {
      key,
      code,
      bubbles: true,
      cancelable: true,
    }),
  );
const said = () => document.querySelector(".keys-status")?.textContent;
const tick = (ms: number) => new Promise((r) => setTimeout(r, ms));
const dropParam = () => nav?.("/a", { replace: true });

describe("afterNav", () => {
  it("is said once, after the URL has changed", async () => {
    mount(() => {
      dropParam();
      return { afterNav: "Words." };
    });
    await tick(20);
    press("u", "KeyU");
    expect(said()).toBe("");
    await vi.waitFor(() => expect(location.search).toBe(""));
    await vi.waitFor(() => expect(said()).toBe("Words."));
  });

  it("is dropped after 1 s with no navigation", async () => {
    mount(() => ({ afterNav: "Words." }));
    await tick(20);
    press("u", "KeyU");
    await tick(1100);
    nav?.("/b");
    await tick(30);
    expect(location.pathname).toBe("/b");
    expect(said()).toBe("");
  });

  it("is cancelled by a key that says something", async () => {
    mount(
      () => ({ afterNav: "Words." }),
      () => "Other.",
    );
    await tick(20);
    press("u", "KeyU");
    press("y", "KeyY");
    expect(said()).toBe("Other.");
    nav?.("/b");
    await tick(30);
    expect(said()).toBe("");
  });

  it("the same words twice are said again", async () => {
    let n = 0;
    mount(() => {
      n++;
      if (n === 1) return "Words.";
      nav?.(n === 2 ? "/a" : "/a?x=1", { replace: true });
      return { afterNav: "Words." };
    });
    await tick(20);
    press("u", "KeyU");
    expect(said()).toBe("Words.");
    press("u", "KeyU");
    await vi.waitFor(() => expect(location.search).toBe(""));
    await vi.waitFor(() => expect(said()).toBe("Words. "));
  });

  it("an unmount with the timer pending is clean", async () => {
    mount(() => ({ afterNav: "Words." }));
    await tick(20);
    press("u", "KeyU");
    dispose?.();
    dispose = undefined;
    await tick(1100);
  });
});
