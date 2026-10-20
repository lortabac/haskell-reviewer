import { render } from "solid-js/web";
import { afterEach, describe, expect, it } from "vitest";
import {
  ConnectionContext,
  ConnectionNotice,
  noticeTexts,
} from "./ConnectionNotice";
import type { Status } from "./protocol";

let dispose: (() => void) | undefined;

function mount(status: Status, fallback: boolean): HTMLElement {
  const root = document.createElement("div");
  document.body.append(root);
  dispose = render(
    () => (
      <ConnectionContext.Provider
        value={{ status: () => status, fallback: () => fallback }}
      >
        <ConnectionNotice />
      </ConnectionContext.Provider>
    ),
    root,
  );
  return root;
}

function region(root: HTMLElement): Element {
  const r = root.querySelector('[role="status"]');
  if (r === null) throw new Error("no status region");
  return r;
}

afterEach(() => {
  dispose?.();
  document.body.replaceChildren();
});

describe("ConnectionNotice", () => {
  it.each<Status>(["connecting", "open"])("%s: empty region", (status) => {
    expect(region(mount(status, false)).textContent).toBe("");
  });

  it("reconnecting", () => {
    expect(region(mount("reconnecting", false)).textContent).toBe(
      "Connection lost. Reconnecting…",
    );
  });

  it("ended", () => {
    expect(region(mount("ended", false)).textContent).toBe(
      "This session has ended. Run haskell-reviewer again.",
    );
  });

  it("fallback", () => {
    expect(region(mount("open", true)).textContent).toBe(noticeTexts.fallback);
    expect(noticeTexts.fallback).toBe(
      "This browser can't share one connection between tabs, so each tab keeps its own. With more than about five tabs open, pages may stop loading.",
    );
  });

  it("no provider: nothing", () => {
    const root = document.createElement("div");
    document.body.append(root);
    dispose = render(() => <ConnectionNotice />, root);
    expect(region(root).textContent).toBe("");
  });
});
