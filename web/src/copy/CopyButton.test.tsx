import { render } from "solid-js/web";
import { afterEach, describe, expect, it, vi } from "vitest";
import { CopyButton } from "./CopyButton";
import { KeysContext, type KeysApi } from "../keys/KeysProvider";
import { CopyLocation } from "./CopyLocation";

afterEach(() => {
  document.body.replaceChildren();
  vi.unstubAllGlobals();
});

describe("CopyButton", () => {
  it("has a name and no text; a press copies", async () => {
    const writeText = vi.fn(() => Promise.resolve());
    vi.stubGlobal("navigator", { clipboard: { writeText } });
    const root = document.createElement("div");
    document.body.append(root);
    render(
      () => <CopyButton text={() => "Data.Foo.bar"} label="Copy bar" />,
      root,
    );
    const b = root.querySelector("button");
    expect(b?.getAttribute("aria-label")).toBe("Copy bar");
    expect(b?.textContent).toBe("");
    b?.click();
    expect(writeText).toHaveBeenCalledWith("Data.Foo.bar");
    await vi.waitFor(() => expect(b?.dataset.state).toBe("copied"));
  });

  it("a normal button keeps its mousedown", () => {
    const root = document.createElement("div");
    document.body.append(root);
    render(() => <CopyButton text={() => "x"} label="Copy x" />, root);
    const e = new MouseEvent("mousedown", { bubbles: true, cancelable: true });
    root.querySelector("button")?.dispatchEvent(e);
    expect(e.defaultPrevented).toBe(false);
  });

  it("tabindex for the mouse-only ones", () => {
    const root = document.createElement("div");
    document.body.append(root);
    render(
      () => <CopyButton text={() => "x"} label="Copy x" mouseOnly />,
      root,
    );
    const b = root.querySelector("button");
    expect(b?.getAttribute("tabindex")).toBe("-1");
    expect(b?.getAttribute("aria-hidden")).toBe("true");
    // A press must not take focus.
    const e = new MouseEvent("mousedown", { bubbles: true, cancelable: true });
    b?.dispatchEvent(e);
    expect(e.defaultPrevented).toBe(true);
  });
});

describe("CopyLocation", () => {
  it("mousedown is prevented, so focus and selection stay", () => {
    const root = document.createElement("div");
    document.body.append(root);
    const keys = {
      location: () => () => "x",
      copyLocation: () => undefined,
    } as unknown as KeysApi;
    render(
      () => (
        <KeysContext.Provider value={keys}>
          <CopyLocation />
        </KeysContext.Provider>
      ),
      root,
    );
    const e = new MouseEvent("mousedown", { bubbles: true, cancelable: true });
    root.querySelector("button")?.dispatchEvent(e);
    expect(e.defaultPrevented).toBe(true);
  });

  it("is hidden with no location (no provider)", () => {
    const root = document.createElement("div");
    document.body.append(root);
    render(() => <CopyLocation />, root);
    expect(root.querySelector("button")).toBeNull();
  });
});
