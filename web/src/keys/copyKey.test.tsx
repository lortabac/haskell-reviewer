import { afterEach, describe, expect, it, vi } from "vitest";
import { cborFixture, fixture, stubFetch } from "../../test/fixtures";
import { mountAppAt, unmountApp } from "../../test/mount";
import { defaultPrefs } from "../api/prefs";

afterEach(() => {
  unmountApp();
  vi.restoreAllMocks();
  vi.unstubAllGlobals();
});

function press(key: string): void {
  (document.activeElement ?? document.body).dispatchEvent(
    new KeyboardEvent("keydown", {
      key,
      code: `Key${key.toUpperCase()}`,
      bubbles: true,
      cancelable: true,
    }),
  );
}

const status = () => document.querySelector(".keys-status")?.textContent;

function mount(prefs = defaultPrefs) {
  stubFetch({
    "/tok/api/modules/tree": [200, fixture("modules")],
    "/tok/api/files/tree": [200, fixture("files")],
    "/tok/api/file/tree/pkg/app/Main.hs": [200, cborFixture("file-text")],
  });
  mountAppAt("/tok", prefs);
}

describe("y", () => {
  it("nothing to copy on the home page", () => {
    vi.stubGlobal("navigator", { clipboard: { writeText: vi.fn() } });
    mount();
    press("y");
    expect(status()).toBe("Nothing to copy here.");
  });

  it("a focused [data-copy] element is copied, and Copied. is said", async () => {
    const writeText = vi.fn(() => Promise.resolve());
    vi.stubGlobal("navigator", { clipboard: { writeText } });
    mount();
    const a = document.createElement("a");
    a.href = "#x";
    a.dataset.copy = "src/Foo.hs";
    document.body.append(a);
    a.focus();
    press("y");
    expect(writeText).toHaveBeenCalledWith("src/Foo.hs");
    await vi.waitFor(() => expect(status()).toBe("Copied."));
    expect(
      document.querySelector(".keys-status")?.getAttribute("aria-live"),
    ).toBe("polite");
  });

  it("off with the shortcuts off", () => {
    const writeText = vi.fn(() => Promise.resolve());
    vi.stubGlobal("navigator", { clipboard: { writeText } });
    mount({ ...defaultPrefs, singleKeyShortcuts: false });
    press("y");
    expect(status()).toBe("");
    expect(writeText).not.toHaveBeenCalled();
  });
});
