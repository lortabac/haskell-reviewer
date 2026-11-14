import { afterEach, describe, expect, it, vi } from "vitest";
import { copyText, failedWords } from "./clipboard";

afterEach(() => vi.unstubAllGlobals());

function stub(writeText: unknown) {
  vi.stubGlobal("navigator", { clipboard: { writeText } });
}

describe("copyText", () => {
  it("writes the text and says Copied and the text", async () => {
    const writeText = vi.fn(() => Promise.resolve());
    stub(writeText);
    const say = vi.fn();
    expect(await copyText("a.b", say)).toBe(true);
    expect(writeText).toHaveBeenCalledWith("a.b");
    expect(say).toHaveBeenCalledWith("Copied a.b");
  });

  it("says just Copied. for an empty or blank text", async () => {
    stub(vi.fn(() => Promise.resolve()));
    const say = vi.fn();
    await copyText("", say);
    await copyText(" \n", say);
    expect(say).toHaveBeenNthCalledWith(1, "Copied.");
    expect(say).toHaveBeenNthCalledWith(2, "Copied.");
  });

  it("says why when it fails", async () => {
    stub(() => Promise.reject(new Error("no")));
    const say = vi.fn();
    expect(await copyText("x", say)).toBe(false);
    expect(say).toHaveBeenCalledWith(failedWords);
  });

  it("no clipboard API", async () => {
    vi.stubGlobal("navigator", {});
    const say = vi.fn();
    expect(await copyText("x", say)).toBe(false);
    expect(say).toHaveBeenCalledWith(failedWords);
  });

  it("calls writeText before any await", () => {
    const writeText = vi.fn(() => Promise.resolve());
    stub(writeText);
    void copyText("x");
    expect(writeText).toHaveBeenCalledTimes(1);
  });
});
