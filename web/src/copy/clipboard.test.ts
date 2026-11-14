import { afterEach, describe, expect, it, vi } from "vitest";
import { copiedWords, copyText, failedWords } from "./clipboard";

afterEach(() => vi.unstubAllGlobals());

function stub(writeText: unknown) {
  vi.stubGlobal("navigator", { clipboard: { writeText } });
}

describe("copyText", () => {
  it("writes the text and says Copied.", async () => {
    const writeText = vi.fn(() => Promise.resolve());
    stub(writeText);
    const say = vi.fn();
    expect(await copyText("a.b", say)).toBe(true);
    expect(writeText).toHaveBeenCalledWith("a.b");
    expect(say).toHaveBeenCalledWith(copiedWords);
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
