import { describe, expect, it } from "vitest";
import { textQueryFromLocation, textSearchHref } from "./urls";

describe("text search urls", () => {
  it("round trip, + and spaces", () => {
    for (const q of ["main", "a+b", ' a "b ', "ü%", "&q=x"]) {
      const href = textSearchHref("tree", q);
      expect(textQueryFromLocation(href.slice(href.indexOf("?")))).toBe(q);
    }
  });

  it("a raw + stays +, the first q counts, bad or empty is undefined", () => {
    expect(textQueryFromLocation("?q=a+b")).toBe("a+b");
    expect(textQueryFromLocation("?q=one&q=two")).toBe("one");
    expect(textQueryFromLocation("?q=%E0")).toBeUndefined();
    expect(textQueryFromLocation("?q=")).toBeUndefined();
    expect(textQueryFromLocation("")).toBeUndefined();
  });
});
