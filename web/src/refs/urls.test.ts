import { describe, expect, it } from "vitest";
import { refsFromLocation, refsHref } from "./urls";

describe("refs urls", () => {
  it("round trip, odd characters", () => {
    for (const [m, d] of [
      ["R.Shape", "val:area"],
      ["a b/c#d", "val:<+>"],
      ["M", "inst:Show:Maybe a+b?"],
    ] as const) {
      const [path, query] = refsHref(m, d).split("?") as [string, string];
      expect(refsFromLocation("/tok" + path, "?" + query)).toEqual({
        module: m,
        decl: d,
      });
    }
  });

  it("needs a decl and a good path", () => {
    expect(refsFromLocation("/tok/refs/tree/M", "")).toBeUndefined();
    expect(refsFromLocation("/tok/refs/tree/M", "?decl=")).toBeUndefined();
    expect(refsFromLocation("/tok/refs/tree", "?decl=x")).toBeUndefined();
    expect(refsFromLocation("/tok/refs/tree/../M", "?decl=x")).toBeUndefined();
    expect(refsFromLocation("/tok/docs/tree/M", "?decl=x")).toBeUndefined();
  });

  it("a raw + stays +, %2B works, the first decl counts", () => {
    const at = (q: string) => refsFromLocation("/tok/refs/tree/M", q)?.decl;
    expect(at("?decl=a+b")).toBe("a+b");
    expect(at("?decl=a%2Bb")).toBe("a+b");
    expect(at("?x=1&decl=v%3Af&decl=g")).toBe("v:f");
    expect(at("?decl=%E0%A4")).toBeUndefined();
    // A bad encoding elsewhere is skipped.
    expect(at("?x=%E0%A4&%E0=1&decl=v%3Af")).toBe("v:f");
  });
});
