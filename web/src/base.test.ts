import { describe, expect, it } from "vitest";
import { basePath } from "./base";

describe("basePath", () => {
  it("is the first path segment", () => {
    expect(basePath("/abc/")).toBe("/abc");
    expect(basePath("/abc")).toBe("/abc");
    expect(basePath("/abc/src/tree/a/b.hs")).toBe("/abc");
  });

  it("is / at the root", () => {
    expect(basePath("/")).toBe("/");
    expect(basePath("")).toBe("/");
  });
});
