import { describe, expect, it } from "vitest";
import { fits } from "./fit";

describe("fits", () => {
  it("60rem first", () => {
    expect(fits(undefined, 960, 16)).toBe(true);
    expect(fits(undefined, 959, 16)).toBe(false);
    expect(fits(true, 960, 16)).toBe(true);
    expect(fits(true, 959, 16)).toBe(false);
  });

  it("62rem to come back", () => {
    expect(fits(false, 960, 16)).toBe(false);
    expect(fits(false, 991, 16)).toBe(false);
    expect(fits(false, 992, 16)).toBe(true);
  });

  it("200% text: twice the width", () => {
    expect(fits(undefined, 1280, 32)).toBe(false);
    expect(fits(undefined, 1920, 32)).toBe(true);
  });
});
