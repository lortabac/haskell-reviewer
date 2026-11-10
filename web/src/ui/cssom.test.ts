import { describe, expect, it } from "vitest";
import { setVar } from "./cssom";

describe("setVar", () => {
  it("sets a custom property", () => {
    const el = document.createElement("div");
    setVar(el, "--code-y", "42px");
    expect(el.getAttribute("style")).toMatch(/--code-y:\s*42px/);
  });

  it.each(["color", "-x", "--", "--A", "--x;color:red"])(
    "rejects %j",
    (name) => {
      expect(() => setVar(document.createElement("div"), name, "1")).toThrow(
        "not a custom property",
      );
    },
  );
});
