import { describe, expect, it } from "vitest";
import { applyTheme } from "./theme";

describe("applyTheme", () => {
  it("system: no override, data-theme absent", () => {
    const el = document.createElement("html");
    el.setAttribute("data-theme", "dark");
    applyTheme(el, "system");
    expect(el.hasAttribute("data-theme")).toBe(false);
  });

  it("light and dark: set data-theme", () => {
    const el = document.createElement("html");
    applyTheme(el, "light");
    expect(el.getAttribute("data-theme")).toBe("light");
    applyTheme(el, "dark");
    expect(el.getAttribute("data-theme")).toBe("dark");
  });
});
