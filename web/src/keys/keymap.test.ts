import { parseKeybinding } from "tinykeys";
import { describe, expect, it } from "vitest";
import { keymap } from "./keymap";

describe("keymap", () => {
  it("ids and bindings are unique", () => {
    const ids = keymap.map((r) => r.id);
    expect(new Set(ids).size).toBe(ids.length);
    const bs = keymap.flatMap((r) => r.bindings);
    expect(new Set(bs).size).toBe(bs.length);
  });

  it("no binding is the start of another (it would never fire)", () => {
    const presses = keymap
      .flatMap((r) => r.bindings)
      .map((b) => parseKeybinding(b).map((p) => String(p[2])));
    for (const a of presses) {
      for (const b of presses) {
        if (a === b || a.length >= b.length) continue;
        expect(b.slice(0, a.length)).not.toEqual(a);
      }
    }
  });

  it("every row has keys, a label and a UI control (D-45)", () => {
    for (const r of keymap) {
      expect(r.keys.length).toBeGreaterThan(0);
      expect(r.label).not.toBe("");
      expect(r.control).not.toBe("");
    }
  });

  it("the G8 keys, and y", () => {
    const keys = keymap.flatMap((r) => r.keys);
    for (const k of ["/", "j", "k", "Enter", "g", "d", "r", "s", "f"]) {
      expect(keys).toContain(k);
    }
    for (const k of ["1", "2", "Esc", "?", "y"]) expect(keys).toContain(k);
    expect(keymap.find((r) => r.id === "open")?.bindings).toEqual([]);
  });

  it("Shift only on ? / 1 2", () => {
    const withShift = keymap
      .filter((r) => r.bindings.some((b) => b.includes("Shift")))
      .map((r) => r.keys.join(""));
    expect(withShift.sort()).toEqual(["/", "1", "2", "?"].sort());
  });
});
