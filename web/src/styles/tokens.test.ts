// @vitest-environment node
// Contrast check for the design tokens (SPEC §5.4): every pair we
// actually put text, a ring or a border on needs the right ratio, in
// both themes. Reads tokens.css as text (fs, like test/lint.test.ts):
// no DOM, no browser colour math, so this can't drift from what's on
// screen.
import { readFileSync } from "node:fs";
import { join } from "node:path";
import { describe, expect, it } from "vitest";

const css = readFileSync(join(import.meta.dirname, "tokens.css"), "utf-8");

// Each --color-<name>'s [light, dark] hex value.
function tokens(): Record<string, [string, string]> {
  const out: Record<string, [string, string]> = {};
  const re =
    /--color-([\w-]+):\s*light-dark\((#[0-9a-f]{6}),\s*(#[0-9a-f]{6})\)/g;
  for (const m of css.matchAll(re)) {
    out[m[1]!] = [m[2]!, m[3]!];
  }
  return out;
}

// WCAG relative luminance and contrast ratio.
function luminance(hex: string): number {
  const n = Number.parseInt(hex.slice(1), 16);
  const channel = (shift: number): number => {
    const c = ((n >> shift) & 0xff) / 255;
    return c <= 0.03928 ? c / 12.92 : ((c + 0.055) / 1.055) ** 2.4;
  };
  return 0.2126 * channel(16) + 0.7152 * channel(8) + 0.0722 * channel(0);
}

function contrast(a: string, b: string): number {
  const sorted = [luminance(a), luminance(b)].sort((x, y) => y - x);
  const lighter = sorted[0]!;
  const darker = sorted[1]!;
  return (lighter + 0.05) / (darker + 0.05);
}

const t = tokens();

function ratio(a: string, b: string, theme: 0 | 1): number {
  const ta = t[a];
  const tb = t[b];
  if (ta === undefined) throw new Error(`no token "${a}"`);
  if (tb === undefined) throw new Error(`no token "${b}"`);
  return contrast(ta[theme], tb[theme]);
}

// The highlight classes' colours (SPEC §5.6).
const hl = [
  "hl-keyword",
  "hl-type",
  "hl-variable",
  "hl-operator",
  "hl-literal",
  "hl-comment",
  "hl-pragma",
  "hl-excluded",
];

// Backgrounds behind source view text.
const codeBgs = ["bg", "surface", "mark", "selection"];

describe("design tokens: contrast (SPEC §5.4, tokens.css header)", () => {
  it("found every token used below", () => {
    for (const name of [
      "fg",
      "bg",
      "muted",
      "surface",
      "border",
      "link",
      "focus",
      "mark",
      "selection",
      ...hl,
    ]) {
      expect(t[name], `token --color-${name}`).toBeDefined();
    }
  });

  const textPairs: [string, string][] = [
    ["fg", "bg"],
    ["fg", "surface"],
    ["fg", "border"],
    ["fg", "mark"],
    ["muted", "bg"],
    ["muted", "surface"],
    ["link", "bg"],
    ["link", "surface"],
    ...["fg", ...hl].flatMap((f) =>
      codeBgs.map((b): [string, string] => [f, b]),
    ),
    ["muted", "selection"],
  ];

  it.each(textPairs)("text %s on %s is >= 4.5:1 in both themes", (a, b) => {
    expect(ratio(a, b, 0)).toBeGreaterThanOrEqual(4.5);
    expect(ratio(a, b, 1)).toBeGreaterThanOrEqual(4.5);
  });

  // A ring or a border that only has to be seen, not read.
  const uiPairs: [string, string][] = [
    ["focus", "bg"],
    ["focus", "surface"],
    ["muted", "bg"],
    ["muted", "surface"],
    // The dashed bar by excluded lines.
    ["hl-excluded", "surface"],
  ];

  it.each(uiPairs)("non-text %s on %s is >= 3:1 in both themes", (a, b) => {
    expect(ratio(a, b, 0)).toBeGreaterThanOrEqual(3);
    expect(ratio(a, b, 1)).toBeGreaterThanOrEqual(3);
  });
});
