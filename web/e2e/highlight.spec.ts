// Highlighting in the source view (T07b, SPEC §4.4, §5.6): classes and
// theme colours, CPP lines, the outline, copy under a late highlight.
import type { Locator, Page } from "@playwright/test";
import { readFileSync } from "node:fs";
import { join } from "node:path";
import {
  expectNoAxeViolations as axe,
  noSideways,
  themes,
} from "./support/a11y";
import { type At, copy, recordCopies, select, textSlice } from "./support/copy";
import { expect, tabKey, test } from "./support/fixtures";
import { hlHs, hlLine, litLhs } from "./support/project";

// Each --color-hl-* token: [light, dark] as rgb().
function hlTokens(): Record<string, [string, string]> {
  const css = readFileSync(
    join(import.meta.dirname, "../src/styles/tokens.css"),
    "utf-8",
  );
  const rgb = (hex: string) => {
    const n = Number.parseInt(hex.slice(1), 16);
    return `rgb(${(n >> 16) & 255}, ${(n >> 8) & 255}, ${n & 255})`;
  };
  const out: Record<string, [string, string]> = {};
  const re =
    /--color-hl-(\w+):\s*light-dark\((#[0-9a-f]{6}),\s*(#[0-9a-f]{6})\)/g;
  for (const m of css.matchAll(re)) out[m[1]!] = [rgb(m[2]!), rgb(m[3]!)];
  return out;
}

const tokens = hlTokens();

// A span's class, its token, and some text it has in Hl.hs.
const classes: [string, string, string][] = [
  ["keyword", "keyword", "module"],
  ["typeCon", "type", "Box"],
  ["variable", "variable", "sorted"],
  ["operator", "operator", "<+>"],
  ["literal", "literal", '"y"'],
  ["comment", "comment", "-- | Every class."],
  ["pragma", "pragma", "{-# LANGUAGE CPP #-}"],
  ["cppDirective", "pragma", "#if 0"],
  ["cppExcluded", "excluded", "hidden = 1"],
];

function view(page: Page): Locator {
  return page.locator("main .code-view");
}

function row(page: Page, n: number): Locator {
  return view(page).locator(`.code-row[aria-rowindex="${n}"]`);
}

// Open a file and wait for its highlight.
async function open(page: Page, url: string): Promise<void> {
  await page.goto(url);
  await expect(view(page).locator(".hl-keyword").first()).toBeVisible();
}

// hlHs from a to b.
const slice = (a: At, b: At) => textSlice(hlHs, a, b);

for (const [i, t] of themes.entries()) {
  test(`classes and colours, ${t.scheme} theme, axe`, async ({
    page,
    startServer,
    hlProject,
  }) => {
    const server = await startServer({ root: hlProject.root });
    await page.emulateMedia({ colorScheme: t.scheme });
    await open(page, server.base + "src/tree/Hl.hs");
    for (const [cls, token, text] of classes) {
      const span = view(page)
        .locator(`.hl-${cls}`)
        .filter({ hasText: text })
        .first();
      await expect(span, cls).toBeVisible();
      const want = tokens[token]?.[i];
      expect(want, token).toBeDefined();
      await expect(span, cls).toHaveCSS("color", want!);
    }
    // Plain: brackets, "=", "::".
    const ty = row(page, hlLine("sorted ::")).locator(".code-cell");
    await expect(ty).toHaveText("sorted :: [Int] -> [Int]");
    await expect(ty.locator(".hl-variable")).toHaveText("sorted");
    // Excluded: greyed, a dashed bar, a label.
    const ex = row(page, hlLine("hidden"));
    await expect(ex).toHaveClass(/code-excluded/);
    await expect(ex.locator(".code-mark")).toHaveCSS(
      "border-left-style",
      "dashed",
    );
    await expect(
      ex.getByRole("rowheader", {
        name: `${hlLine("hidden")}, excluded by CPP`,
      }),
    ).toHaveCount(1);
    await expect(
      row(page, hlLine("#if")).getByRole("rowheader", {
        name: `${hlLine("#if")}, CPP directive`,
      }),
    ).toHaveCount(1);
    // The text is still the file's.
    await expect(
      row(page, hlLine("astral =")).locator(".code-cell"),
    ).toHaveText('astral = "\u{1d465}" ++\t"y"');
    await page.locator(".code-outline summary").click();
    await axe(page, `highlighted source, ${t.scheme}`);
  });
}

test("outline markers link to the docs", async ({
  page,
  startServer,
  hlProject,
}) => {
  const server = await startServer({ root: hlProject.root });
  await open(page, server.base + "src/tree/Hl.hs");
  const mark = view(page).getByRole("link", { name: "Docs: sorted" });
  await expect(mark).toHaveAttribute("tabindex", "-1");
  // On the declaration's line.
  await expect(
    view(page).locator(".code-row", {
      has: page.getByRole("link", { name: "Docs: sorted" }),
    }),
  ).toHaveAttribute("data-line", String(hlLine("sorted ::")));
  await mark.click();
  await expect(page).toHaveURL(server.base + "docs/tree/Hl#val%3Asorted");
  await expect(page.locator("main h1")).toHaveText("Hl");
});

test("the outline list, by keyboard", async ({
  page,
  startServer,
  hlProject,
  browserName,
}) => {
  const server = await startServer({ root: hlProject.root });
  await open(page, server.base + "src/tree/Hl.hs");
  const tab = tabKey(browserName);
  const summary = page.locator(".code-outline summary");
  await expect(summary).toHaveText(/^Outline \(\d+\)$/);
  // Right after the line (the view's one stop) in Tab order.
  await view(page).locator(".code-cell[tabindex='0']").focus();
  await page.keyboard.press(tab);
  await expect(summary).toBeFocused();
  // Open: Tab goes to the first entry.
  await page.keyboard.press("Enter");
  const nav = page.getByRole("navigation", { name: "Outline" });
  await expect(nav).toBeVisible();
  await page.keyboard.press(tab);
  await expect(nav.getByRole("link").first()).toBeFocused();
  // "sorted"'s line link marks its line.
  const item = nav.getByRole("listitem").filter({ hasText: "sorted" });
  await item.getByRole("link", { name: /^line \d+$/ }).focus();
  await page.keyboard.press("Enter");
  await expect(page).toHaveURL(new RegExp(`#L${hlLine("sorted ::")}$`));
  await expect(view(page).locator(".code-line-target")).toHaveCount(1);
  // Its name goes to the docs.
  await item.getByRole("link", { name: "sorted", exact: true }).focus();
  await page.keyboard.press("Enter");
  await expect(page).toHaveURL(server.base + "docs/tree/Hl#val%3Asorted");
});

test("the outline shows an instance as written", async ({
  page,
  startServer,
  hlProject,
}) => {
  const server = await startServer({ root: hlProject.root });
  await open(page, server.base + "src/tree/Hl.hs");
  await page.locator(".code-outline summary").click();
  const nav = page.getByRole("navigation", { name: "Outline" });
  await expect(
    nav.getByRole("listitem").filter({ hasText: "instance" }),
  ).toHaveText(/^instance C Maybe \(Maybe a\) line \d+$/);
});

test("copy across highlighted lines is the file's text", async ({
  page,
  startServer,
  hlProject,
}) => {
  const server = await startServer({ root: hlProject.root });
  await open(page, server.base + "src/tree/Hl.hs");
  await recordCopies(page);
  const a: At = [hlLine("a <+>"), 2];
  const b: At = [hlLine("astral ="), 14];
  await select(page, a, b);
  expect(await copy(page)).toBe(slice(a, b));
});

test("a late highlight keeps the selection and focus; copy exact", async ({
  page,
  startServer,
  hlProject,
}) => {
  const server = await startServer({ root: hlProject.root });
  const release = await holdSource(page);
  await page.goto(server.base + "src/tree/Hl.hs");
  await expect(row(page, 1)).toBeVisible();
  await expect(view(page).locator(".hl-keyword")).toHaveCount(0);
  await recordCopies(page);
  // Focus a line, then select from it to another.
  const n = hlLine("sorted xs");
  await row(page, n).locator(".code-cell").click();
  await expect(row(page, n).locator(".code-cell")).toBeFocused();
  const a: At = [hlLine("module"), 3];
  const b: At = [n, 9];
  await select(page, a, b);
  const shown = await page.evaluate(() => getSelection()?.toString());
  expect(await copy(page)).toBe(slice(a, b));
  release();
  await expect(view(page).locator(".hl-keyword").first()).toBeVisible();
  await expect(row(page, n).locator(".code-cell span").first()).toBeAttached();
  expect(await page.evaluate(() => getSelection()?.toString())).toBe(shown);
  await expect(row(page, n).locator(".code-cell")).toBeFocused();
  expect(await copy(page)).toBe(slice(a, b));
});

test("literate: prose is comment, bird marks plain", async ({
  page,
  startServer,
  hlProject,
}) => {
  const server = await startServer({ root: hlProject.root });
  await open(page, server.base + "src/tree/Lit.lhs");
  await expect(row(page, 1).locator(".hl-comment")).toHaveText("Some prose.");
  const code = row(page, 3).locator(".code-cell");
  await expect(code).toHaveText(litLhs.split("\n")[2]!);
  await expect(code.locator(".hl-keyword")).toHaveText(["module", "where"]);
  await expect(code.locator("span").first()).not.toHaveText(/>/);
});

test("320 px: no sideways page scroll, markers stick with the gutter", async ({
  page,
  startServer,
  hlProject,
}) => {
  const server = await startServer({ root: hlProject.root });
  await page.setViewportSize({ width: 320, height: 640 });
  await open(page, server.base + "src/tree/Hl.hs");
  await noSideways(page);
  const v = view(page);
  await v.evaluate((el) => {
    el.scrollLeft = 100;
  });
  await expect.poll(() => v.evaluate((el) => el.scrollLeft)).toBeGreaterThan(0);
  const r = row(page, hlLine("hidden"));
  const [num, mark] = await Promise.all([
    r.locator(".code-num").boundingBox(),
    r.locator(".code-mark").boundingBox(),
  ]);
  if (num === null || mark === null) throw new Error("no box");
  expect(Math.abs(num.x + num.width - mark.x)).toBeLessThanOrEqual(1);
  await noSideways(page);
});

// Holds api/source until release() (the text comes first).
async function holdSource(page: Page): Promise<() => void> {
  let release: (() => void) | undefined;
  const held = new Promise<void>((r) => (release = r));
  await page.route("**/api/source/**", async (route) => {
    await held;
    await route.continue();
  });
  return () => release?.();
}

test("Ctrl+A, then a late highlight: copy is still the whole file", async ({
  page,
  startServer,
  hlProject,
}) => {
  const server = await startServer({ root: hlProject.root });
  const release = await holdSource(page);
  await page.goto(server.base + "src/tree/Hl.hs");
  await expect(row(page, 1)).toBeVisible();
  await recordCopies(page);
  await row(page, 3).locator(".code-cell").click();
  await expect(row(page, 3).locator(".code-cell")).toBeFocused();
  await page.keyboard.press("ControlOrMeta+a");
  expect(await copy(page)).toBe(hlHs);
  release();
  await expect(view(page).locator(".hl-keyword").first()).toBeVisible();
  expect(await copy(page)).toBe(hlHs);
});

for (const width of [1280, 320]) {
  test(`a late highlight doesn't move the view, ${width} px`, async ({
    page,
    startServer,
    hlProject,
  }) => {
    const server = await startServer({ root: hlProject.root });
    await page.setViewportSize({ width, height: 800 });
    const release = await holdSource(page);
    await page.goto(server.base + "src/tree/Hl.hs");
    await expect(row(page, 1)).toBeVisible();
    const top = async () => (await view(page).boundingBox())?.y;
    const before = await top();
    expect(before).toBeDefined();
    release();
    await expect(view(page).locator(".hl-keyword").first()).toBeVisible();
    await expect(page.locator(".code-outline")).toBeVisible();
    expect(await top()).toBe(before);
    await expect(row(page, 1)).toBeInViewport();
  });
}
