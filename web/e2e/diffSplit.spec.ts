// The side-by-side diff (D-344): ?layout=split. fixtures/repos/diffview.sh
// with --diff HEAD~1..HEAD. src/Pairs.hs: base 6-7 against head 6-9,
// base 10-12 against head 12, line 15 is 301 characters. Split rows:
// a gap of 2 (lines 1-2), then 15 rows.
import type { Locator, Page } from "@playwright/test";
import { expectNoAxeViolations, noSideways, themes } from "./support/a11y";
import { expect, test } from "./support/fixtures";
import type { Server } from "./support/server";

const diff = "HEAD~1..HEAD";
const note = "Side by side needs a wider window: showing unified.";

async function start(
  startServer: (o: { root: string; diff?: string }) => Promise<Server>,
  root: string,
): Promise<Server> {
  const server = await startServer({ root, diff });
  await server.waitForLog(/indexing: done/, 20_000);
  return server;
}

const grid = (page: Page) => page.getByRole("grid", { name: /^Diff of / });

// Open a diff and wait for its rows (never press keys before).
async function open(
  page: Page,
  server: Server,
  rest: string,
  width = 1400,
): Promise<Locator> {
  await page.setViewportSize({ width, height: 800 });
  await page.goto(server.base + "diff/" + rest);
  const g = grid(page);
  await expect(g.locator(".code-row").first()).toBeVisible();
  return g;
}

// The half whose number says label, and its code cell.
const half = (g: Locator, label: string) =>
  g.locator(".diff-half", {
    has: g.page().locator(`.code-num[aria-label="${label}"]`),
  });
const code = (g: Locator, label: string) =>
  half(g, label).locator(".code-cell");
const rowOf = (g: Locator, label: string) =>
  g.locator(".code-row", {
    has: g.page().locator(`.code-num[aria-label="${label}"]`),
  });
// Focus is in l: on it, or on a link in it.
const focusIn = (l: Locator) =>
  expect
    .poll(() => l.evaluate((el) => el.contains(document.activeElement)))
    .toBe(true);

test("rows pair removed and added lines; empty cells are named", async ({
  page,
  startServer,
  diffviewProject,
}) => {
  const server = await start(startServer, diffviewProject.root);
  const g = await open(page, server, "src/Pairs.hs?layout=split");
  await expect(g).toHaveAttribute("aria-rowcount", "16");
  await expect(g).toHaveAttribute("aria-colcount", "4");
  // Two code cells a row.
  await expect(rowOf(g, "removed line 6").locator(".code-cell")).toHaveCount(2);
  // 6 with 6, 7 with 7, then head's 8 and 9 alone.
  for (const [b, h] of [
    ["removed line 6", "added line 6"],
    ["removed line 7", "added line 7"],
    ["removed line 10", "added line 12"],
  ] as const) {
    await expect(rowOf(g, b)).toHaveAttribute(
      "aria-rowindex",
      (await rowOf(g, h).getAttribute("aria-rowindex")) ?? "",
    );
  }
  await expect(
    rowOf(g, "added line 8").locator('.code-cell[data-side="base"]'),
  ).toHaveAttribute("aria-label", "No base line");
  await expect(
    rowOf(g, "removed line 11").locator('.code-cell[data-side="head"]'),
  ).toHaveAttribute("aria-label", "No head line");
  await expect(code(g, "removed line 6")).toHaveText("pick n = double n");
  await expect(code(g, "added line 6")).toHaveText("pick n = triple n");
  await expect(code(g, "base line 8, unchanged")).toHaveText("");
  await expect(code(g, "line 10, unchanged")).toHaveText("");
  // The gap spans both sides.
  const gap = g.locator(".diff-gap-cell");
  await expect(gap).toHaveAttribute("aria-colspan", "4");
  await g.getByRole("button", { name: "Show 2 lines, 1 to 2" }).click();
  await expect(gap).toHaveCount(0);
  await expect(g).toHaveAttribute("aria-rowcount", "17");
  await focusIn(code(g, "line 1, unchanged"));
});

test("links go to their own side; same-file ones keep layout=", async ({
  page,
  startServer,
  diffviewProject,
}) => {
  const server = await start(startServer, diffviewProject.root);
  const g = await open(page, server, "src/Pairs.hs?layout=split");
  const double = code(g, "removed line 6").getByRole("link", {
    name: /^double, /,
  });
  await expect(double).toHaveAttribute(
    "href",
    /\/src\/base\/src\/Util\.hs#L3$/,
  );
  await expect(
    code(g, "added line 6").getByRole("link", { name: /^triple, / }),
  ).toHaveAttribute("href", /\/src\/head\/src\/Util\.hs#L6$/);
  // A local on head's line 7: head's line 6, this layout.
  const n = code(g, "added line 7").getByRole("link", { name: /^n, / });
  await expect(n).toHaveAttribute(
    "href",
    /\/diff\/src\/Pairs\.hs\?layout=split#head-L6$/,
  );
  await n.click();
  await expect(page).toHaveURL(/\?layout=split#head-L6$/);
  await expect(half(g, "added line 6")).toHaveClass(/code-line-target/);
  await expect(half(g, "removed line 6")).not.toHaveClass(/code-line-target/);
  await focusIn(code(g, "added line 6"));
  await double.click();
  await expect(page).toHaveURL(/\/src\/base\/src\/Util\.hs#L3$/);
});

test("a hash marks its side only", async ({
  page,
  startServer,
  diffviewProject,
}) => {
  const server = await start(startServer, diffviewProject.root);
  const g = await open(page, server, "src/Pairs.hs?layout=split#base-L9");
  await expect(half(g, "base line 9, unchanged")).toHaveClass(
    /code-line-target/,
  );
  await expect(half(g, "line 11, unchanged")).not.toHaveClass(
    /code-line-target/,
  );
  await open(page, server, "src/Pairs.hs?layout=split#head-L11");
  await expect(half(g, "line 11, unchanged")).toHaveClass(/code-line-target/);
  await expect(half(g, "base line 9, unchanged")).not.toHaveClass(
    /code-line-target/,
  );
});

test("keys: ↑/↓ keep the side, ←/→ cross, n/p land on the right side", async ({
  page,
  startServer,
  diffviewProject,
}) => {
  const server = await start(startServer, diffviewProject.root);
  const g = await open(page, server, "src/Pairs.hs?layout=split");
  await code(g, "base line 5, unchanged").click({ position: { x: 2, y: 5 } });
  await focusIn(half(g, "base line 5, unchanged"));
  await page.keyboard.press("ArrowDown");
  await focusIn(half(g, "removed line 6"));
  await page.keyboard.press("ArrowDown");
  await page.keyboard.press("ArrowDown");
  // The empty base cell of head's line 8.
  const empty = rowOf(g, "added line 8").locator(
    '.code-cell[data-side="base"]',
  );
  await expect(empty).toBeFocused();
  // No stop in it: → goes to head's cell, ← back to base's end.
  await page.keyboard.press("ArrowRight");
  await focusIn(half(g, "added line 8"));
  await page.keyboard.press("ArrowUp");
  await focusIn(half(g, "added line 7"));
  // Column 0, no link before it: ← goes to base's cell on that row.
  await page.keyboard.press("ArrowLeft");
  await expect(code(g, "removed line 7")).toBeFocused();
  await page.keyboard.press("Home");
  await expect(g.locator(".diff-gap-cell")).toBeFocused();
  // n / p: base when the block starts with a removed line.
  await page.keyboard.press("n");
  await focusIn(half(g, "removed line 6"));
  await page.keyboard.press("n");
  await focusIn(half(g, "removed line 10"));
  await page.keyboard.press("p");
  await focusIn(half(g, "removed line 6"));
  // From head's 6, ← walks its links, then crosses to base's cell.
  const base6 = code(g, "removed line 6");
  await code(g, "added line 6").click({ position: { x: 2, y: 5 } });
  for (let i = 0; i < 6; i++) {
    await page.keyboard.press("ArrowLeft");
    if (await base6.evaluate((el) => el.contains(document.activeElement)))
      break;
  }
  await expect(base6).toBeFocused();
  // 2 opens base's line 6, Back comes to it.
  await page.keyboard.press("2");
  await expect(page).toHaveURL(/\/src\/base\/src\/Pairs\.hs#L6$/);
  await page.goBack();
  await expect(page).toHaveURL(/\?layout=split#base-L6$/);
  await focusIn(half(g, "removed line 6"));
});

test("the long line: → to its last link brings it in; gutters stay", async ({
  page,
  startServer,
  diffviewProject,
}) => {
  const server = await start(startServer, diffviewProject.root);
  const g = await open(page, server, "src/Pairs.hs?layout=split");
  const cell = code(g, "line 15, unchanged");
  const double = cell.getByRole("link", { name: /^double, / });
  await expect(double).toBeAttached();
  const nums = g.locator(".diff-half .code-num");
  const before = await nums.evaluateAll((ns) =>
    ns.map((n) => n.getBoundingClientRect().left),
  );
  await cell.click({ position: { x: 2, y: 5 } });
  for (let i = 0; i < 300; i++) {
    if (await double.evaluate((el) => el === document.activeElement)) break;
    await page.keyboard.press("ArrowRight");
  }
  await expect(double).toBeFocused();
  await expect(double).toBeInViewport({ ratio: 1 });
  // Right of head's number, inside the grid.
  const num = await half(g, "line 15, unchanged")
    .locator(".code-num")
    .boundingBox();
  const link = await double.boundingBox();
  const box = await g.boundingBox();
  expect(link!.x).toBeGreaterThanOrEqual(num!.x + num!.width - 1);
  expect(link!.x + link!.width).toBeLessThanOrEqual(box!.x + box!.width);
  const after = await nums.evaluateAll((ns) =>
    ns.map((n) => n.getBoundingClientRect().left),
  );
  expect(after).toEqual(before);
  // Base's cell moved by the same offset: the sides stay aligned.
  const shift = await g.evaluate((el) =>
    getComputedStyle(el.querySelector(".code-rows")!).getPropertyValue(
      "--diff-x",
    ),
  );
  expect(parseFloat(shift)).toBeGreaterThan(0);
  const offset = () =>
    g.evaluate((el) =>
      parseFloat(
        getComputedStyle(el.querySelector(".code-rows")!).getPropertyValue(
          "--diff-x",
        ),
      ),
    );

  // Up to line 14 (short), head side, offset kept. ← into base: base's
  // text end comes into view.
  await page.keyboard.press("ArrowUp");
  await focusIn(code(g, "line 14, unchanged"));
  const base14 = code(g, "base line 14, unchanged");
  for (let i = 0; i < 8; i++) {
    if (await base14.evaluate((el) => el === document.activeElement)) break;
    await page.keyboard.press("ArrowLeft");
  }
  await expect(base14).toBeFocused();
  const seen = await base14.evaluate((cell) => {
    const range = document.createRange();
    range.selectNodeContents(cell);
    const text = range.getBoundingClientRect();
    const half = cell.closest(".diff-half")!;
    const num = half.querySelector(".code-num")!.getBoundingClientRect();
    const clip = half.querySelector(".diff-clip")!.getBoundingClientRect();
    return {
      start: text.left,
      end: text.right,
      left: num.right,
      right: clip.right,
    };
  });
  // A short line: all of it, not just its last pixel.
  expect(seen.start).toBeGreaterThanOrEqual(seen.left - 1);
  expect(seen.end).toBeLessThanOrEqual(seen.right);

  // Scrolled right, → from base's end goes to head's start: offset 0.
  await g.evaluate((el) => {
    el.scrollLeft = 300;
  });
  await expect.poll(offset).toBeGreaterThan(0);
  await page.keyboard.press("ArrowRight");
  await focusIn(code(g, "line 14, unchanged"));
  await expect.poll(offset).toBe(0);
});

test("too narrow: unified with a note, back to split when wide", async ({
  page,
  startServer,
  diffviewProject,
}) => {
  const server = await start(startServer, diffviewProject.root);
  const g = await open(page, server, "src/Pairs.hs?layout=split", 900);
  const status = page.locator("main .file-note", { hasText: note });
  await expect(status).toBeVisible();
  await expect(g.locator(".diff-half")).toHaveCount(0);
  await expect(g.locator('[role="rowheader"]').first()).toBeAttached();
  // The cursor and focus are kept over a switch.
  await g
    .locator(".code-row", {
      has: page.locator('[role="rowheader"][aria-label="removed line 10"]'),
    })
    .locator(".code-cell")
    .click({ position: { x: 2, y: 5 } });
  await page.setViewportSize({ width: 1400, height: 800 });
  await expect(g.locator(".diff-half").first()).toBeVisible();
  await expect(status).toHaveCount(0);
  await focusIn(half(g, "removed line 10"));
  // The 2rem band: no flapping between 60 and 62rem.
  const wrapWidth = () =>
    page
      .locator("main .diff-text")
      .evaluate((el) => el.getBoundingClientRect().width);
  const delta = 1400 - (await wrapWidth());
  const at = async (w: number) => {
    await page.setViewportSize({ width: Math.round(w + delta), height: 800 });
    // Two frames: the observer, then the frame it asks for.
    await page.evaluate(
      () =>
        new Promise((r) =>
          requestAnimationFrame(() => requestAnimationFrame(r)),
        ),
    );
  };
  await at(976);
  await expect(g.locator(".diff-half").first()).toBeVisible();
  await at(955);
  await expect(status).toBeVisible();
  // at() waited for the observer's frame: still unified.
  await at(976);
  await expect(status).toBeVisible();
  await at(1000);
  await expect(status).toHaveCount(0);
  await expect(g.locator(".diff-half").first()).toBeVisible();
  await focusIn(half(g, "removed line 10"));
});

test("200% text at 1400 px: unified", async ({
  page,
  startServer,
  diffviewProject,
}) => {
  const server = await start(startServer, diffviewProject.root);
  const g = await open(page, server, "src/Pairs.hs?layout=split");
  await expect(g.locator(".diff-half").first()).toBeVisible();
  // Text zoom: a CSSOM write (an inline <style> breaks the CSP).
  await page.evaluate(() =>
    // eslint-disable-next-line no-restricted-syntax -- the test's zoom
    document.documentElement.style.setProperty("font-size", "200%"),
  );
  await expect(
    page.locator("main .file-note", { hasText: note }),
  ).toBeVisible();
  await expect(g.locator(".diff-half")).toHaveCount(0);
});

test("320 px: unified, no sideways scroll", async ({
  page,
  startServer,
  diffviewProject,
}) => {
  const server = await start(startServer, diffviewProject.root);
  await page.setViewportSize({ width: 320, height: 640 });
  await page.goto(server.base + "diff/src/Pairs.hs?layout=split");
  const g = grid(page);
  await expect(g.locator(".code-row").first()).toBeVisible();
  await expect(
    page.locator("main .file-note", { hasText: note }),
  ).toBeVisible();
  await expect(g.locator(".diff-half")).toHaveCount(0);
  await noSideways(page);
});

test("a 20,000-line diff side by side keeps few rows in the DOM", async ({
  page,
  startServer,
  diffviewProject,
}) => {
  const server = await start(startServer, diffviewProject.root);
  const g = await open(page, server, "big.txt?layout=split");
  await expect(g).toHaveAttribute("aria-rowcount", "20000");
  const rows = g.locator(".code-row");
  expect(await rows.count()).toBeLessThan(100);
  await g.evaluate((el) => {
    el.scrollTop = el.scrollHeight / 2;
  });
  await expect(half(g, "added line 10000")).toBeAttached();
  expect(await rows.count()).toBeLessThan(100);
  await code(g, "added line 10000").click({ position: { x: 2, y: 5 } });
  await page.keyboard.press("End");
  await expect(code(g, "added line 20000")).toBeFocused();
  await expect(half(g, "added line 20000")).toBeInViewport();
  expect(await rows.count()).toBeLessThan(100);
});

for (const t of themes) {
  test(`axe side by side, ${t.scheme} theme`, async ({
    page,
    startServer,
    diffviewProject,
  }) => {
    const server = await start(startServer, diffviewProject.root);
    await page.emulateMedia({ colorScheme: t.scheme });
    const g = await open(page, server, "src/Pairs.hs?layout=split#head-L9");
    await expect(
      code(g, "removed line 6").getByRole("link").first(),
    ).toBeVisible();
    await expect(
      code(g, "added line 6").getByRole("link").first(),
    ).toBeVisible();
    await code(g, "removed line 10").click({ position: { x: 2, y: 5 } });
    await expectNoAxeViolations(page, `split diff view (${t.scheme})`);
  });
}
