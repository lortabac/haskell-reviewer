// Cursor line band (T14d, D-302): a soft band on the cursor line while
// the source view has focus. Focus is asserted, not the selection
// (WebKit drops the selection when focus moves, D-278).
import type { Locator, Page } from "@playwright/test";
import { expectNoAxeViolations as axe, themes } from "./support/a11y";
import { expect, test } from "./support/fixtures";

const big = "src/tree/data/lines-20k.txt";

const view = (page: Page): Locator => page.locator("main .code-view");
const row = (page: Page, n: number): Locator =>
  view(page).locator(`.code-row[data-line="${n}"]`);
const cell = (page: Page, n: number): Locator =>
  row(page, n).locator(".code-cell");
const findButton = (page: Page): Locator =>
  page.getByRole("button", { name: "Find", exact: true });
const box = (page: Page): Locator =>
  page.getByRole("textbox", { name: "Find" });

const light = "rgb(236, 239, 243)";
const dark = "rgb(33, 38, 45)";

const bg = (l: Locator): Promise<string> =>
  l.evaluate((e) => getComputedStyle(e).backgroundColor);
const ring = (l: Locator): Promise<boolean> =>
  l.evaluate((e) => e.matches(":focus-visible"));

// The lines (of the first 12) that have the band.
async function banded(page: Page, band: string, upto = 12): Promise<number[]> {
  const out: number[] = [];
  for (let n = 1; n <= upto; n++) {
    if ((await bg(cell(page, n))) === band) out.push(n);
  }
  return out;
}

test.beforeEach(async ({ page, server }) => {
  await page.emulateMedia({ colorScheme: "light" });
  await page.goto(server.base + big);
  await expect(view(page)).toBeVisible();
});

test("click a line, click a line number: the band is on that row only", async ({
  page,
}) => {
  await cell(page, 4).click({ position: { x: 4, y: 4 } });
  await expect(cell(page, 4)).toBeFocused();
  await expect.poll(() => banded(page, light)).toEqual([4]);
  expect(await ring(cell(page, 4))).toBe(false);
  await row(page, 7).locator(".code-num").click();
  await expect(cell(page, 7)).toBeFocused();
  await expect.poll(() => banded(page, light)).toEqual([7]);
  expect(await ring(cell(page, 7))).toBe(false);
  // The number turns fg, not just the row's background.
  const num = row(page, 7).locator(".code-num");
  const color = (l: Locator) => l.evaluate((e) => getComputedStyle(e).color);
  expect(await color(num)).not.toBe(
    await color(row(page, 6).locator(".code-num")),
  );
});

test("ArrowDown moves it; a key shows the ring", async ({ page }) => {
  await cell(page, 4).click({ position: { x: 4, y: 4 } });
  await expect.poll(() => banded(page, light)).toEqual([4]);
  await page.keyboard.press("ArrowDown");
  await expect(cell(page, 5)).toBeFocused();
  await expect.poll(() => banded(page, light)).toEqual([5]);
  expect(await ring(cell(page, 5))).toBe(true);
});

test("j moves it to the next declaration", async ({
  page,
  startServer,
  linksProject,
}) => {
  const server = await startServer({ root: linksProject.root });
  await page.goto(server.base + "src/tree/Links/Main.hs");
  await expect(view(page)).toBeVisible();
  await cell(page, 1).click({ position: { x: 2, y: 4 } });
  await expect.poll(() => banded(page, light)).toEqual([1]);
  await page.keyboard.press("j");
  const now = view(page).locator(".code-cell:focus");
  await expect(now).toHaveCount(1);
  const n = Number(
    await now.evaluate((e) => e.parentElement?.getAttribute("data-line")),
  );
  expect(n).toBeGreaterThan(1);
  await expect.poll(() => banded(page, light, n + 1)).toEqual([n]);
});

test("Esc in the find bar puts it on the hit's line; no band while typing", async ({
  page,
}) => {
  await cell(page, 2).click({ position: { x: 4, y: 4 } });
  await findButton(page).click();
  await expect(box(page)).toBeFocused();
  await box(page).fill("9\tl");
  await expect.poll(() => banded(page, light)).toEqual([]);
  await page.keyboard.press("Escape");
  await expect(view(page).locator(":focus")).toHaveCount(1);
  await expect.poll(() => banded(page, light)).toEqual([9]);
});

test("Shift+Tab out and the Find button hide it", async ({ page }) => {
  await cell(page, 3).click({ position: { x: 4, y: 4 } });
  await expect.poll(() => banded(page, light)).toEqual([3]);
  await page.keyboard.press("Shift+Tab");
  await expect(view(page).locator(":focus")).toHaveCount(0);
  await expect.poll(() => banded(page, light)).toEqual([]);
  await cell(page, 3).click({ position: { x: 4, y: 4 } });
  await expect.poll(() => banded(page, light)).toEqual([3]);
  await findButton(page).click();
  await expect.poll(() => banded(page, light)).toEqual([]);
});

const mark = "rgb(255, 248, 197)";

test("on a #L line the band wins in the code area; the mark stays in the gutter", async ({
  page,
  server,
}) => {
  await page.goto(server.base + big + "#L5");
  await expect(row(page, 5)).toBeVisible();
  await expect(row(page, 5)).toHaveClass(/code-line-target/);
  await cell(page, 5).click({ position: { x: 4, y: 4 } });
  await expect(cell(page, 5)).toBeFocused();
  await expect.poll(() => bg(cell(page, 5))).toBe(light);
  expect(await bg(row(page, 5))).toBe(mark);
  expect(await bg(row(page, 5).locator(".code-num"))).toBe(mark);
  const m = row(page, 5).locator(".code-mark");
  for (let i = 0; i < (await m.count()); i++) {
    expect(await bg(m.nth(i))).toBe(mark);
  }
});

test("a #L jump with focus in the view moves the band", async ({ page }) => {
  await cell(page, 4).click({ position: { x: 4, y: 4 } });
  await expect.poll(() => banded(page, light)).toEqual([4]);
  await page.evaluate(() => {
    location.hash = "#L9";
  });
  await expect(cell(page, 9)).toBeFocused();
  await expect.poll(() => banded(page, light)).toEqual([9]);
});

test("forced colours: the cursor number has an outline, others none", async ({
  page,
  browserName,
}) => {
  test.skip(browserName !== "chromium", "forced colours emulation: Chromium");
  await page.emulateMedia({ forcedColors: "active" });
  await cell(page, 4).click({ position: { x: 4, y: 4 } });
  const outline = (n: number) =>
    row(page, n)
      .locator(".code-num")
      .evaluate((e) => {
        const c = getComputedStyle(e);
        return `${c.outlineWidth} ${c.outlineStyle}`;
      });
  await expect.poll(() => outline(4)).toBe("2px solid");
  expect(await outline(3)).not.toBe("2px solid");
  expect(await outline(5)).not.toBe("2px solid");
});

for (const t of themes) {
  test(`axe with the band, ${t.scheme} theme`, async ({ page }) => {
    await page.emulateMedia({ colorScheme: t.scheme });
    await cell(page, 4).click({ position: { x: 4, y: 4 } });
    await expect
      .poll(() => banded(page, t.scheme === "light" ? light : dark))
      .toEqual([4]);
    await axe(page, `cursor band, ${t.scheme}`);
  });
}
