// Find in the diff view (D-346). fixtures/repos/diffview.sh with
// --diff HEAD~1..HEAD. "double" in src/Pairs.hs: base lines 3, 6, 15;
// head lines 3, 7, 12, 15. Unified: 5 hits (equal rows once, on
// head). Split: 7 (equal rows on both sides).
import type { Locator, Page } from "@playwright/test";
import { expectNoAxeViolations, noSideways, themes } from "./support/a11y";
import { expect, test } from "./support/fixtures";
import type { Server } from "./support/server";

const diff = "HEAD~1..HEAD";

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

const findButton = (page: Page) =>
  page.getByRole("button", { name: "Find", exact: true });
const box = (page: Page) => page.getByRole("textbox", { name: "Find" });
const live = (page: Page) =>
  page.locator("main .code-toolbar .code-find-status");

// Open the bar and search. The input has focus.
async function find(page: Page, q: string): Promise<void> {
  if ((await findButton(page).getAttribute("aria-expanded")) !== "true") {
    await findButton(page).click();
  }
  await expect(box(page)).toBeFocused();
  await box(page).fill(q);
}

// The code cell whose row (unified) or half (split) number says label.
const unifiedCell = (g: Locator, label: string) =>
  g
    .locator(".code-row", {
      has: g.page().locator(`.code-num[aria-label="${label}"]`),
    })
    .locator(".code-cell");
const splitCell = (g: Locator, label: string) =>
  g
    .locator(".diff-half", {
      has: g.page().locator(`.code-num[aria-label="${label}"]`),
    })
    .locator(".code-cell");

// Focus is in l: on it, or on a link in it.
const focusIn = (l: Locator) =>
  expect
    .poll(() => l.evaluate((el) => el.contains(document.activeElement)))
    .toBe(true);

function ranges(page: Page, name: string): Promise<string[]> {
  return page.evaluate(
    (n) =>
      [...(CSS.highlights.get(n) ?? [])].map((r) => (r as Range).toString()),
    name,
  );
}

test("the Find button comes first; the hint is FindBar's", async ({
  page,
  startServer,
  diffviewProject,
}) => {
  const server = await start(startServer, diffviewProject.root);
  await open(page, server, "src/Pairs.hs");
  const first = page.locator("main .code-toolbar button").first();
  await expect(first).toHaveText("Find");
  await expect(page.locator("main .code-hint")).toHaveText(
    "Browser find sees only visible lines — use Find (f) to search the whole file",
  );
});

test("unified: both sides in row order, equal rows once", async ({
  page,
  startServer,
  diffviewProject,
}) => {
  const server = await start(startServer, diffviewProject.root);
  const g = await open(page, server, "src/Pairs.hs");
  await page.keyboard.press("f");
  await expect(box(page)).toBeFocused();
  await box(page).fill("double");
  await expect(live(page)).toHaveText("Match 1 of 5, line 3, unchanged");
  const said = [
    "Match 2 of 5, removed line 6",
    "Match 3 of 5, added line 7",
    "Match 4 of 5, added line 12",
    "Match 5 of 5, line 15, unchanged",
    "Match 1 of 5, line 3, unchanged",
  ];
  for (const s of said) {
    await page.keyboard.press("Enter");
    await expect(live(page)).toHaveText(s);
  }
  await page.keyboard.press("Shift+Enter");
  await expect(live(page)).toHaveText("Match 5 of 5, line 15, unchanged");
  await expect
    .poll(() => ranges(page, "code-find-current"))
    .toEqual(["double"]);
  await expect(g.locator(".code-row.code-find-line .code-num")).toHaveAttribute(
    "aria-label",
    "line 15, unchanged",
  );
  // The outline is on the hit.
  const outline = page.locator(".code-find-box");
  await expect(outline).toHaveCount(1);
  await expect(outline).toBeInViewport();
});

test("split: base before head on a row, equal rows twice", async ({
  page,
  startServer,
  diffviewProject,
}) => {
  const server = await start(startServer, diffviewProject.root);
  const g = await open(page, server, "src/Pairs.hs?layout=split");
  await find(page, "double");
  await expect(live(page)).toHaveText("Match 1 of 7, base line 3, unchanged");
  const said = [
    "Match 2 of 7, line 3, unchanged",
    "Match 3 of 7, removed line 6",
    "Match 4 of 7, added line 7",
    "Match 5 of 7, added line 12",
    "Match 6 of 7, base line 15, unchanged",
    "Match 7 of 7, line 15, unchanged",
  ];
  for (const s of said) {
    await page.keyboard.press("Enter");
    await expect(live(page)).toHaveText(s);
  }
  // Line 15 ends with "double k" past the half's right edge: scrolled
  // to, and the outline stays inside the head half.
  await expect
    .poll(() => ranges(page, "code-find-current"))
    .toEqual(["double"]);
  await expect(
    g.locator(".diff-half.code-find-line .code-num"),
  ).toHaveAttribute("aria-label", "line 15, unchanged");
  const outline = page.locator(".code-find-box");
  await expect(outline).toHaveCount(1);
  const clip = g
    .locator(".diff-half.code-find-line .diff-clip")
    .first()
    .boundingBox();
  const o = await outline.boundingBox();
  const c = await clip;
  expect(o).not.toBeNull();
  expect(c).not.toBeNull();
  expect(o!.x).toBeGreaterThanOrEqual(c!.x - 1);
  expect(o!.x + o!.width).toBeLessThanOrEqual(c!.x + c!.width + 1);
  // Esc: focus on the hit.
  await page.keyboard.press("Escape");
  await expect(findButton(page)).toHaveAttribute("aria-expanded", "false");
  await focusIn(splitCell(g, "line 15, unchanged"));
});

test("a layout switch searches again, from the cursor", async ({
  page,
  startServer,
  diffviewProject,
}) => {
  const server = await start(startServer, diffviewProject.root);
  await open(page, server, "src/Pairs.hs");
  await find(page, "double");
  await page.keyboard.press("Enter");
  await expect(live(page)).toHaveText("Match 2 of 5, removed line 6");
  await page.getByRole("button", { name: "Side by side" }).click();
  await expect(grid(page).locator(".diff-half").first()).toBeVisible();
  await expect(live(page)).toHaveText("Match 3 of 7, removed line 6");
  await page.getByRole("button", { name: "Unified" }).click();
  await expect(grid(page).locator(".diff-half")).toHaveCount(0);
  await expect(live(page)).toHaveText("Match 2 of 5, removed line 6");
});

test("a hit in a gap opens it", async ({
  page,
  startServer,
  diffviewProject,
}) => {
  const server = await start(startServer, diffviewProject.root);
  const g = await open(page, server, "src/Calc.hs");
  const row48 = unifiedCell(g, "line 48, unchanged");
  await expect(row48).toHaveCount(0);
  await find(page, "f20 = ");
  await expect(live(page)).toHaveText("Match 1 of 1, line 48, unchanged");
  await expect(row48).toBeInViewport();
  await expect
    .poll(() => ranges(page, "code-find-current"))
    .toEqual(["f20 = "]);
  // The outline is on the hit's row.
  const o = await page.locator(".code-find-box").boundingBox();
  const r = await row48.boundingBox();
  expect(o).not.toBeNull();
  expect(r).not.toBeNull();
  expect(Math.abs(o!.y - r!.y)).toBeLessThan(r!.height);
  // Close: focus on the hit; the gap stays open.
  await page.getByRole("button", { name: "Close" }).click();
  await focusIn(row48);
  await expect(row48).toBeInViewport();
});

test("a 20,000-line diff: an off-screen hit is scrolled to", async ({
  page,
  startServer,
  diffviewProject,
}) => {
  const server = await start(startServer, diffviewProject.root);
  const g = await open(page, server, "big.txt");
  const cell = unifiedCell(g, "added line 19999");
  await expect(cell).toHaveCount(0);
  await find(page, "head line 19999");
  await expect(live(page)).toHaveText("Match 1 of 1, added line 19999");
  await expect(cell).toBeInViewport();
  expect(await g.locator(".code-row").count()).toBeLessThan(100);
  await page.keyboard.press("Escape");
  await expect(cell).toBeFocused();
  // One letter: every line of both sides.
  await find(page, "e");
  await expect(live(page)).toHaveText(/^Match \d+ of 80000, /);
});

test("320 px: the bar open, no sideways scroll", async ({
  page,
  startServer,
  diffviewProject,
}) => {
  const server = await start(startServer, diffviewProject.root);
  await open(page, server, "src/Pairs.hs", 320);
  await find(page, "double");
  await expect(live(page)).toHaveText(/^Match 1 of 5/);
  await noSideways(page);
});

for (const t of themes) {
  test(`axe with the bar open, ${t.scheme} theme`, async ({
    page,
    startServer,
    diffviewProject,
  }) => {
    const server = await start(startServer, diffviewProject.root);
    await page.emulateMedia({ colorScheme: t.scheme });
    await open(page, server, "src/Pairs.hs?layout=split");
    await find(page, "double");
    await page.keyboard.press("Enter");
    await expect(live(page)).toHaveText(/^Match 2 of 7/);
    await expectNoAxeViolations(page, `diff find (${t.scheme})`);
  });
}
