// Text search (D-309, D-310): "text" in quotes in the header box, the
// results page, the source view landing on the match. Keyboard first.
import type { Locator, Page } from "@playwright/test";
import { expectNoAxeViolations as axe, themes } from "./support/a11y";
import { expect, moduleNotFound404, test } from "./support/fixtures";
import { lineOf, linksMainHs } from "./support/project";
import { settle } from "./support/settle";

const mainUrl = "src/tree/Links/Main.hs";
const goLine = lineOf(linksMainHs, "    go ys");

const box = (page: Page): Locator =>
  page
    .locator(".frame-header")
    .getByRole("combobox", { name: "Search modules and names" });
const options = (page: Page): Locator =>
  page.locator(".frame-header .search-results a[role=option]");
const status = (page: Page): Locator =>
  page.locator(".frame-header .search-status");
const row = (page: Page, n: number): Locator =>
  page.locator(`main .code-view .code-row[data-line="${n}"]`);

function currentHit(page: Page): Promise<string[]> {
  return page.evaluate(() =>
    [...(CSS.highlights.get("code-find-current") ?? [])].map((r) =>
      (r as Range).toString(),
    ),
  );
}

test("box: a name query ends with the text row; picking it searches text", async ({
  page,
  startServer,
  linksProject,
}) => {
  const server = await startServer({ root: linksProject.root });
  await page.goto(server.base);
  await settle(page);
  await box(page).fill("map area");
  await expect(options(page).last()).toHaveText("Search text for 'map area'");
  // ArrowDown to the last row, then Enter.
  const n = await options(page).count();
  for (let i = 0; i < n; i++) await box(page).press("ArrowDown");
  await box(page).press("Enter");
  await expect(box(page)).toHaveValue('"map area"');
  await expect(box(page)).toHaveAttribute("aria-expanded", "true");
  await expect(options(page)).toHaveCount(2);
  await expect(options(page).first()).toContainText(`Links/Main.hs:${goLine}`);
  await expect(options(page).first().locator("mark")).toHaveText("map area");
  await expect(options(page).last()).toHaveText("All results (1)");
  await expect(status(page)).toHaveText("1 matching line.");
  await settle(page);
});

test('box: an unclosed "fo sends no request until the quote is closed', async ({
  page,
  startServer,
  linksProject,
}) => {
  const server = await startServer({ root: linksProject.root });
  await page.goto(server.base);
  await settle(page);
  const asked: string[] = [];
  page.on("request", (r) => {
    if (r.url().includes("/api/text-search/")) asked.push(r.url());
  });
  await box(page).pressSequentially('"are', { delay: 30 });
  await expect(options(page)).toHaveCount(1);
  await expect(options(page)).toHaveText("Search text for 'are'");
  await expect(status(page)).toHaveText("Close the quote to search text.");
  await page.waitForTimeout(400);
  expect(asked).toEqual([]);
  await box(page).press("End");
  await box(page).press('"');
  await expect(options(page).last()).toHaveText(/^All results \(\d+\)$/);
  expect(asked).toHaveLength(1);
  expect(asked[0]).toContain("/api/text-search/tree?q=are&limit=8");
  // 8 hit rows, then All results.
  await expect(options(page)).toHaveCount(9);
  await expect(status(page)).toHaveText(/^Showing 8 of \d+ matching lines\.$/);
  await settle(page);
});

test("results page: Enter opens it, a row lands on the match, Back focuses the row", async ({
  page,
  startServer,
  linksProject,
}) => {
  const server = await startServer({ root: linksProject.root });
  await page.goto(server.base);
  await settle(page);
  await box(page).fill('"map area"');
  await box(page).press("Enter");
  await expect(page).toHaveURL(server.base + "search/tree?q=map%20area");
  const h1 = page.locator("main h1");
  await expect(h1).toHaveText('Text search: "map area"');
  await expect(h1).toBeFocused();
  await expect(page).toHaveTitle(/Text search: "map area"/);
  await expect(page.locator("main .refs-summary")).toHaveText(
    "1 matching line in 1 file.",
  );
  const group = page.locator("main .refs-group");
  await expect(group.getByRole("heading", { level: 2 })).toHaveText(
    "Links/Main.hs",
  );
  const item = group.locator("li a").first();
  await expect(item).toHaveAttribute(
    "href",
    `/${server.token}/${mainUrl}#L${goLine}`,
  );
  await expect(item.locator("mark")).toHaveText("map area");
  await item.focus();
  await page.keyboard.press("Enter");
  await expect(page).toHaveURL(server.base + mainUrl + `#L${goLine}`);
  // The match is marked as the current find hit, no find bar opened.
  await expect.poll(() => currentHit(page)).toEqual(["map area"]);
  await expect(page.locator("main .code-find-button")).toHaveAttribute(
    "aria-expanded",
    "false",
  );
  // The cursor is on the match: "map", a link, once the highlight is
  // there.
  await expect(
    row(page, goLine).getByRole("link", { name: /^map, / }),
  ).toBeFocused();
  // The cursor on a link shows its card: Esc hides it first.
  await page.mouse.move(0, 0);
  const card = page.locator("main .code-card");
  await expect(card).toBeVisible();
  await page.keyboard.press("Escape");
  await expect(card).toBeHidden();
  expect(await currentHit(page)).toEqual(["map area"]);
  // Then one Esc takes the mark away.
  await page.keyboard.press("Escape");
  await expect.poll(() => currentHit(page)).toEqual([]);
  await page.goBack();
  await expect(page).toHaveURL(server.base + "search/tree?q=map%20area");
  await expect(item).toBeFocused();
  await settle(page);
});

test("notice page: its box searches text too", async ({
  page,
  startServer,
  linksProject,
  guard,
}) => {
  guard.allow(moduleNotFound404);
  const server = await startServer({ root: linksProject.root });
  await page.goto(server.base + "docs/tree/No.Such");
  const pageBox = page
    .locator("main")
    .getByRole("combobox", { name: "Search modules and names" });
  await expect(page.locator("main .search-hint")).toContainText(
    "Put text in double quotes",
  );
  await pageBox.fill('"map area"');
  const hits = page.locator("main .search-results a[data-kind=hit]");
  await expect(hits).toHaveCount(1);
  await expect(page.locator("main .search-status")).toHaveText(
    "1 matching line.",
  );
  await pageBox.press("Enter");
  await expect(page).toHaveURL(server.base + "search/tree?q=map%20area");
  await expect(page.locator("main h1")).toBeFocused();
  await settle(page);
});

test("results page: a reload serves the shell; a dropdown hit lands too", async ({
  page,
  startServer,
  linksProject,
}) => {
  const server = await startServer({ root: linksProject.root });
  await page.goto(server.base + "search/tree?q=area");
  await expect(page.locator("main .refs-summary")).toHaveText(
    /^\d+ matching lines in 2 files\.$/,
  );
  await page.reload();
  await expect(page.locator("main h1")).toHaveText('Text search: "area"');
  await box(page).fill('"map area"');
  await expect(options(page)).toHaveCount(2);
  await box(page).press("ArrowDown");
  await box(page).press("Enter");
  await expect(page).toHaveURL(server.base + mainUrl + `#L${goLine}`);
  await expect.poll(() => currentHit(page)).toEqual(["map area"]);
  await settle(page);
});

test("the ? overlay says how to search text", async ({
  page,
  startServer,
  linksProject,
}) => {
  const server = await startServer({ root: linksProject.root });
  await page.goto(server.base);
  await settle(page);
  await page.keyboard.press("?");
  await expect(page.getByRole("dialog")).toContainText(
    'Focus the search box ("text" in quotes searches files)',
  );
  await settle(page);
});

for (const t of themes) {
  test(`axe: the open text dropdown and the results page, ${t.scheme} theme`, async ({
    page,
    startServer,
    linksProject,
  }) => {
    const server = await startServer({ root: linksProject.root });
    await page.emulateMedia({ colorScheme: t.scheme });
    await page.goto(server.base);
    await settle(page);
    await box(page).fill('"area"');
    await expect(options(page)).toHaveCount(9);
    await box(page).press("ArrowDown");
    await axe(page, `text dropdown, ${t.scheme}`);
    await box(page).fill('"are');
    await expect(options(page)).toHaveCount(1);
    await axe(page, `unclosed quote dropdown, ${t.scheme}`);
    await page.goto(server.base + "search/tree?q=area");
    await expect(page.locator("main .refs-group").first()).toBeVisible();
    await axe(page, `text results page, ${t.scheme}`);
    await settle(page);
  });
}
