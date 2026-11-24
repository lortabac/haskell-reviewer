// View on base / head and b (D-349). fixtures/repos/marks.sh and
// sides.sh with --diff HEAD~1..HEAD. src/Marks.hs, head → base: 9 → 8
// (d = 4), 4 → 5 and 5 → 6 (inserted), 7 → 7 and 8 → 7 (replaced).
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

const view = (page: Page): Locator => page.locator("main .code-view");
const cell = (page: Page, n: number): Locator =>
  view(page).locator(`.code-row[data-line="${n}"] .code-cell`);
const status = (page: Page) => page.locator(".keys-status");
// Focus on line n: its cell, or a link in it.
const lineFocus = (page: Page, n: number): Locator =>
  view(page).locator(`.code-row[data-line="${n}"]`).locator(":focus");
const item = (page: Page) => page.locator(".frame-header .side-switch");

// A source page, settled: the marks are in (mark: a line's name) and
// the header item is a link. Never press keys before.
async function openSource(
  page: Page,
  server: Server,
  rest: string,
  mark: string,
) {
  await page.goto(server.base + "src/" + rest);
  await expect(view(page)).toBeVisible();
  await expect(
    view(page).locator(`[role="rowheader"][aria-label="${mark}"]`),
  ).toHaveCount(1);
  await expect(item(page)).toHaveJSProperty("tagName", "A");
}

test("source: an equal line maps exactly; Back returns", async ({
  page,
  startServer,
  marksProject,
}) => {
  const server = await start(startServer, marksProject.root);
  await openSource(page, server, "head/src/Marks.hs#L9", "7, modified");
  await cell(page, 9).click();
  await page.keyboard.press("b");
  await expect(page).toHaveURL(/\/src\/base\/src\/Marks\.hs#L8$/);
  await expect(lineFocus(page, 8)).toHaveCount(1);
  await expect(cell(page, 8)).toHaveText("d = 4");
  await page.goBack();
  await expect(page).toHaveURL(/\/src\/head\/src\/Marks\.hs#L9$/);
  await expect(lineFocus(page, 9)).toHaveCount(1);
});

test("source: inside an insertion, the nearest equal line", async ({
  page,
  startServer,
  marksProject,
}) => {
  const server = await start(startServer, marksProject.root);
  for (const [from, to, text] of [
    [4, 5, "a = 1"],
    [5, 6, "b = 2"],
  ] as const) {
    await openSource(page, server, `head/src/Marks.hs#L${from}`, "7, modified");
    await cell(page, from).click();
    await page.keyboard.press("b");
    await expect(page).toHaveURL(
      new RegExp(`/src/base/src/Marks\\.hs#L${to}$`),
    );
    await expect(lineFocus(page, to)).toHaveCount(1);
    await expect(cell(page, to)).toHaveText(text);
    await expect(status(page)).toHaveText(
      `Line ${from} isn't on base: nearest line ${to}.`,
    );
  }
});

test("source: the header item; a rename goes to the other path", async ({
  page,
  startServer,
  marksProject,
}) => {
  const server = await start(startServer, marksProject.root);
  await openSource(page, server, "head/ren/New.hs#L5", "3, modified");
  await expect(item(page)).toHaveText("View on base");
  await expect(item(page)).toHaveAttribute(
    "href",
    /\/src\/base\/ren\/Old\.hs$/,
  );
  await cell(page, 5).click();
  await item(page).click();
  await expect(page).toHaveURL(/\/src\/base\/ren\/Old\.hs#L5$/);
  await expect(lineFocus(page, 5)).toHaveCount(1);
});

test("source: only on head is disabled, with the reason", async ({
  page,
  startServer,
  marksProject,
}) => {
  const server = await start(startServer, marksProject.root);
  await page.goto(server.base + "src/head/added.txt");
  await expect(view(page)).toBeVisible();
  await expect(item(page)).toHaveAttribute("data-tip", "Only on head.");
  await expect(item(page)).toHaveAttribute("aria-disabled", "true");
  await expect(item(page)).toHaveAccessibleName("View on base");
  await expect(item(page)).toHaveAccessibleDescription("Only on head.");
  await item(page).focus();
  await expect(item(page)).toBeFocused();
  await page.keyboard.press("Enter");
  await expect(status(page)).toHaveText("Only on head.");
  await cell(page, 1).click();
  await page.keyboard.press("b");
  await expect(status(page)).toHaveText(/^Only on head\./);
  await expect(page).toHaveURL(/\/src\/head\/added\.txt$/);
});

test("source: a head-only file opened on base offers View on head", async ({
  page,
  startServer,
  marksProject,
  guard,
}) => {
  // Firefox logs no console line for a 404.
  guard.allow(
    /Failed to load resource: the server responded with a status of 404.* \[\S*\/api\/(file|source)\/base\/added\.txt\]/,
  );
  const server = await start(startServer, marksProject.root);
  await page.goto(server.base + "src/base/added.txt");
  await expect(page.locator("main h1")).toHaveText(/not found/i);
  await expect(item(page)).toHaveText("View on head");
  await expect(item(page)).toHaveAttribute("href", /\/src\/head\/added\.txt$/);
  await item(page).click();
  await expect(page).toHaveURL(/\/src\/head\/added\.txt$/);
  await expect(view(page)).toBeVisible();
});

test("source: an unchanged file maps 1:1 with no diff fetch", async ({
  page,
  startServer,
  marksProject,
}) => {
  const server = await start(startServer, marksProject.root);
  const diffs: string[] = [];
  page.on("request", (r) => {
    if (r.url().includes("/api/diff/")) diffs.push(r.url());
  });
  await page.goto(server.base + "src/head/same.txt#L1");
  await expect(view(page)).toBeVisible();
  await expect(item(page)).toHaveJSProperty("tagName", "A");
  await cell(page, 1).click();
  await page.keyboard.press("b");
  await expect(page).toHaveURL(/\/src\/base\/same\.txt#L1$/);
  await expect(lineFocus(page, 1)).toHaveCount(1);
  expect(diffs).toEqual([]);
});

// A docs page, settled: the header item is a link (both trees in).
async function openDocs(page: Page, server: Server, rest: string) {
  await page.goto(server.base + "docs/" + rest);
  await expect(page.locator("main h1")).toBeVisible();
  await expect(item(page)).toHaveJSProperty("tagName", "A");
}

test("docs: the same declaration; missing there, the notice", async ({
  page,
  startServer,
  marksProject,
}) => {
  const server = await start(startServer, marksProject.root);
  await openDocs(page, server, "head/Marks#val:d");
  const focused = page.locator("main section.decl:focus");
  await expect(focused).toHaveCount(1);
  await page.keyboard.press("b");
  await expect(page).toHaveURL(/\/docs\/base\/Marks#val%3Ad$/);
  await expect(focused).toHaveCount(1);
  await expect(focused).toContainText("d");
  await page.goBack();
  await expect(page).toHaveURL(/\/docs\/head\/Marks#val(:|%3A)d$/);
  await openDocs(page, server, "head/Marks#val:inserted1");
  await expect(focused).toHaveCount(1);
  await page.keyboard.press("b");
  await expect(page).toHaveURL(/\/docs\/base\/Marks#val%3Ainserted1$/);
  await expect(page.locator("main .decl-missing")).toContainText(
    "No declaration",
  );
});

test("docs: a renamed loose module, through the file rename", async ({
  page,
  startServer,
  marksProject,
}) => {
  const server = await start(startServer, marksProject.root);
  // Loose: the ids are the paths, the refs the file names.
  await openDocs(page, server, "head/New");
  await expect(item(page)).toHaveAttribute("href", /\/docs\/base\/Old$/);
  await item(page).click();
  // The first declaration on screen comes along.
  await expect(page).toHaveURL(/\/docs\/base\/Old#val%3Ax$/);
  await expect(page.locator("main h1")).toHaveText("Ren");
});

test("docs: a module only on head is disabled", async ({
  page,
  startServer,
  sidesProject,
}) => {
  const server = await start(startServer, sidesProject.root);
  await page.goto(server.base + "docs/head/C");
  await expect(page.locator("main h1")).toHaveText("C");
  await expect(item(page)).toHaveAttribute("data-tip", "Only on head.");
  await expect(item(page)).toHaveAttribute("aria-disabled", "true");
  await page.locator("main h1").focus();
  await page.keyboard.press("b");
  await expect(status(page)).toHaveText("Only on head.");
  // A is on both.
  await openDocs(page, server, "head/A");
  await expect(item(page)).toHaveAttribute("href", /\/docs\/base\/A$/);
});

// A diff page, settled: the grid is in and the hash marks its row.
async function openDiff(page: Page, server: Server, rest: string) {
  await page.goto(server.base + "diff/" + rest);
  await expect(page.locator("main .code-line-target").first()).toBeVisible();
}

// The focused cell's side, and its row's name (unified).
const focusedCell = (page: Page) =>
  page.evaluate(() => {
    const c = document.activeElement?.closest<HTMLElement>(".code-cell");
    const n = c?.closest(".code-row")?.querySelector('[role="rowheader"]');
    return `${c?.dataset.side ?? "?"} ${n?.getAttribute("aria-label") ?? "?"}`;
  });

test("diff, side by side: b crosses the columns, the hash follows", async ({
  page,
  startServer,
  marksProject,
}) => {
  const server = await start(startServer, marksProject.root);
  // Wide enough for side by side.
  await page.setViewportSize({ width: 1400, height: 800 });
  await openDiff(page, server, "src/Marks.hs?layout=split#head-L9");
  await expect(item(page)).toHaveCount(0);
  await page.keyboard.press("b");
  await expect(page).toHaveURL(/\?layout=split#base-L8$/);
  await expect.poll(() => focusedCell(page)).toMatch(/^base /);
  await page.keyboard.press("b");
  await expect(page).toHaveURL(/\?layout=split#head-L9$/);
  await expect.poll(() => focusedCell(page)).toMatch(/^head /);
  // An empty cell: head's line 8 has no base cell, so base line 7.
  await openDiff(page, server, "src/Marks.hs?layout=split#head-L8");
  await page.keyboard.press("b");
  await expect(page).toHaveURL(/\?layout=split#base-L7$/);
  // The button: focus stays, the cell is said.
  const button = page.getByRole("button", { name: /^Other side/ });
  await button.click();
  await expect(page).toHaveURL(/\?layout=split#head-L7$/);
  await expect(button).toBeFocused();
  await expect(status(page)).toHaveText("added line 7");
});

test("diff, unified: a removed line to the nearest head line", async ({
  page,
  startServer,
  marksProject,
}) => {
  const server = await start(startServer, marksProject.root);
  await openDiff(page, server, "src/Marks.hs?layout=unified#base-L1");
  await page.keyboard.press("b");
  await expect(page).toHaveURL(/#head-L1$/);
  await expect
    .poll(() => focusedCell(page))
    .toMatch(/ line 1, unchanged, base line 3$/);
  await page.keyboard.press("b");
  await expect(status(page)).toHaveText(
    "Same line on both sides: base 3, head 1.",
  );
});

for (const t of themes) {
  test(`axe, ${t.scheme} theme: the item on and off`, async ({
    page,
    startServer,
    marksProject,
  }) => {
    const server = await start(startServer, marksProject.root);
    await page.emulateMedia({ colorScheme: t.scheme });
    await openSource(page, server, "head/src/Marks.hs", "7, modified");
    await item(page).focus();
    await expectNoAxeViolations(page, `side switch on (${t.scheme})`);
    await page.goto(server.base + "src/head/added.txt");
    await expect(item(page)).toHaveAttribute("data-tip", "Only on head.");
    await item(page).focus();
    await expectNoAxeViolations(page, `side switch off (${t.scheme})`);
  });
}

test("320 px: no sideways scroll with the item", async ({
  page,
  startServer,
  marksProject,
}) => {
  const server = await start(startServer, marksProject.root);
  await page.setViewportSize({ width: 320, height: 640 });
  await openSource(page, server, "head/src/Marks.hs", "7, modified");
  await expect(item(page)).toBeVisible();
  await noSideways(page);
  await page.goto(server.base + "src/head/added.txt");
  await expect(item(page)).toHaveAttribute("data-tip", "Only on head.");
  await noSideways(page);
});
