// The source view's change marks in diff mode (D-348).
// fixtures/repos/marks.sh with --diff HEAD~1..HEAD. src/Marks.hs, head:
// 1 removed above, 4-5 added, 7-8 modified, 11 removed below. Base: 1-2
// removed, 5 added below, 7 modified, 11-12 removed.
import type { Locator, Page } from "@playwright/test";
import { expectNoAxeViolations, noSideways, themes } from "./support/a11y";
import { expect, test } from "./support/fixtures";
import type { Server } from "./support/server";

const diff = "HEAD~1..HEAD";

async function start(
  startServer: (o: { root: string; diff?: string }) => Promise<Server>,
  root: string,
  withDiff = true,
): Promise<Server> {
  const server = await startServer(withDiff ? { root, diff } : { root });
  await server.waitForLog(/indexing: done/, 20_000);
  return server;
}

const view = (page: Page): Locator => page.locator("main .code-view");
const row = (page: Page, n: number): Locator =>
  view(page).locator(`.code-row[data-line="${n}"]`);
const cell = (page: Page, n: number): Locator =>
  row(page, n).locator(".code-cell");
const name = (page: Page, n: number) =>
  row(page, n).locator('[role="rowheader"]');
const status = (page: Page) => page.locator(".keys-status");

// Open a source page and wait until the marks are in: the app renders
// after api/session, and the marks after api/diff. Never press keys
// before.
async function open(page: Page, server: Server, rest: string, mark: string) {
  await page.goto(server.base + "src/" + rest);
  await expect(view(page)).toBeVisible();
  await expect(
    view(page).locator(`[role="rowheader"][aria-label="${mark}"]`),
  ).toHaveCount(1);
}

const focusedLine = (page: Page) =>
  page.evaluate(() =>
    document.activeElement?.closest(".code-row")?.getAttribute("data-line"),
  );

test("head: names of the marked lines", async ({
  page,
  startServer,
  marksProject,
}) => {
  const server = await start(startServer, marksProject.root);
  await open(page, server, "head/src/Marks.hs", "7, modified");
  await expect(name(page, 1)).toHaveAttribute(
    "aria-label",
    "1, lines removed above",
  );
  await expect(name(page, 2)).toHaveAttribute("aria-label", "2");
  await expect(name(page, 4)).toHaveAttribute("aria-label", "4, added");
  await expect(name(page, 5)).toHaveAttribute("aria-label", "5, added");
  await expect(name(page, 8)).toHaveAttribute("aria-label", "8, modified");
  await expect(name(page, 11)).toHaveAttribute(
    "aria-label",
    "11, lines removed below",
  );
  // The sign is CSS, in the gutter.
  const sign = await row(page, 4)
    .locator(".code-num")
    .evaluate((e) => getComputedStyle(e, "::after").content);
  expect(sign).toContain("+");
});

test("base: names of the marked lines", async ({
  page,
  startServer,
  marksProject,
}) => {
  const server = await start(startServer, marksProject.root);
  await open(page, server, "base/src/Marks.hs", "7, modified");
  await expect(name(page, 1)).toHaveAttribute("aria-label", "1, removed");
  await expect(name(page, 2)).toHaveAttribute("aria-label", "2, removed");
  await expect(name(page, 5)).toHaveAttribute(
    "aria-label",
    "5, lines added below",
  );
  await expect(name(page, 11)).toHaveAttribute("aria-label", "11, removed");
  await expect(name(page, 12)).toHaveAttribute("aria-label", "12, removed");
});

test("n and p step between changes; no more at the ends", async ({
  page,
  startServer,
  marksProject,
}) => {
  const server = await start(startServer, marksProject.root);
  await open(page, server, "head/src/Marks.hs", "7, modified");
  await page.keyboard.press("n");
  await expect.poll(() => focusedLine(page)).toBe("1");
  for (const n of ["4", "7", "11"]) {
    await page.keyboard.press("n");
    await expect.poll(() => focusedLine(page)).toBe(n);
  }
  await page.keyboard.press("n");
  await expect(status(page)).toHaveText(/^No more changes\./);
  await page.keyboard.press("p");
  await expect.poll(() => focusedLine(page)).toBe("7");
});

test("the buttons move the cursor and keep focus", async ({
  page,
  startServer,
  marksProject,
}) => {
  const server = await start(startServer, marksProject.root);
  await open(page, server, "head/src/Marks.hs", "7, modified");
  const next = page.getByRole("button", { name: "Next change" });
  await next.focus();
  await next.click();
  await expect(status(page)).toHaveText("line 1, lines removed above");
  await next.click();
  await expect(status(page)).toHaveText("added line 4");
  await expect(next).toBeFocused();
  await expect(row(page, 4)).toHaveClass(/code-cursor-line/);
  await page.getByRole("button", { name: "Previous change" }).click();
  await expect(status(page)).toHaveText("line 1, lines removed above");
});

test("3 and Show diff here: the diff at the line; Back returns", async ({
  page,
  startServer,
  marksProject,
}) => {
  const server = await start(startServer, marksProject.root);
  await open(page, server, "head/src/Marks.hs#L4", "7, modified");
  await cell(page, 4).click();
  await page.keyboard.press("3");
  await expect(page).toHaveURL(/\/diff\/src\/Marks\.hs#head-L4$/);
  const g = page.getByRole("grid", { name: /^Diff of / });
  await expect(
    g.locator(`[role="rowheader"][aria-label="added line 4"]`),
  ).toBeVisible();
  await expect(
    g
      .locator(".code-row", {
        has: page.locator('[aria-label="added line 4"]'),
      })
      .locator(":focus"),
  ).toHaveCount(1);
  await page.goBack();
  await expect(page).toHaveURL(/\/src\/head\/src\/Marks\.hs#L4$/);
  await expect(cell(page, 4)).toBeFocused();
  await page.getByRole("button", { name: /^Show diff here/ }).click();
  await expect(page).toHaveURL(/\/diff\/src\/Marks\.hs#head-L4$/);
});

test("the header Diff item", async ({ page, startServer, marksProject }) => {
  const server = await start(startServer, marksProject.root);
  await open(page, server, "head/src/Marks.hs", "7, modified");
  const item = page.getByRole("navigation", { name: "View" });
  await item.getByRole("link", { name: "Diff" }).click();
  await expect(page).toHaveURL(/\/diff\/src\/Marks\.hs#head-L1$/);
});

test("base of a rename: 3 goes to the head path, base line", async ({
  page,
  startServer,
  marksProject,
}) => {
  const server = await start(startServer, marksProject.root);
  await open(page, server, "base/ren/Old.hs#L3", "3, modified");
  await page.keyboard.press("3");
  await expect(page).toHaveURL(/\/diff\/ren\/New\.hs#base-L3$/);
  await expect(
    page.locator('[role="rowheader"][aria-label="removed line 3"]'),
  ).toBeVisible();
});

test("added and removed files are marked; unchanged says why", async ({
  page,
  startServer,
  marksProject,
}) => {
  const server = await start(startServer, marksProject.root);
  await open(page, server, "head/added.txt", "1, added");
  await expect(name(page, 2)).toHaveAttribute("aria-label", "2, added");
  await open(page, server, "base/removed.txt", "1, removed");
  await open(page, server, "head/same.txt", "1");
  // Settled: api/diff-files is in, and the header has no diff to link.
  await expect(
    page.locator(".view-switch .view-missing", { hasText: "Diff" }),
  ).toHaveCount(1);
  await expect(page.locator(".code-chg")).toHaveCount(0);
  await expect(page.locator(".code-view-changes")).toHaveCount(1);
  await page.keyboard.press("3");
  await expect(status(page)).toHaveText("No changes in this file.");
  await page.getByRole("button", { name: /^Show diff here/ }).click();
  await expect(status(page)).toHaveText(/^No changes in this file\./);
  await expect(page.getByRole("button", { name: "Next change" })).toHaveCount(
    0,
  );
  // EOL only: nothing marked, and n says so.
  const eol = page.waitForResponse((r) =>
    r.url().endsWith("/api/diff/eol.txt"),
  );
  await open(page, server, "head/eol.txt", "1");
  await eol;
  // The buttons come with the diff.
  await expect(page.getByRole("button", { name: "Next change" })).toBeVisible();
  await page.keyboard.press("n");
  await expect(status(page)).toHaveText("No changes in the text.");
});

test("browse mode: no column, no buttons", async ({
  page,
  startServer,
  marksProject,
}) => {
  const server = await start(startServer, marksProject.root, false);
  await page.goto(server.base + "src/tree/src/Marks.hs");
  await expect(view(page)).toBeVisible();
  await expect(page.locator(".code-view-changes")).toHaveCount(0);
  await expect(
    page.getByRole("button", { name: /Show diff here/ }),
  ).toHaveCount(0);
  await expect(page.getByRole("button", { name: "Next change" })).toHaveCount(
    0,
  );
  await expect(page.getByRole("link", { name: "Diff" })).toHaveCount(0);
});

for (const t of themes) {
  test(`axe, ${t.scheme} theme, with a marked line`, async ({
    page,
    startServer,
    marksProject,
  }) => {
    const server = await start(startServer, marksProject.root);
    await page.emulateMedia({ colorScheme: t.scheme });
    await open(page, server, "head/src/Marks.hs#L7-L8", "7, modified");
    await row(page, 4).locator(".code-cell").click();
    await expectNoAxeViolations(page, `source marks (${t.scheme})`);
    await open(page, server, "base/src/Marks.hs#L1", "1, removed");
    await expectNoAxeViolations(page, `source marks, base (${t.scheme})`);
  });
}

test("forced colours: the glyph and a border stay", async ({
  page,
  browserName,
  startServer,
  marksProject,
}) => {
  test.skip(browserName !== "chromium", "forced colours emulation: Chromium");
  await page.emulateMedia({ forcedColors: "active" });
  const server = await start(startServer, marksProject.root);
  await open(page, server, "head/src/Marks.hs", "7, modified");
  const style = await row(page, 4)
    .locator(".code-num")
    .evaluate((e) => ({
      content: getComputedStyle(e, "::after").content,
      border: getComputedStyle(e).borderRightWidth,
    }));
  expect(style.content).toContain("+");
  expect(style.border).toBe("3px");
});

test("320 px: no sideways scroll, the text doesn't move when marks come", async ({
  page,
  startServer,
  marksProject,
}) => {
  const server = await start(startServer, marksProject.root);
  await page.setViewportSize({ width: 320, height: 640 });
  // Hold api/diff back, to measure before the marks.
  let release!: () => void;
  const held = new Promise<void>((r) => (release = r));
  await page.route("**/api/diff/**", async (route) => {
    await held;
    await route.continue();
  });
  await page.goto(server.base + "src/head/src/Marks.hs");
  await expect(view(page)).toBeVisible();
  await expect(name(page, 7)).toHaveAttribute("aria-label", "7");
  const x = () => cell(page, 3).evaluate((e) => e.getBoundingClientRect().x);
  const before = await x();
  release();
  await expect(name(page, 7)).toHaveAttribute("aria-label", "7, modified");
  expect(await x()).toBe(before);
  await noSideways(page);
});
