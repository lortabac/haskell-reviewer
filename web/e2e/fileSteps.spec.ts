// ] and [: changed files and modules (D-350). fixtures/repos/marks.sh
// with --diff HEAD~1..HEAD. Changed: ren/New.hs (from ren/Old.hs),
// src/Marks.hs, added.txt, eol.txt, removed.txt.
import type { Locator, Page } from "@playwright/test";
import { expect, sidebar, test } from "./support/fixtures";
import type { Server } from "./support/server";
import { settle } from "./support/settle";

async function start(
  startServer: (o: { root: string; diff?: string }) => Promise<Server>,
  root: string,
): Promise<Server> {
  const server = await startServer({ root, diff: "HEAD~1..HEAD" });
  await server.waitForLog(/indexing: done/, 20_000);
  return server;
}

const status = (page: Page): Locator => page.locator(".keys-status");
const title = (page: Page): Locator => page.locator("main h1");

// Rendered, and api/diff-files is in: the sidebar has its ghost (the
// file only on the other side). Never press keys before.
async function ready(page: Page, h1: string): Promise<void> {
  await expect(title(page)).toHaveText(h1);
  await settle(page);
  await expect(sidebar(page).locator(".tree-ghost")).not.toHaveCount(0);
}

test("diff: every changed file in sidebar order, ws kept, no wrap", async ({
  page,
  startServer,
  marksProject,
}) => {
  const server = await start(startServer, marksProject.root);
  await page.goto(server.base + "diff/ren/New.hs?ws=ignore");
  await ready(page, "ren/New.hs");
  await page.keyboard.press("[");
  await expect(status(page)).toHaveText("No more changed files.");
  const order = ["src/Marks.hs", "added.txt", "eol.txt", "removed.txt"];
  for (const [i, path] of order.entries()) {
    await page.keyboard.press("]");
    await expect(page).toHaveURL(
      new RegExp(`/diff/${path.replace(".", "\\.")}\\?ws=ignore$`),
    );
    await expect(title(page)).toHaveText(path);
    await expect(status(page)).toHaveText(
      `${path}, changed file ${i + 2} of 5.`,
    );
  }
  await page.keyboard.press("]");
  await expect(status(page)).toHaveText("No more changed files.");
  await expect(page).toHaveURL(/\/diff\/removed\.txt\?ws=ignore$/);
});

test("diff: Back comes to the cursor's line", async ({
  page,
  startServer,
  marksProject,
}) => {
  const server = await start(startServer, marksProject.root);
  await page.goto(server.base + "diff/src/Marks.hs#head-L9");
  await expect(page.locator("main .code-line-target").first()).toBeVisible();
  await ready(page, "src/Marks.hs");
  await page.keyboard.press("]");
  await expect(page).toHaveURL(/\/diff\/added\.txt$/);
  await page.goBack();
  await expect(page).toHaveURL(/\/diff\/src\/Marks\.hs#head-L9$/);
});

test("the Next file button: the next page's button gets focus", async ({
  page,
  startServer,
  marksProject,
}) => {
  const server = await start(startServer, marksProject.root);
  await page.goto(server.base + "diff/ren/New.hs");
  await ready(page, "ren/New.hs");
  const next = page.getByRole("button", { name: "Next file (])" });
  await next.focus();
  await page.keyboard.press("Enter");
  await expect(page).toHaveURL(/\/diff\/src\/Marks\.hs$/);
  await expect(status(page)).toHaveText("src/Marks.hs, changed file 2 of 5.");
  await expect(next).toBeFocused();
  await page.keyboard.press("Enter");
  await expect(page).toHaveURL(/\/diff\/added\.txt$/);
});

test("source: this side's files only, top of the file; Back", async ({
  page,
  startServer,
  marksProject,
}) => {
  const server = await start(startServer, marksProject.root);
  // Head: removed.txt is base's, so eol.txt is the last.
  await page.goto(server.base + "src/head/eol.txt");
  await ready(page, "eol.txt");
  await page.keyboard.press("]");
  await expect(status(page)).toHaveText("No more changed files.");
  await page.keyboard.press("[");
  await expect(page).toHaveURL(/\/src\/head\/added\.txt$/);
  await expect(status(page)).toHaveText("added.txt, changed file 3 of 4.");
  // Base: its own paths (the rename's old one), removed.txt last.
  await page.goto(server.base + "src/base/src/Marks.hs#L8");
  await ready(page, "src/Marks.hs");
  const cell = page.locator('main .code-row[data-line="8"] .code-cell');
  await cell.click();
  await page.keyboard.press("[");
  await expect(page).toHaveURL(/\/src\/base\/ren\/Old\.hs$/);
  await page.goBack();
  await expect(page).toHaveURL(/\/src\/base\/src\/Marks\.hs#L8$/);
  await expect(
    page.locator('main .code-row[data-line="8"]').locator(":focus"),
  ).toHaveCount(1);
  await page.keyboard.press("]");
  await expect(page).toHaveURL(/\/src\/base\/eol\.txt$/);
  await page.keyboard.press("]");
  await expect(page).toHaveURL(/\/src\/base\/removed\.txt$/);
});

test("docs: the changed modules, no wrap", async ({
  page,
  startServer,
  marksProject,
}) => {
  const server = await start(startServer, marksProject.root);
  await page.goto(server.base + "docs/head/Marks");
  await ready(page, "Marks");
  await page.keyboard.press("]");
  await expect(status(page)).toHaveText("No more changed modules.");
  await page.keyboard.press("[");
  await expect(page).toHaveURL(/\/docs\/head\/New$/);
  await expect(status(page)).toHaveText(/, changed module 1 of 2\.$/);
  await page.keyboard.press("[");
  await expect(status(page)).toHaveText("No more changed modules.");
  await page.keyboard.press("]");
  await expect(page).toHaveURL(/\/docs\/head\/Marks$/);
  await expect(status(page)).toHaveText("Marks, changed module 2 of 2.");
});

test("Home: says where it works", async ({
  page,
  startServer,
  marksProject,
}) => {
  const server = await start(startServer, marksProject.root);
  await page.goto(server.base);
  await settle(page);
  await page.keyboard.press("]");
  await expect(status(page)).toHaveText(
    "Changed files: from the diff, source or docs view.",
  );
});
