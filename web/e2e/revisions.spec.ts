// The revisions in the header and the filtered note (D-335, SPEC
// §4.1, §5.7). sides.sh: two commits; edits.sh: one commit plus edits
// in the working tree, data.bin filtered (filter=lfs).
import { execFileSync } from "node:child_process";
import type { Page } from "@playwright/test";
import { expectNoAxeViolations, noSideways, themes } from "./support/a11y";
import { grantClipboard, nextWrite, stubWriteText } from "./support/copy";
import { expect, test } from "./support/fixtures";
import { makeSidesProject } from "./support/project";
import type { Server } from "./support/server";

function git(root: string, ...args: string[]): string {
  return execFileSync("git", ["-C", root, ...args], {
    encoding: "utf-8",
    env: { ...process.env, GIT_CONFIG_GLOBAL: "/dev/null" },
  }).trim();
}

async function start(
  startServer: (o: { root: string; diff?: string }) => Promise<Server>,
  root: string,
  diff: string,
): Promise<Server> {
  const server = await startServer({ root, diff });
  await server.waitForLog(/indexing: done/, 20_000);
  return server;
}

const items = (page: Page) => page.locator(".frame-header .rev-item");

test.beforeEach(async ({ context, browserName }) => {
  await grantClipboard(context, browserName);
});

test("two dots: spec, short hashes, the side; copy gives the full hash", async ({
  page,
  startServer,
  sidesProject,
}) => {
  const head = git(sidesProject.root, "rev-parse", "HEAD");
  const base = git(sidesProject.root, "rev-parse", "HEAD~1");
  await stubWriteText(page);
  const server = await start(startServer, sidesProject.root, "HEAD~1..HEAD");
  await page.goto(server.base + "docs/base/A");
  await expect(page.locator("main")).toContainText("One.");
  await expect(items(page)).toHaveText([
    "Diff HEAD~1..HEAD",
    `base: HEAD~1 ${base.slice(0, 7)}`,
    `head: HEAD ${head.slice(0, 7)}`,
    "Viewing: base",
  ]);
  const copyHead = page
    .locator(".frame-header")
    .getByRole("button", { name: "Copy head commit hash" });
  expect(await nextWrite(page, () => copyHead.click())).toBe(head);
  await expect(page.locator(".keys-status")).toContainText("Copied");
  // Home: no side.
  await page.goto(server.base);
  await expect(items(page).first()).toBeVisible();
  await expect(page.locator(".frame-header .rev-current")).toHaveCount(0);
});

test("working tree: no head hash; the filtered note on its file only", async ({
  page,
  startServer,
  editsProject,
}) => {
  const server = await start(startServer, editsProject.root, "HEAD");
  await page.goto(server.base + "src/head/data.bin");
  await expect(page.locator("main .code-view")).toContainText("big data");
  await expect(items(page).nth(2)).toHaveText("head: working tree");
  await expect(
    page.locator(".frame-header .revisions").getByRole("button"),
  ).toHaveCount(1);
  const note = page.locator("main .file-note");
  await expect(note).toHaveText(
    "Shown as on disk: a git filter applies; the stored content may differ.",
  );
  // Base is the commit: git's bytes, no note.
  await page.goto(server.base + "src/base/data.bin");
  await expect(page.locator("main .code-view")).toContainText("version");
  await expect(note).toHaveCount(0);
  // An edited file with no filter: no note.
  await page.goto(server.base + "src/head/src/A.hs");
  await expect(page.locator("main .code-view")).toContainText("a = 2");
  await expect(note).toHaveCount(0);
});

test("three dots, a long branch: merge base, cut label, no sideways scroll at 320px", async ({
  page,
  startServer,
}) => {
  // Its own repo: the branch must not leak into the shared one.
  const repo = await makeSidesProject();
  try {
    await threeDots(page, startServer, repo.root);
  } finally {
    await repo.remove();
  }
});

async function threeDots(
  page: Page,
  startServer: (o: { root: string; diff?: string }) => Promise<Server>,
  root: string,
): Promise<void> {
  const long = "feature/a-very-long-branch-name-that-goes-on-and-on-and-on";
  git(root, "branch", long, "HEAD");
  const server = await start(startServer, root, `HEAD~1...${long}`);
  await page.setViewportSize({ width: 320, height: 640 });
  await page.goto(server.base + "src/head/src/A.hs");
  await expect(page.locator("main .code-view")).toContainText("a = c + 1");
  await expect(items(page).nth(1)).toContainText("merge base");
  const label = page.locator(".frame-header .rev-cut-text").last();
  await expect(label).toHaveText(long);
  // Cut: narrower than its text.
  expect(await label.evaluate((e) => e.scrollWidth > e.clientWidth)).toBe(true);
  await noSideways(page);
  // Keyboard focus shows the tips: the cut label and the merge base.
  const tipShown = (sel: string) =>
    page
      .locator(sel)
      .evaluate((e) => getComputedStyle(e, "::after").display !== "none");
  // The head label (the third item).
  const cut = ".frame-header .rev-item:nth-child(3) .rev-cut";
  for (const sel of [cut, ".frame-header .rev-merge-base"]) {
    await expect(page.locator(sel)).toHaveAttribute("tabindex", "0");
    await expect(page.locator(sel)).toHaveAttribute("data-tip", /./);
    await page.keyboard.press("Shift");
    await page.locator(sel).focus();
    await expect.poll(() => tipShown(sel)).toBe(true);
  }
}

test("the header grows when it wraps and shrinks back; the sidebar follows", async ({
  page,
  startServer,
  sidesProject,
}) => {
  const server = await start(startServer, sidesProject.root, "HEAD~1..HEAD");
  await page.setViewportSize({ width: 1400, height: 800 });
  await page.goto(server.base + "docs/head/A");
  await expect(page.locator("main")).toContainText("Two.");
  const header = page.locator(".frame-header");
  const height = () =>
    header.evaluate((e) => Math.round(e.getBoundingClientRect().height));
  // The sidebar's max-height leaves room for the real header.
  const room = () =>
    page.evaluate(() => {
      const s = document.querySelector(".frame-sidebar-content");
      const h = document.querySelector(".frame-header");
      if (s === null || h === null) return Number.NaN;
      const max = Number.parseFloat(getComputedStyle(s).maxHeight);
      return Math.round(max + h.getBoundingClientRect().height);
    });
  const wide = await height();
  await expect.poll(room).toBe(800);
  await page.setViewportSize({ width: 400, height: 800 });
  await expect.poll(height).toBeGreaterThan(wide);
  await page.setViewportSize({ width: 1400, height: 800 });
  await expect.poll(height).toBe(wide);
  await expect.poll(room).toBe(800);
});

for (const t of themes) {
  test(`axe on a diff page, ${t.scheme} theme`, async ({
    page,
    startServer,
    editsProject,
  }) => {
    const server = await start(startServer, editsProject.root, "HEAD");
    await page.emulateMedia({ colorScheme: t.scheme });
    await page.goto(server.base + "src/head/data.bin");
    await expect(page.locator("main .file-note")).toBeVisible();
    await expect(items(page).first()).toBeVisible();
    await expectNoAxeViolations(page, `a diff page (${t.scheme})`);
  });
}
