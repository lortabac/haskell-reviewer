// The diff layout pref (D-345): toolbar group, `u`, ?layout= override,
// config default, restart. fixtures/repos/diffview.sh, --diff HEAD~1..HEAD.
import type { Locator, Page } from "@playwright/test";
import { mkdir, writeFile } from "node:fs/promises";
import { join } from "node:path";
import { expect, test } from "./support/fixtures";
import type { Server } from "./support/server";
import { settle } from "./support/settle";

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
const split = (page: Page) => grid(page).locator(".diff-half").first();
const layoutGroup = (page: Page) => page.getByRole("group", { name: "Layout" });
const choice = (page: Page, name: string) =>
  layoutGroup(page).getByRole("button", { name, exact: true });

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

// Set up before the action that triggers the POST.
function prefsPostSaved(page: Page): Promise<unknown> {
  return page.waitForResponse(
    (r) => r.url().endsWith("/api/prefs") && r.request().method() === "POST",
  );
}

const savedLayout = (page: Page, server: Server): Promise<unknown> =>
  page.evaluate(async (url) => {
    const r = await fetch(url + "api/prefs");
    return ((await r.json()) as { diffLayout: string }).diffLayout;
  }, server.base);

async function writeConfig(configHome: string, toml: string): Promise<void> {
  const dir = join(configHome, "haskell-reviewer");
  await mkdir(dir, { recursive: true });
  await writeFile(join(dir, "config.toml"), toml);
}

const status = (page: Page) => page.locator(".keys-status");

test("the toggle saves, is aria-pressed, and split survives a restart", async ({
  page,
  startServer,
  diffviewProject,
}) => {
  const a = await start(startServer, diffviewProject.root);
  await open(page, a, "src/Pairs.hs");
  await expect(choice(page, "Unified")).toHaveAttribute("aria-pressed", "true");
  await expect(choice(page, "Side by side")).toHaveAttribute(
    "aria-pressed",
    "false",
  );
  await expect(split(page)).toHaveCount(0);
  const saved = prefsPostSaved(page);
  await choice(page, "Side by side").click();
  await saved;
  await expect(split(page)).toBeVisible();
  await expect(choice(page, "Side by side")).toHaveAttribute(
    "aria-pressed",
    "true",
  );
  await expect(choice(page, "Unified")).toHaveAttribute(
    "aria-pressed",
    "false",
  );
  await settle(page);
  await a.stop();

  // Same state dir, no ?layout=: split comes from ui.json.
  const b = await start(startServer, diffviewProject.root);
  await open(page, b, "src/Pairs.hs");
  await expect(split(page)).toBeVisible();
  await expect(choice(page, "Side by side")).toHaveAttribute(
    "aria-pressed",
    "true",
  );
});

test("u toggles and saves", async ({ page, startServer, diffviewProject }) => {
  const server = await start(startServer, diffviewProject.root);
  await open(page, server, "src/Pairs.hs");
  const first = prefsPostSaved(page);
  await page.keyboard.press("u");
  await first;
  await expect(split(page)).toBeVisible();
  await expect(status(page)).toHaveText(/^Side by side\.\s*$/);
  await expect(choice(page, "Side by side")).toHaveAttribute(
    "aria-pressed",
    "true",
  );
  expect(await savedLayout(page, server)).toBe("split");
  const second = prefsPostSaved(page);
  await page.keyboard.press("u");
  await second;
  await expect(split(page)).toHaveCount(0);
  await expect(status(page)).toHaveText(/^Unified\.\s*$/);
  expect(await savedLayout(page, server)).toBe("unified");
});

test("u too narrow: the pref flips, the status says why", async ({
  page,
  startServer,
  diffviewProject,
}) => {
  const server = await start(startServer, diffviewProject.root);
  await open(page, server, "src/Pairs.hs", 900);
  const saved = prefsPostSaved(page);
  await page.keyboard.press("u");
  await saved;
  await expect(status(page)).toHaveText(/^Side by side\.\s*$/);
  // The note says why; the key status doesn't repeat it.
  await expect(page.locator(".diff-text .file-note")).toHaveText(
    "Side by side needs a wider window: showing unified.",
  );
  await expect(split(page)).toHaveCount(0);
  await expect(choice(page, "Side by side")).toHaveAttribute(
    "aria-pressed",
    "true",
  );
  expect(await savedLayout(page, server)).toBe("split");
});

test("?layout=unified over the pref split: unified, the pref stays split", async ({
  page,
  startServer,
  diffviewProject,
}) => {
  const server = await start(startServer, diffviewProject.root);
  await open(page, server, "src/Pairs.hs");
  const saved = prefsPostSaved(page);
  await choice(page, "Side by side").click();
  await saved;
  await expect(split(page)).toBeVisible();
  await open(page, server, "src/Pairs.hs?layout=unified");
  await expect(split(page)).toHaveCount(0);
  await expect(choice(page, "Unified")).toHaveAttribute("aria-pressed", "true");
  expect(await savedLayout(page, server)).toBe("split");
});

test("a click on a ?layout= page drops the param, no new history entry", async ({
  page,
  startServer,
  diffviewProject,
  xdg,
}) => {
  // The pref is split too, so the click has something to save.
  await writeConfig(xdg.configHome, '[ui]\ndiff-layout = "split"\n');
  const server = await start(startServer, diffviewProject.root);
  await open(page, server, "src/Pairs.hs?layout=split");
  await expect(split(page)).toBeVisible();
  const before = await page.evaluate(() => history.length);
  const saved = prefsPostSaved(page);
  await choice(page, "Unified").click();
  await expect(split(page)).toHaveCount(0);
  await expect.poll(() => page.url()).not.toContain("layout=");
  expect(await page.evaluate(() => history.length)).toBe(before);
  await saved;
  expect(await savedLayout(page, server)).toBe("unified");
});

test("u on a ?layout= page: says it, drops the param, no new history entry", async ({
  page,
  startServer,
  diffviewProject,
}) => {
  const server = await start(startServer, diffviewProject.root);
  await open(page, server, "src/Pairs.hs?layout=split");
  await expect(split(page)).toBeVisible();
  const before = await page.evaluate(() => history.length);
  await page.keyboard.press("u");
  await expect(split(page)).toHaveCount(0);
  await expect.poll(() => page.url()).not.toContain("layout=");
  await expect(status(page)).toHaveText(/^Unified\.\s*$/);
  expect(await page.evaluate(() => history.length)).toBe(before);
  // Back to split by the key; the pref is unified, so nothing to drop.
  const saved = prefsPostSaved(page);
  await page.keyboard.press("u");
  await saved;
  await expect(status(page)).toHaveText(/^Side by side\.\s*$/);
});

test("config diff-layout = split is the default; a click wins over it", async ({
  page,
  startServer,
  diffviewProject,
  xdg,
}) => {
  await writeConfig(xdg.configHome, '[ui]\ndiff-layout = "split"\n');
  const a = await start(startServer, diffviewProject.root);
  await open(page, a, "src/Pairs.hs");
  await expect(split(page)).toBeVisible();
  expect(await savedLayout(page, a)).toBe("split");
  const saved = prefsPostSaved(page);
  await choice(page, "Unified").click();
  await saved;
  await expect(split(page)).toHaveCount(0);
  await settle(page);
  await a.stop();

  const b = await start(startServer, diffviewProject.root);
  await open(page, b, "src/Pairs.hs");
  await expect(split(page)).toHaveCount(0);
  await expect(choice(page, "Unified")).toHaveAttribute("aria-pressed", "true");
});

test("the ? overlay lists u", async ({
  page,
  startServer,
  diffviewProject,
}) => {
  const server = await start(startServer, diffviewProject.root);
  await open(page, server, "src/Pairs.hs");
  await page.keyboard.press("?");
  const dialog = page.getByRole("dialog");
  await expect(dialog).toBeVisible();
  const row = dialog.locator("tbody tr", {
    hasText: "Unified or side by side",
  });
  await expect(row).toHaveCount(1);
  await expect(row.locator("kbd")).toHaveText("u");
  await expect(row).toContainText("Unified / Side by side buttons");
});
