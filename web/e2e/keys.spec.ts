// Single-key shortcuts (T13; D-297, D-298, SPEC §5.3): each key, the ?
// overlay, the on/off switch and its memory, the Docs / Source switch.
// Keyboard first, focus by .focus() (WebKit skips links on Tab), and
// focus asserted rather than the selection (WebKit drops it when focus
// moves).
import type { Locator, Page } from "@playwright/test";
import {
  expectNoAxeViolations as axe,
  noSideways,
  themes,
} from "./support/a11y";
import { copy, recordCopies } from "./support/copy";
import { expect, test } from "./support/fixtures";
import { lineOf, linksMainHs, shapesLine } from "./support/project";

const mainUrl = "src/tree/Links/Main.hs";
const shapesUrl = "docs/tree/Shapes";

const view = (page: Page): Locator => page.locator("main .code-view");
const cell = (page: Page, n: number): Locator =>
  view(page).locator(`.code-row[data-line="${n}"] .code-cell`);
const linkAt = (page: Page, n: number, name: string): Locator =>
  cell(page, n)
    .locator("a.code-link")
    .filter({ hasText: new RegExp(`^${name}$`) });
const line = (start: string) => lineOf(linksMainHs, start);
const dialog = (page: Page): Locator => page.getByRole("dialog");
const toggle = (page: Page): Locator =>
  dialog(page).getByRole("checkbox", { name: "Single-key shortcuts" });
const shortcuts = (page: Page): Locator =>
  page.getByRole("button", { name: "Shortcuts", exact: true });
const search = (page: Page): Locator =>
  page.locator(".search-box-header input");
const sections = (page: Page): Locator => page.locator("main section.decl");

async function openSource(page: Page, base: string): Promise<void> {
  await page.goto(base + mainUrl);
  await expect(view(page).locator("a.code-link").first()).toBeAttached();
  await expect(page.locator("main .code-outline")).toBeAttached();
}

async function openDocs(page: Page, base: string): Promise<void> {
  await page.goto(base + shapesUrl);
  await expect(sections(page).first()).toBeVisible();
}

// The cursor on a link: a click at column 0, then → until it.
async function keyTo(page: Page, n: number, name: string): Promise<Locator> {
  await cell(page, n).click({ position: { x: 1, y: 5 } });
  const a = linkAt(page, n, name);
  for (let i = 0; i < 8; i++) {
    await page.keyboard.press("ArrowRight");
    if (await a.evaluate((el) => el === document.activeElement)) break;
  }
  await expect(a).toBeFocused();
  return a;
}

function prefsSaved(page: Page): Promise<unknown> {
  return page.waitForResponse(
    (r) => r.url().endsWith("/api/prefs") && r.request().method() === "POST",
  );
}

test("/ focuses the search box; keys type there", async ({ page, server }) => {
  await page.goto(server.base);
  await page.keyboard.press("/");
  await expect(search(page)).toBeFocused();
  await page.keyboard.type("j?");
  await expect(search(page)).toHaveValue("j?");
  await expect(dialog(page)).toBeHidden();
});

test("?: the overlay, focus on the switch, Tab wraps, Esc puts focus back", async ({
  page,
  server,
}) => {
  await page.goto(server.base);
  const dark = page.getByRole("button", { name: "Dark", exact: true });
  await dark.focus();
  await page.keyboard.press("?");
  await expect(dialog(page)).toBeVisible();
  await expect(
    dialog(page).getByRole("heading", { name: "Keyboard shortcuts" }),
  ).toBeVisible();
  await expect(toggle(page)).toBeFocused();
  await expect(toggle(page)).toBeChecked();
  await expect(dialog(page).locator("tbody tr")).toHaveCount(13);
  await expect(dialog(page)).toContainText("Copy the name or path");
  const close = dialog(page).getByRole("button", { name: "Close" });
  // Tab and Shift+Tab stay inside.
  await page.keyboard.press("Shift+Tab");
  await expect(close).toBeFocused();
  await page.keyboard.press("Tab");
  await expect(toggle(page)).toBeFocused();
  // Keys do nothing while it's open.
  await page.keyboard.press("/");
  await expect(toggle(page)).toBeFocused();
  await page.keyboard.press("Escape");
  await expect(dialog(page)).toBeHidden();
  await expect(dark).toBeFocused();
  // The header button opens it too; Close puts focus back on it.
  await shortcuts(page).focus();
  await page.keyboard.press("Enter");
  await expect(toggle(page)).toBeFocused();
  await close.focus();
  await page.keyboard.press("Enter");
  await expect(dialog(page)).toBeHidden();
  await expect(shortcuts(page)).toBeFocused();
});

test("docs: j k move by declaration, s shows the source, Esc clears the selection", async ({
  page,
  startServer,
  docsProject,
}) => {
  const server = await startServer({ root: docsProject.root });
  await openDocs(page, server.base);
  await page.keyboard.press("j");
  await expect(sections(page).nth(0)).toBeFocused();
  await page.keyboard.press("j");
  await expect(sections(page).nth(1)).toBeFocused();
  await page.keyboard.press("k");
  await expect(sections(page).nth(0)).toBeFocused();
  // No hash change.
  await expect(page).toHaveURL(server.base + shapesUrl);
  const toggleSource = sections(page)
    .nth(0)
    .locator("button.decl-source-toggle");
  await expect(toggleSource).toHaveAttribute("aria-expanded", "false");
  await page.keyboard.press("s");
  await expect(toggleSource).toHaveAttribute("aria-expanded", "true");
  await page.keyboard.press("s");
  await expect(toggleSource).toHaveAttribute("aria-expanded", "false");
  // Modifiers: never.
  await page.keyboard.press("Control+j");
  await page.keyboard.press("Alt+j");
  await expect(sections(page).nth(0)).toBeFocused();
  // Esc with nothing to close: the selection goes.
  await page.evaluate(() => {
    const h1 = document.querySelector("main h1");
    if (h1 !== null) getSelection()?.selectAllChildren(h1);
  });
  await page.keyboard.press("Escape");
  expect(await page.evaluate(() => getSelection()?.isCollapsed)).toBe(true);
});

test("docs: 2 goes to the declaration's source, 1 comes back to it", async ({
  page,
  startServer,
  docsProject,
}) => {
  const server = await startServer({ root: docsProject.root });
  await openDocs(page, server.base);
  const nav = page.getByRole("navigation", { name: "View" });
  await expect(nav.getByRole("link", { name: "Docs" })).toHaveAttribute(
    "aria-current",
    "page",
  );
  await page.keyboard.press("j");
  await expect(sections(page).nth(0)).toBeFocused();
  await page.keyboard.press("2");
  const n = shapesLine("data Shape");
  await expect(page).toHaveURL(server.base + `src/tree/Shapes.hs#L${n}`);
  await expect(
    view(page).locator(`.code-row[data-line="${n}"] .code-cell`),
  ).toBeFocused();
  await expect(nav.getByRole("link", { name: "Source" })).toHaveAttribute(
    "aria-current",
    "page",
  );
  await expect(page.locator("main .code-outline")).toBeAttached();
  await page.keyboard.press("1");
  await expect(page).toHaveURL(new RegExp(`${shapesUrl}#type(%3A|:)Shape$`));
  await expect(sections(page).nth(0)).toBeFocused();
});

test("docs: g r goes to the references of the focused declaration", async ({
  page,
  startServer,
  docsProject,
}) => {
  const server = await startServer({ root: docsProject.root });
  await openDocs(page, server.base);
  await page.keyboard.press("j");
  await expect(sections(page).nth(0)).toBeFocused();
  await page.keyboard.press("g");
  await page.keyboard.press("r");
  await expect(page).toHaveURL(
    server.base + "refs/tree/Shapes?decl=type%3AShape",
  );
});

test("source: j k by outline, f finds, g d follows, g r opens the refs page", async ({
  page,
  startServer,
  linksProject,
}) => {
  const server = await startServer({ root: linksProject.root });
  await openSource(page, server.base);
  const p = line("data P");
  await cell(page, p).click({ position: { x: 1, y: 5 } });
  await expect(cell(page, p)).toBeFocused();
  await page.keyboard.press("j");
  await expect(cell(page, line("data Q"))).toBeFocused();
  await page.keyboard.press("k");
  await expect(cell(page, p)).toBeFocused();
  // f: the find bar, focus in it.
  await page.keyboard.press("f");
  const box = page.getByRole("textbox", { name: "Find" });
  await expect(box).toBeFocused();
  await page.keyboard.type("j");
  await expect(box).toHaveValue("j");
  await page.keyboard.press("Escape");
  await expect(page.locator("main .code-find-bar")).toBeHidden();
  // g d on area: its definition, in another file.
  const n = line("    go ys");
  await keyTo(page, n, "area");
  await page.keyboard.press("g");
  await page.keyboard.press("d");
  await expect(page).toHaveURL(/src\/tree\/Links\/Types\.hs#L\d+$/);
  await page.goBack();
  await expect(page).toHaveURL(server.base + mainUrl);
  await openSource(page, server.base);
  await keyTo(page, n, "area");
  await page.keyboard.press("g");
  await page.keyboard.press("r");
  await expect(page).toHaveURL(
    server.base + "refs/tree/Links.Types?decl=val%3Aarea",
  );
});

test("source: g then a pause does nothing; no link says so", async ({
  page,
  startServer,
  linksProject,
}) => {
  const server = await startServer({ root: linksProject.root });
  await openSource(page, server.base);
  const n = line("    go ys");
  await keyTo(page, n, "area");
  await page.keyboard.press("g");
  await page.waitForTimeout(1300);
  await page.keyboard.press("d");
  await page.waitForTimeout(300);
  await expect(page).toHaveURL(server.base + mainUrl);
  // On an empty line: nothing to follow.
  await cell(page, 3).click({ position: { x: 1, y: 5 } });
  await page.keyboard.press("g");
  await page.keyboard.press("d");
  await expect(page.locator(".keys-status")).toHaveText("No link here.");
  await expect(page).toHaveURL(server.base + mainUrl);
});

test("source: Esc closes the hover card first, then the find bar", async ({
  page,
  startServer,
  linksProject,
}) => {
  const server = await startServer({ root: linksProject.root });
  await openSource(page, server.base);
  await page.keyboard.press("f");
  const bar = page.locator("main .code-find-bar");
  await expect(bar).toBeVisible();
  const n = line("    go ys");
  const area = await keyTo(page, n, "area");
  const card = page.locator("main .code-card");
  await expect(card).toBeVisible();
  await page.keyboard.press("Escape");
  await expect(card).toBeHidden();
  await expect(bar).toBeVisible();
  await expect(area).toBeFocused();
  await page.keyboard.press("Escape");
  await expect(bar).toBeHidden();
  // 1: the docs of the module.
  await page.keyboard.press("1");
  await expect(page).toHaveURL(/docs\/tree\/Links\.Main#/);
});

test("source: Esc ends Ctrl+A at once, a copy right after gets nothing", async ({
  page,
  startServer,
  linksProject,
}) => {
  const server = await startServer({ root: linksProject.root });
  await openSource(page, server.base);
  await recordCopies(page);
  await cell(page, 3).click({ position: { x: 1, y: 5 } });
  await page.keyboard.press("ControlOrMeta+a");
  expect(await copy(page)).toBe(linksMainHs);
  await page.keyboard.press("Escape");
  expect(await copy(page)).toBeNull();
});

test("a pointer card under the overlay: Esc closes the overlay", async ({
  page,
  startServer,
  linksProject,
}) => {
  const server = await startServer({ root: linksProject.root });
  await openSource(page, server.base);
  const a = linkAt(page, line("    go ys"), "area");
  const b = await a.boundingBox();
  if (b === null) throw new Error("no link box");
  await page.mouse.move(b.x + b.width / 2, b.y + b.height / 2, { steps: 3 });
  const card = page.locator("main .code-card");
  await expect(card).toBeVisible();
  await shortcuts(page).focus();
  await page.keyboard.press("Enter");
  await expect(toggle(page)).toBeFocused();
  await page.keyboard.press("Escape");
  await expect(dialog(page)).toBeHidden();
  await expect(shortcuts(page)).toBeFocused();
});

test("refs page: Docs and Source links, 2 goes to the binding", async ({
  page,
  startServer,
  linksProject,
}) => {
  const server = await startServer({ root: linksProject.root });
  await page.goto(server.base + "refs/tree/Links.Types?decl=val%3Aarea");
  await expect(page.locator("main .refs-summary")).toBeVisible();
  const nav = page.getByRole("navigation", { name: "View" });
  await expect(nav.getByRole("link", { name: "Docs" })).toHaveAttribute(
    "href",
    `/${server.token}/docs/tree/Links.Types#val%3Aarea`,
  );
  await expect(nav.getByRole("link")).toHaveCount(2);
  await expect(nav.locator("[aria-current]")).toHaveCount(0);
  await page.keyboard.press("2");
  await expect(page).toHaveURL(/src\/tree\/Links\/Types\.hs#L\d+$/);
});

test("a file with no module: Docs is not available", async ({
  page,
  server,
}) => {
  await page.goto(server.base + "src/tree/README.md");
  const nav = page.getByRole("navigation", { name: "View" });
  await expect(nav.locator(".view-missing")).toHaveText("Docs (not available)");
  await page.keyboard.press("1");
  await expect(page.locator(".keys-status")).toHaveText(
    "No docs for this page.",
  );
});

test("off: the keys do nothing, remembered over a reload and a restart", async ({
  page,
  startServer,
  docsProject,
}) => {
  const a = await startServer({ root: docsProject.root });
  await openDocs(page, a.base);
  await shortcuts(page).click();
  const saved = prefsSaved(page);
  await toggle(page).uncheck();
  await saved;
  await expect(dialog(page)).toContainText("Off:");
  await page.keyboard.press("Escape");
  await expect(dialog(page)).toBeHidden();
  await page.keyboard.press("j");
  await expect(sections(page).nth(0)).not.toBeFocused();
  await page.keyboard.press("?");
  await expect(dialog(page)).toBeHidden();

  await page.reload();
  await expect(sections(page).first()).toBeVisible();
  await page.keyboard.press("/");
  await expect(search(page)).not.toBeFocused();
  await a.stop();

  const b = await startServer({ root: docsProject.root });
  await openDocs(page, b.base);
  await page.keyboard.press("j");
  await expect(sections(page).nth(0)).not.toBeFocused();
  await shortcuts(page).click();
  await expect(toggle(page)).not.toBeChecked();
  const on = prefsSaved(page);
  await toggle(page).check();
  await on;
  await page.keyboard.press("Escape");
  await page.keyboard.press("j");
  await expect(sections(page).nth(0)).toBeFocused();
});

for (const t of themes) {
  test(`axe: the overlay and the header, ${t.scheme} theme`, async ({
    page,
    startServer,
    docsProject,
  }) => {
    const server = await startServer({ root: docsProject.root });
    await page.emulateMedia({ colorScheme: t.scheme });
    await openDocs(page, server.base);
    await axe(page, `docs page header, ${t.scheme}`);
    await page.keyboard.press("?");
    await expect(dialog(page)).toBeVisible();
    await axe(page, `the ? overlay, ${t.scheme}`);
    await toggle(page).uncheck();
    await expect(dialog(page)).toContainText("Off:");
    await axe(page, `the ? overlay, off, ${t.scheme}`);
  });
}

test("320 px: the header and the overlay fit", async ({
  page,
  startServer,
  docsProject,
}) => {
  await page.setViewportSize({ width: 320, height: 640 });
  const server = await startServer({ root: docsProject.root });
  await openDocs(page, server.base);
  await noSideways(page);
  await page.keyboard.press("?");
  await expect(dialog(page)).toBeVisible();
  await noSideways(page);
  const box = await dialog(page).boundingBox();
  expect(box).not.toBeNull();
  expect((box?.x ?? 0) + (box?.width ?? 0)).toBeLessThanOrEqual(320);
});
