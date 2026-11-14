// Copy (T14; D-299, SPEC §5.7): the buttons, y, Copy location in the
// header. Firefox and WebKit can't read the clipboard, so a stub
// records what goes to writeText; Chromium also reads the real one.
// Keyboard first, focus by .focus() (WebKit skips links on Tab), and
// focus asserted rather than the selection (WebKit drops it when focus
// moves).
import type { Locator, Page } from "@playwright/test";
import { expectNoAxeViolations as axe, themes } from "./support/a11y";
import {
  copy,
  grantClipboard,
  nextWrite,
  recordCopies,
  select,
  stubWriteText,
  written,
} from "./support/copy";
import { expect, showFiles, test } from "./support/fixtures";
import { lineOf, linksMainHs, shapesHs } from "./support/project";

test.beforeEach(async ({ context, browserName }) => {
  await grantClipboard(context, browserName);
});

const view = (page: Page): Locator => page.locator("main .code-view");
const cell = (page: Page, n: number): Locator =>
  view(page).locator(`.code-row[data-line="${n}"] .code-cell`);
const line = (start: string) => lineOf(linksMainHs, start);
const shapesLine = (start: string) => lineOf(shapesHs, start);
const status = (page: Page): Locator => page.locator(".keys-status");
const headerCopy = (page: Page): Locator =>
  page.getByRole("button", { name: "Copy location", exact: true });
const section = (page: Page, head: RegExp): Locator =>
  page.locator("main section.decl").filter({
    has: page.locator(".decl-head").filter({ hasText: head }),
  });

async function docs(
  page: Page,
  startServer: (o: { root: string }) => Promise<{ base: string }>,
  root: string,
): Promise<void> {
  await stubWriteText(page);
  const server = await startServer({ root });
  await page.goto(server.base + "docs/tree/Shapes");
  await expect(page.locator("main section.decl").first()).toBeVisible();
}

async function source(
  page: Page,
  startServer: (o: { root: string }) => Promise<{ base: string }>,
  root: string,
): Promise<void> {
  await stubWriteText(page);
  const server = await startServer({ root });
  await page.goto(server.base + "src/tree/Links/Main.hs");
  await expect(view(page).locator("a.code-link").first()).toBeAttached();
}

// The cursor on a link: a click at column 0, then → until it.
async function keyTo(page: Page, n: number, name: string): Promise<void> {
  await cell(page, n).click({ position: { x: 1, y: 5 } });
  const a = cell(page, n)
    .locator("a.code-link")
    .filter({ hasText: new RegExp(`^${name}$`) });
  for (let i = 0; i < 8; i++) {
    await page.keyboard.press("ArrowRight");
    if (await a.evaluate((el) => el === document.activeElement)) break;
  }
  await expect(a).toBeFocused();
}

test("docs buttons: module, path, declaration, instance; Copied. is polite", async ({
  page,
  startServer,
  docsProject,
}) => {
  await docs(page, startServer, docsProject.root);
  const text = (b: Locator) => nextWrite(page, () => b.click());
  const module = page
    .locator("main")
    .getByRole("button", { name: "Copy module name Shapes" });
  // Hidden until hover or focus.
  await expect(module).toHaveCSS("opacity", "0");
  await page.locator("main h1").hover();
  await expect(module).toHaveCSS("opacity", "1");
  expect(await text(module)).toBe("Shapes");
  await expect(status(page)).toHaveText(/^Copied\./);
  await expect(status(page)).toHaveAttribute("aria-live", "polite");
  expect(
    await text(page.getByRole("button", { name: "Copy path Shapes.hs" })),
  ).toBe("Shapes.hs");
  const shape = section(page, /^data Shape/);
  expect(await text(shape.getByRole("button", { name: "Copy Shape" }))).toBe(
    "Shapes.Shape",
  );
  const op = section(page, /<\+>/);
  expect(await text(op.getByRole("button", { name: "Copy <+>" }))).toBe(
    "Shapes.<+>",
  );
  // Instances: open the list of the first type that has some.
  await page
    .locator("main summary", { hasText: /^Instances/ })
    .first()
    .click();
  const inst = page
    .locator("main button[aria-label='Copy instance head']")
    .first();
  await expect(inst).toBeAttached();
  await inst.focus();
  expect(await text(inst)).toMatch(/^(data |type )?instance /);
});

test("docs: y on a declaration and on a nested item; nothing to copy", async ({
  page,
  startServer,
  docsProject,
}) => {
  await docs(page, startServer, docsProject.root);
  await section(page, /^data Shape/).focus();
  expect(await nextWrite(page, () => page.keyboard.press("y"))).toBe(
    "Shapes.Shape",
  );
  await expect(status(page)).toHaveText(/^Copied\./);
  const circle = page.locator("main li.subdecl").filter({ hasText: /Circle/ });
  await circle.first().focus();
  expect(await nextWrite(page, () => page.keyboard.press("y"))).toBe(
    "Shapes.Circle",
  );
  // Nested items have no button.
  await expect(circle.first().locator(".copy-button")).toHaveCount(0);
});

test("docs: Copy location, the file or the focused declaration", async ({
  page,
  startServer,
  docsProject,
}) => {
  await docs(page, startServer, docsProject.root);
  expect(await nextWrite(page, () => headerCopy(page).click())).toBe(
    "Shapes.hs",
  );
  await section(page, /^data Shape/).focus();
  expect(await nextWrite(page, () => headerCopy(page).click())).toBe(
    `Shapes.hs:${shapesLine("data Shape")}`,
  );
  // Keyboard: the header button has focus by then.
  await section(page, /^data Shape/).focus();
  await headerCopy(page).focus();
  expect(await nextWrite(page, () => page.keyboard.press("Enter"))).toBe(
    `Shapes.hs:${shapesLine("data Shape")}`,
  );
});

test("docs: a selection copied with Ctrl+C has no button words", async ({
  page,
  startServer,
  docsProject,
  browserName,
}) => {
  await docs(page, startServer, docsProject.root);
  await recordCopies(page);
  await page.evaluate(() => {
    const main = document.querySelector("main");
    if (main !== null) getSelection()?.selectAllChildren(main);
  });
  // The text of the selection, in every browser.
  const t = await page.evaluate(() => getSelection()?.toString() ?? "");
  expect(t).toContain("Shape");
  expect(t).not.toMatch(/\bcopy\b/);
  // What Ctrl+C really puts on the clipboard: Chromium can read it.
  if (browserName === "chromium") {
    await page.keyboard.press("ControlOrMeta+c");
    await expect
      .poll(() => page.evaluate(() => navigator.clipboard.readText()))
      .toContain("Shape");
    const real = await page.evaluate(() => navigator.clipboard.readText());
    expect(real).not.toMatch(/\bcopy\b/);
  } else {
    // The recorder sees the event; the app doesn't take docs copies.
    expect(await copy(page)).toBeNull();
  }
});

test("source: y on a declaration, an external name, a local; others fall back", async ({
  page,
  startServer,
  linksProject,
}) => {
  await source(page, startServer, linksProject.root);
  const y = () => nextWrite(page, () => page.keyboard.press("y"));
  const v1 = line("v1 =");
  await keyTo(page, v1, "area");
  expect(await y()).toBe("Links.Types.area");
  // A guess among several modules: not copied as a name, the place.
  await keyTo(page, line("up ="), "toUpper");
  expect(await y()).toBe(`Links/Main.hs:${line("up =")}`);
  const go = line("total xs");
  await keyTo(page, go, "go");
  expect(await y()).toBe(`Links/Main.hs:${go}`);
  // No token: the cursor line.
  await cell(page, line("import Data.Char")).click({
    position: { x: 1, y: 5 },
  });
  expect(await y()).toBe(`Links/Main.hs:${line("import Data.Char")}`);
});

test("source: Copy location, cursor, selection, triple click, select all", async ({
  page,
  startServer,
  linksProject,
}) => {
  await source(page, startServer, linksProject.root);
  const loc = () => nextWrite(page, () => headerCopy(page).click());
  const a = line("total ::");
  await cell(page, a).click({ position: { x: 1, y: 5 } });
  expect(await loc()).toBe(`Links/Main.hs:${a}`);
  await select(page, [a, 2], [a + 2, 3]);
  expect(await loc()).toBe(`Links/Main.hs:${a}-${a + 2}`);
  // A press in the header keeps the selection and the focus.
  expect(await page.evaluate(() => getSelection()?.isCollapsed)).toBe(false);
  expect(
    await page.evaluate(
      () => document.activeElement?.closest(".code-view") !== null,
    ),
  ).toBe(true);
  await select(page, [a, 2], [a, 4]);
  expect(await loc()).toBe(`Links/Main.hs:${a}`);
  await cell(page, a + 1).click({ clickCount: 3 });
  expect(await loc()).toBe(`Links/Main.hs:${a + 1}`);
  await cell(page, a).click({ position: { x: 1, y: 5 } });
  await page.keyboard.press("ControlOrMeta+a");
  const n = linksMainHs.replace(/\n$/, "").split("\n").length;
  expect(await loc()).toBe(`Links/Main.hs:1-${n}`);
});

test("file tree: a mouse button and y", async ({
  page,
  startServer,
  linksProject,
}) => {
  await stubWriteText(page);
  const server = await startServer({ root: linksProject.root });
  await page.goto(server.base);
  await showFiles(page);
  await page.getByRole("button", { name: "Links", exact: true }).click();
  const link = page.getByRole("link", { name: "Main.hs" });
  // Mouse only, and not in the accessibility tree.
  const b = page.locator("nav button[aria-label='Copy path Links/Main.hs']");
  await expect(b).toHaveAttribute("aria-hidden", "true");
  await expect(b).toHaveAttribute("tabindex", "-1");
  await link.hover();
  await expect(b).toHaveCSS("opacity", "1");
  expect(await nextWrite(page, () => b.click())).toBe("Links/Main.hs");
  await link.focus();
  expect(await nextWrite(page, () => page.keyboard.press("y"))).toBe(
    "Links/Main.hs",
  );
});

test("module tree: y copies the module name", async ({
  page,
  startServer,
  linksProject,
}) => {
  await stubWriteText(page);
  const server = await startServer({ root: linksProject.root });
  await page.goto(server.base);
  // A loose module: shown by its path, copied by its name.
  const link = page.locator(".module-tree a.tree-file", {
    hasText: /^Links\/Main\.hs$/,
  });
  const folder = page.getByRole("button", { name: "Implicit package" });
  if ((await folder.getAttribute("aria-expanded")) !== "true") {
    await folder.click();
  }
  await expect(link).toBeVisible();
  await link.focus();
  expect(await nextWrite(page, () => page.keyboard.press("y"))).toBe(
    "Links.Main",
  );
});

test("refs page: buttons, y, Copy location", async ({
  page,
  startServer,
  linksProject,
}) => {
  await stubWriteText(page);
  const server = await startServer({ root: linksProject.root });
  await page.goto(server.base + "refs/tree/Links.Types?decl=val%3Aarea");
  const first = page.locator("main .refs-items a").first();
  await expect(first).toBeVisible();
  expect(await nextWrite(page, () => page.keyboard.press("y"))).toBe(
    "Links.Types.area",
  );
  await first.focus();
  expect(await nextWrite(page, () => page.keyboard.press("y"))).toMatch(
    /^Links\/\w+\.hs:\d+$/,
  );
  await first.focus();
  await headerCopy(page).focus();
  expect(await nextWrite(page, () => page.keyboard.press("Enter"))).toMatch(
    /^Links\/\w+\.hs:\d+$/,
  );
  const group = page.locator("main .refs-group").first();
  await group.hover();
  expect(
    await nextWrite(page, () =>
      group.getByRole("button", { name: /^Copy module name/ }).click(),
    ),
  ).toMatch(/^Links\./);
  expect(
    await nextWrite(page, () =>
      page.getByRole("button", { name: "Copy area" }).click(),
    ),
  ).toBe("Links.Types.area");
});

test("a blocked clipboard: says so", async ({
  page,
  startServer,
  docsProject,
}) => {
  await stubWriteText(page, true);
  const server = await startServer({ root: docsProject.root });
  await page.goto(server.base + "docs/tree/Shapes");
  await expect(headerCopy(page)).toBeVisible();
  await headerCopy(page).click();
  await expect(status(page)).toHaveText(/^Couldn't copy/);
  expect(await written(page)).toEqual(["Shapes.hs"]);
});

test("the real clipboard gets the text", async ({
  page,
  startServer,
  docsProject,
  context,
  browserName,
}) => {
  test.skip(browserName !== "chromium", "clipboard permissions: Chromium");
  await context.grantPermissions(["clipboard-read", "clipboard-write"]);
  await docs(page, startServer, docsProject.root);
  await section(page, /^data Shape/).focus();
  await page.keyboard.press("y");
  await expect
    .poll(() => page.evaluate(() => navigator.clipboard.readText()))
    .toBe("Shapes.Shape");
});

for (const t of themes) {
  test(`axe: buttons shown, ${t.scheme} theme`, async ({
    page,
    startServer,
    docsProject,
  }) => {
    await page.emulateMedia({ colorScheme: t.scheme });
    await docs(page, startServer, docsProject.root);
    const b = section(page, /^data Shape/).getByRole("button", {
      name: "Copy Shape",
    });
    await b.focus();
    await expect(b).toHaveCSS("opacity", "1");
    await axe(page, `docs page with a copy button, ${t.scheme}`);
  });
}

test("nothing to copy: home page, and focus in the sidebar", async ({
  page,
  startServer,
  docsProject,
}) => {
  await docs(page, startServer, docsProject.root);
  await section(page, /^data Shape/).focus();
  // A folder button in the sidebar, not the view's current item.
  const side = page.getByRole("button", { name: "Files", exact: true });
  await side.focus();
  await page.keyboard.press("y");
  await expect(status(page)).toHaveText(/^Nothing to copy here\./);
  expect(await written(page)).toEqual([]);
  await page.goto(page.url().replace(/docs\/tree\/Shapes.*/, ""));
  await page.keyboard.press("y");
  await expect(status(page)).toHaveText(/^Nothing to copy here\./);
  expect(await written(page)).toEqual([]);
});

test("docs: y on an instance", async ({ page, startServer, docsProject }) => {
  await docs(page, startServer, docsProject.root);
  await page
    .locator("main summary", { hasText: /^Instances/ })
    .first()
    .click();
  const inst = page
    .locator("main section.decl")
    .filter({
      has: page.locator(":scope > button[aria-label='Copy instance head']"),
    })
    .first();
  await expect(inst).toBeVisible();
  await inst.focus();
  expect(await nextWrite(page, () => page.keyboard.press("y"))).toMatch(
    /^(data |type )?instance /,
  );
});

test("file view: the path button", async ({
  page,
  startServer,
  linksProject,
}) => {
  await source(page, startServer, linksProject.root);
  const b = page
    .locator("main")
    .getByRole("button", { name: "Copy path", exact: true });
  await expect(b).toHaveCSS("opacity", "0");
  await page.locator("main h1").hover();
  await expect(b).toHaveCSS("opacity", "1");
  expect(await nextWrite(page, () => b.click())).toBe("Links/Main.hs");
});

test("module tree: the hover button, rows stay one line", async ({
  page,
  startServer,
  linksProject,
}) => {
  await stubWriteText(page);
  const server = await startServer({ root: linksProject.root });
  await page.goto(server.base);
  await page.getByRole("button", { name: "Implicit package" }).click();
  const link = page.locator(".module-tree a.tree-file", {
    hasText: /^Links\/Main\.hs$/,
  });
  const li = link.locator("xpath=ancestor::li[1]");
  const b = link.locator("xpath=following-sibling::button");
  await expect(b).toHaveAttribute("aria-hidden", "true");
  await expect(b).toHaveCSS("opacity", "0");
  await link.hover();
  await expect(b).toHaveCSS("opacity", "1");
  expect(await nextWrite(page, () => b.click())).toBe("Links.Main");
  // A press on the button keeps focus on the link, so y still works.
  await link.focus();
  await b.click();
  expect(await nextWrite(page, () => page.keyboard.press("y"))).toBe(
    "Links.Main",
  );
  const [h1, h2] = await Promise.all([li.boundingBox(), link.boundingBox()]);
  expect(h1?.height).toBeLessThanOrEqual((h2?.height ?? 0) + 1);
});

test("file tree: rows stay one line", async ({
  page,
  startServer,
  linksProject,
}) => {
  const server = await startServer({ root: linksProject.root });
  await page.goto(server.base);
  await showFiles(page);
  await page.getByRole("button", { name: "Links", exact: true }).click();
  const link = page.getByRole("link", { name: "Main.hs" });
  const li = link.locator("xpath=ancestor::li[1]");
  const [a, b] = await Promise.all([li.boundingBox(), link.boundingBox()]);
  expect(a?.height).toBeLessThanOrEqual((b?.height ?? 0) + 1);
});

test("refs: no location without a focused row", async ({
  page,
  startServer,
  linksProject,
}) => {
  await stubWriteText(page);
  const server = await startServer({ root: linksProject.root });
  await page.goto(server.base + "refs/tree/Links.Types?decl=val%3Aarea");
  await expect(page.locator("main .refs-items a").first()).toBeVisible();
  await headerCopy(page).click();
  await expect(status(page)).toHaveText(/^No location here\./);
  expect(await written(page)).toEqual([]);
});

test("a module that doesn't parse: Copy location is the path", async ({
  page,
  startServer,
  docsProject,
}) => {
  await stubWriteText(page);
  const server = await startServer({ root: docsProject.root });
  await page.goto(server.base + "docs/tree/Broken");
  await expect(page.locator("main .docs-banner")).toBeVisible();
  expect(await nextWrite(page, () => headerCopy(page).click())).toBe(
    "Broken.hs",
  );
  await page.keyboard.press("y");
  await expect(status(page)).toHaveText(/^Copied\./);
});

test("Copy location forgets a declaration when focus went to the sidebar", async ({
  page,
  startServer,
  docsProject,
}) => {
  await docs(page, startServer, docsProject.root);
  await section(page, /^data Shape/).focus();
  await page.getByRole("button", { name: "Files", exact: true }).focus();
  await headerCopy(page).focus();
  expect(await nextWrite(page, () => page.keyboard.press("Enter"))).toBe(
    "Shapes.hs",
  );
});

test("buttons never cover text; the heading keeps its spacing", async ({
  page,
  startServer,
  docsProject,
}) => {
  await docs(page, startServer, docsProject.root);
  const sec = section(page, /^longSig/);
  await sec.scrollIntoViewIfNeeded();
  await sec.hover();
  const b = sec.locator(":scope > button.copy-button");
  await expect(b).toHaveCSS("opacity", "1");
  const hit = await sec.evaluate((el) => {
    const btn = el.querySelector(":scope > .copy-button");
    const head = el.querySelector(".decl-head");
    if (btn === null || head === null) return true;
    const r = btn.getBoundingClientRect();
    // The rects of the text itself (not the boxes around it).
    const w = document.createTreeWalker(head, NodeFilter.SHOW_TEXT);
    for (let t = w.nextNode(); t !== null; t = w.nextNode()) {
      const range = document.createRange();
      range.selectNodeContents(t);
      for (const x of range.getClientRects()) {
        if (
          x.width > 0 &&
          x.left < r.right &&
          x.right > r.left &&
          x.top < r.bottom &&
          x.bottom > r.top
        ) {
          return true;
        }
      }
    }
    return false;
  });
  expect(hit).toBe(false);
  // The h1: its box is the host's (no extra margin), and focus on the
  // heading itself does not show the button.
  const host = page.locator("main .copy-host").first();
  const h1 = page.locator("main h1");
  const [hb, h1b] = await Promise.all([host.boundingBox(), h1.boundingBox()]);
  expect(Math.abs((hb?.height ?? 0) - (h1b?.height ?? 0))).toBeLessThan(2);
  await page.mouse.move(0, 0);
  await h1.focus();
  await expect(host.locator("button")).toHaveCSS("opacity", "0");
});

test("axe: trees, file view, refs page, header; buttons shown", async ({
  page,
  startServer,
  linksProject,
}, testInfo) => {
  testInfo.setTimeout(90_000);
  const server = await startServer({ root: linksProject.root });
  for (const t of themes) {
    await page.emulateMedia({ colorScheme: t.scheme });
    await page.goto(server.base);
    // The sidebar remembers its view.
    await page.getByRole("button", { name: "Modules", exact: true }).click();
    await page.getByRole("button", { name: "Implicit package" }).click();
    const row = page.locator(".module-tree .tree-row").first();
    await row.hover();
    await expect(row.locator("button")).toHaveCSS("opacity", "1");
    await axe(page, `module tree, ${t.scheme}`);
    await showFiles(page);
    await page.getByRole("button", { name: "Links", exact: true }).click();
    const frow = page.locator(".file-tree .tree-row").first();
    await frow.hover();
    await axe(page, `file tree, ${t.scheme}`);
    await page.goto(server.base + "src/tree/Links/Main.hs");
    await page.locator("main h1").hover();
    await expect(headerCopy(page)).toBeVisible();
    await axe(page, `file view, ${t.scheme}`);
    await page.goto(server.base + "refs/tree/Links.Types?decl=val%3Aarea");
    await expect(page.locator("main .refs-items a").first()).toBeVisible();
    await page.locator("main .refs-group").first().hover();
    await axe(page, `refs page, ${t.scheme}`);
  }
});
