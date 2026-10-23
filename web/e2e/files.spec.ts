// The file tree and the file view, against the fixture project. The
// project has no .cabal file, so the sidebar starts on Modules: the
// tree tests switch to Files first.
import type { Locator, Page } from "@playwright/test";
import {
  expect,
  notFound404,
  showFiles,
  sidebar,
  tabKey,
  test,
} from "./support/fixtures";
import {
  deepHs,
  evilHs,
  mainHs,
  markupName,
  notices,
  oddFiles,
  readme,
  topFolders,
} from "./support/project";

function tree(page: Page): Locator {
  return sidebar(page);
}

function folder(page: Page, name: string): Locator {
  return tree(page).getByRole("button", { name, exact: true });
}

function fileLink(page: Page, name: string): Locator {
  return tree(page).getByRole("link", { name, exact: true });
}

function fileUrl(base: string, path: string): string {
  return base + "src/tree/" + path.split("/").map(encodeURIComponent).join("/");
}

// The code text, exactly. textContent, never innerHTML.
function code(page: Page): Promise<string | null> {
  return page.locator("main .file-code").evaluate((el) => el.textContent);
}

test("tree: top level", async ({ page, server }) => {
  await page.goto(server.base);
  await showFiles(page);
  const top = tree(page).locator(".file-tree > li > .tree-folder");
  await expect(top).toHaveText(topFolders);
  for (const b of await top.all()) {
    await expect(b).toHaveAttribute("aria-expanded", "false");
  }
  await expect(fileLink(page, "README.md")).toBeVisible();
  await expect(fileLink(page, "untracked.txt")).toBeVisible();
  await expect(tree(page)).not.toContainText("secret.log");
  await expect(fileLink(page, "link-to-main link")).toBeVisible();

  await folder(page, "xss").click();
  const markup = fileLink(page, markupName);
  await expect(markup).toBeVisible();
  expect(await markup.evaluate((el) => el.textContent)).toBe(markupName);
  await expect(tree(page).locator("b")).toHaveCount(0);

  await folder(page, "vendor").click();
  await expect(fileLink(page, "nested submodule")).toBeVisible();
});

test("tree: keyboard", async ({ page, server, browserName }) => {
  await page.goto(server.base);
  await showFiles(page);
  const src = folder(page, "src");
  await src.focus();
  await page.keyboard.press("Enter");
  await expect(src).toHaveAttribute("aria-expanded", "true");
  await page.keyboard.press(" ");
  await expect(src).toHaveAttribute("aria-expanded", "false");
  await page.keyboard.press("Enter");
  await expect(src).toHaveAttribute("aria-expanded", "true");
  // Tab: into src, past the Data folder, to Main.hs.
  await page.keyboard.press(tabKey(browserName));
  await expect(folder(page, "Data")).toBeFocused();
  await page.keyboard.press(tabKey(browserName));
  await expect(fileLink(page, "Main.hs")).toBeFocused();
});

test("open a text file", async ({ page, server }) => {
  await page.goto(server.base);
  await showFiles(page);
  await folder(page, "src").click();
  await fileLink(page, "Main.hs").click();
  await expect(page).toHaveURL(server.base + "src/tree/src/Main.hs");
  await expect(page.locator("main h1")).toHaveText("src/Main.hs");
  await expect(page).toHaveTitle(/src\/Main\.hs/);
  await expect.poll(() => code(page)).toBe(mainHs);
  expect(
    await page.locator("main .file-gutter").evaluate((el) => el.textContent),
  ).toBe("1\n2\n3\n4\n5");
  await expect(fileLink(page, "Main.hs")).toHaveAttribute(
    "aria-current",
    "page",
  );
});

test("markup in a file is text", async ({ page, server }) => {
  await page.goto(fileUrl(server.base, "xss/Evil.hs"));
  await expect.poll(() => code(page)).toBe(evilHs);
  expect(await page.evaluate(() => window.__pwned)).toBeUndefined();
  await expect(page.locator("main img, main script")).toHaveCount(0);
});

test("notices: binary, too large, nested repo", async ({ page, server }) => {
  const cases: [string, string][] = [
    ["data/blob.bin", notices.binary],
    ["data/big.txt", notices.tooLarge],
    ["vendor/nested", notices.nested],
  ];
  for (const [path, text] of cases) {
    await page.goto(fileUrl(server.base, path));
    await expect(page.locator("main p:not(.project-notice)")).toHaveText(text);
  }
});

test("symlinks", async ({ page, server }) => {
  await page.goto(fileUrl(server.base, "link-to-main"));
  const target = page.locator("main").getByRole("link", {
    name: "src/Main.hs",
  });
  await expect(target).toBeVisible();
  await target.click();
  await expect(page).toHaveURL(server.base + "src/tree/src/Main.hs");
  await expect(page.locator("main h1")).toBeFocused();
  await expect.poll(() => code(page)).toBe(mainHs);

  await page.goto(fileUrl(server.base, "dangling"));
  await expect(page.locator("main p code")).toHaveText("nowhere");
  await expect(page.locator("main a")).toHaveCount(0);
});

for (const [path, text] of Object.entries(oddFiles)) {
  test(`odd name: ${path}`, async ({ page, server }) => {
    await page.goto(server.base);
    await showFiles(page);
    await folder(page, "odd").click();
    const name = path.slice("odd/".length);
    await fileLink(page, name).click();
    await expect(page).toHaveURL(fileUrl(server.base, path));
    await expect(page.locator("main h1")).toHaveText(path);
    await expect.poll(() => code(page)).toBe(text);
    await page.reload();
    await expect(page.locator("main h1")).toHaveText(path);
    await expect.poll(() => code(page)).toBe(text);
  });
}

test("not found", async ({ page, server, guard }) => {
  guard.allow(notFound404);
  await page.goto(server.base + "src/tree/no/such.hs");
  await expect(page.locator("main h1")).toHaveText(
    "Not found in this snapshot",
  );
});

test("back and forward", async ({ page, server }) => {
  const current = tree(page).locator(".file-tree [aria-current=page]");
  await page.goto(server.base);
  await showFiles(page);
  await folder(page, "src").click();
  await fileLink(page, "Main.hs").click();
  await expect(page.locator("main h1")).toHaveText("src/Main.hs");
  await fileLink(page, "README.md").click();
  await expect(page.locator("main h1")).toHaveText("README.md");
  await expect.poll(() => code(page)).toBe(readme);
  await expect(current).toHaveText("README.md");

  await page.goBack();
  await expect(page).toHaveURL(server.base + "src/tree/src/Main.hs");
  await expect(page.locator("main h1")).toHaveText("src/Main.hs");
  await expect(current).toHaveText("Main.hs");

  await page.goForward();
  await expect(page.locator("main h1")).toHaveText("README.md");
  await expect(current).toHaveText("README.md");

  await page.goBack();
  await page.goBack();
  await expect(page).toHaveURL(server.base);
  await expect(page.locator("main")).toContainText(
    "Pick a file in the sidebar.",
  );
  await expect(current).toHaveCount(0);
});

test("deep link in a new page", async ({ context, server }) => {
  const page = await context.newPage();
  await page.goto(fileUrl(server.base, "src/Data/Deep/Nested/Mod.hs"));
  await showFiles(page);
  for (const name of ["src", "Data", "Deep", "Nested"]) {
    await expect(folder(page, name)).toHaveAttribute("aria-expanded", "true");
  }
  await expect(fileLink(page, "Mod.hs")).toHaveAttribute(
    "aria-current",
    "page",
  );
  await expect(page.locator("main h1")).toHaveText(
    "src/Data/Deep/Nested/Mod.hs",
  );
  await expect.poll(() => code(page)).toBe(deepHs);
});
