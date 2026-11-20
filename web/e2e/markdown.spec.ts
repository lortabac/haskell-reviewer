// Markdown files (D-327, D-328): coloured in the source view, rendered
// as their docs view, links into the tool.
import { expect, test } from "./support/fixtures";

test("source view: coloured", async ({ page, server }) => {
  await page.goto(server.base + "src/tree/README.md");
  const rows = page.locator("main .code-rows");
  await expect(rows.locator(".hl-keyword").first()).toHaveText("# Fixture");
  await expect(rows.locator(".hl-typeCon").first()).toHaveText(
    "[Main](src/Main.hs#L1)",
  );
  // The Haskell block: GHC's tokens.
  await expect(rows.locator(".hl-variable").first()).toHaveText("main");
});

test("Docs: the file rendered; 1 and 2 switch", async ({ page, server }) => {
  await page.goto(server.base + "src/tree/README.md");
  const nav = page.getByRole("navigation", { name: "View" });
  await expect(nav.getByRole("link", { name: "Docs" })).toBeVisible();
  // The view is there: its keys work.
  await expect(
    page.locator("main .code-rows .hl-keyword").first(),
  ).toBeVisible();
  await page.keyboard.press("1");
  await expect(page).toHaveURL(/md\/tree\/README\.md$/);
  const main = page.locator("main");
  await expect(main.locator("h1")).toHaveText("README.md");
  await expect(main.getByRole("heading", { name: "Fixture" })).toBeVisible();
  await expect(main.locator("pre .hl-variable").first()).toHaveText("main");
  await expect(nav.locator("[aria-current=page]")).toHaveText("Docs");
  await page.keyboard.press("2");
  await expect(page).toHaveURL(/src\/tree\/README\.md$/);
});

test("Docs: links to a file line and to a heading", async ({
  page,
  server,
}) => {
  await page.goto(server.base + "md/tree/README.md");
  const main = page.locator("main");
  await main.getByRole("link", { name: "usage" }).click();
  await expect(page).toHaveURL(/md\/tree\/README\.md#usage$/);
  await expect(page.locator("#md-usage")).toBeFocused();
  await main.getByRole("link", { name: "Main" }).click();
  await expect(page).toHaveURL(/src\/tree\/src\/Main\.hs#L1$/);
  await expect(page.locator("main h1")).toHaveText("src/Main.hs");
});

test("not Markdown, or no such file: not found", async ({ page, server }) => {
  await page.goto(server.base + "md/tree/src/Main.hs");
  await expect(page.locator("main")).toContainText("not found", {
    ignoreCase: true,
  });
});
