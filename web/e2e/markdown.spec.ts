// Markdown files (D-327, D-328): coloured in the source view, rendered
// as their docs view, links into the tool.
import { execFileSync } from "node:child_process";
import { appendFile, cp, mkdtemp, rm, writeFile } from "node:fs/promises";
import { tmpdir } from "node:os";
import { join } from "node:path";
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

// A copy of edits.sh's repo where *.md is filtered, and an edited
// notes.md: read from disk, so the rendered page says so (D-343).
test("filtered: the rendered page has the note", async ({
  page,
  startServer,
  editsProject,
}) => {
  const root = await mkdtemp(join(tmpdir(), "hr-e2e-mdfilter-"));
  try {
    await cp(editsProject.root, root, { recursive: true });
    await appendFile(join(root, ".gitattributes"), "*.md filter=lfs -text\n");
    // Committed, then edited: git can't tell without the filter, so the
    // file is read from disk.
    await writeFile(join(root, "notes.md"), "pointer\n");
    const git = (...args: string[]) =>
      execFileSync("git", ["-C", root, ...args], {
        stdio: "ignore",
        // As support/project.ts: no user or system git config.
        env: {
          ...process.env,
          GIT_CONFIG_GLOBAL: "/dev/null",
          GIT_CONFIG_NOSYSTEM: "1",
        },
      });
    git("add", "-A");
    git(
      "-c",
      "user.name=t",
      "-c",
      "user.email=t@example.com",
      "commit",
      "-q",
      "-m",
      "md",
    );
    await writeFile(join(root, "notes.md"), "# Notes\n");
    const server = await startServer({ root, diff: "HEAD" });
    await server.waitForLog(/indexing: done/, 20_000);
    await page.goto(server.base + "md/head/notes.md");
    await expect(
      page.getByRole("heading", { name: "Notes", exact: true }),
    ).toBeVisible();
    await expect(page.locator("main .file-note")).toHaveText([
      "Shown as on disk: a git filter applies; the stored content may differ.",
    ]);
  } finally {
    await rm(root, { recursive: true, force: true });
  }
});
