// Reload in the browser (D-354, SPEC §5.2): the button and r, the place
// kept, gone declarations and modules, a second tab, --diff REV, and no
// button for a commit range. One temp project per test: they edit it.
import { readFile, rm, writeFile } from "node:fs/promises";
import { join } from "node:path";
import type { Locator, Page } from "@playwright/test";
import { expectNoAxeViolations, themes } from "./support/a11y";
import { expect, showFiles, sidebar, test } from "./support/fixtures";
import { makeReloadProject, type Project, reloadRHs } from "./support/project";
import { settle } from "./support/settle";

let project: Project;
test.beforeEach(async () => {
  project = await makeReloadProject();
});
test.afterEach(async () => {
  await project.remove();
});

const button = (page: Page): Locator =>
  page.getByRole("button", { name: /^Reload/ });
const status = (page: Page): Locator => page.locator(".keys-status");
// Three new lines, put in above.
const three = "-- one\n-- two\n-- three\n";
const decl = (page: Page, name: string): Locator =>
  page
    .locator("main section.decl")
    .filter({
      has: page.locator(":scope > .decl-head .sig", {
        hasText: new RegExp(`^${name}\\b`),
      }),
    })
    .first();

async function openDocs(page: Page, base: string, ref: string) {
  await page.goto(base + "docs/tree/" + ref);
  await expect(page.locator("main section.decl").first()).toBeVisible();
}

// Wait for the reload, then for the new indexing to be done.
async function reloaded(page: Page): Promise<void> {
  await expect(status(page)).toHaveText("Files reloaded.");
  await settle(page);
}

test("browse: Reload keeps the declaration, with the new text", async ({
  page,
  startServer,
}) => {
  const server = await startServer({ root: project.root });
  await openDocs(page, server.base, "R");
  await decl(page, "target").focus();
  await writeFile(
    join(project.root, "R.hs"),
    reloadRHs({ above: three, doc: "New target doc." }),
  );
  await button(page).click();
  await reloaded(page);
  await expect(page).toHaveURL(/\/docs\/tree\/R#val%3Atarget$/);
  await expect(decl(page, "target")).toContainText("New target doc.");
  await expect(decl(page, "target")).toBeInViewport();
  // Focus was on the button: it stays there.
  await expect(button(page)).toBeFocused();
  expect(server.count(/reload: generation 2/)).toBe(1);
});

test("r: focus in main goes to the same declaration", async ({
  page,
  startServer,
}) => {
  const server = await startServer({ root: project.root });
  await openDocs(page, server.base, "R");
  await decl(page, "target").focus();
  await writeFile(join(project.root, "R.hs"), reloadRHs({ above: "-- one\n" }));
  await page.keyboard.press("r");
  await reloaded(page);
  await expect(decl(page, "target")).toBeFocused();
  await expect(page).toHaveURL(/#val%3Atarget$/);
});

test("a removed declaration: the same module, with a notice", async ({
  page,
  startServer,
}) => {
  const server = await startServer({ root: project.root });
  await openDocs(page, server.base, "R");
  await decl(page, "removed").focus();
  await writeFile(join(project.root, "R.hs"), reloadRHs({ removed: false }));
  await page.keyboard.press("r");
  await reloaded(page);
  await expect(page).toHaveURL(/\/docs\/tree\/R#val%3Aremoved$/);
  await expect(page.locator("main .decl-missing")).toContainText(
    "No declaration val:removed in this module, in this snapshot.",
  );
  await expect(decl(page, "removed")).toHaveCount(0);
});

test("a removed module: Home, with a notice", async ({
  page,
  startServer,
  guard,
}) => {
  // The new page asks for the module: 404.
  guard.allow(/\/api\/module\/tree\/Other/);
  const server = await startServer({ root: project.root });
  await openDocs(page, server.base, "Other");
  await rm(join(project.root, "Other.hs"));
  await button(page).click();
  // No "Files reloaded." here: the move to Home clears the status.
  await expect(page).toHaveURL(new RegExp(`/${server.token}/?$`));
  await expect(page.locator("main .reload-gone")).toHaveText(
    "Other is not in the files any more.",
  );
  await settle(page);
  await expect(sidebar(page).getByRole("link", { name: /^Other/ })).toHaveCount(
    0,
  );
});

test("shortcuts off: r does nothing, the button still works", async ({
  page,
  startServer,
}) => {
  const server = await startServer({ root: project.root });
  let posts = 0;
  page.on("request", (r) => {
    if (r.method() === "POST" && r.url().endsWith("/api/reload")) posts++;
  });
  await openDocs(page, server.base, "R");
  await page.getByRole("button", { name: "Shortcuts", exact: true }).click();
  const toggle = page
    .getByRole("dialog")
    .getByRole("checkbox", { name: "Single-key shortcuts" });
  await toggle.uncheck();
  await page.keyboard.press("Escape");
  await expect(page.getByRole("dialog")).toBeHidden();
  await expect(button(page)).toHaveText("Reload");
  await page.locator("main h1").focus();
  await page.keyboard.press("r");
  await expect(status(page)).toHaveText("");
  await button(page).click();
  await reloaded(page);
  expect(posts).toBe(1);
});

test("a second tab gets the reload too", async ({
  context,
  page,
  startServer,
}) => {
  const server = await startServer({ root: project.root });
  await openDocs(page, server.base, "R");
  const other = await context.newPage();
  await openDocs(other, server.base, "R");
  await writeFile(
    join(project.root, "R.hs"),
    reloadRHs({ doc: "Second tab." }),
  );
  await button(page).click();
  // Both before the 5 s clear of the status line.
  await Promise.all([
    expect(status(page)).toHaveText("Files reloaded."),
    expect(status(other)).toHaveText("Files reloaded."),
  ]);
  await expect(decl(other, "target")).toContainText("Second tab.");
  await settle(page);
  await settle(other);
  await other.close();
});

test("Back after a reload focuses the declaration again (D-339)", async ({
  page,
  startServer,
}) => {
  const server = await startServer({ root: project.root });
  await openDocs(page, server.base, "R");
  // Focus on the button: this landing doesn't take focus.
  await button(page).click();
  await reloaded(page);
  await expect(button(page)).toBeFocused();
  const target = decl(page, "target");
  await target.focus();
  await target.locator("a.decl-goto").first().click();
  await expect(page).toHaveURL(/\/src\/tree\/R\.hs/);
  await page.goBack();
  await expect(page).toHaveURL(/\/docs\/tree\/R#val%3Atarget$/);
  await expect(decl(page, "target")).toBeFocused();
});

test("a new file shows in Files after Reload", async ({
  page,
  startServer,
}) => {
  const server = await startServer({ root: project.root });
  await openDocs(page, server.base, "R");
  await showFiles(page);
  const notes = sidebar(page).getByRole("link", { name: /^notes\.txt/ });
  await expect(
    sidebar(page).getByRole("link", { name: /^R\.hs/ }),
  ).toBeVisible();
  await expect(notes).toHaveCount(0);
  await writeFile(join(project.root, "notes.txt"), "new\n");
  await button(page).click();
  await reloaded(page);
  await expect(notes).toBeVisible();
});

test("a failed listing at start, fixed, then Reload", async ({
  page,
  startServer,
  guard,
}) => {
  // The answers while the listing is broken.
  guard.allow(/status of 500/);
  guard.allow(/HTTP 500|500 \(Internal Server Error\)/);
  const index = join(project.root, ".git", "index");
  const good = await readFile(index);
  await writeFile(index, "garbage");
  const server = await startServer({ root: project.root });
  await server.waitForLog(/cannot list files/, 10_000);
  await page.goto(server.base);
  await expect(sidebar(page)).toContainText("Couldn't");
  await writeFile(index, good);
  await button(page).click();
  await reloaded(page);
  await expect(
    sidebar(page).getByRole("button", { name: /^Implicit package/ }),
  ).toBeVisible();
});

test("--diff HEAD: Reload shows a new edit as changed", async ({
  page,
  startServer,
}) => {
  const server = await startServer({ root: project.root, diff: "HEAD" });
  await page.goto(server.base);
  await expect(button(page)).toBeVisible();
  await expect(page.locator("main")).toContainText(
    "No files changed under ROOT.",
  );
  await showFiles(page);
  const r = sidebar(page).getByRole("link", { name: /^R\.hs/ });
  await expect(r).toHaveAccessibleName("R.hs");
  await writeFile(join(project.root, "R.hs"), reloadRHs({ doc: "Edited." }));
  await button(page).click();
  await reloaded(page);
  await expect(r).toHaveAccessibleName(/^R\.hs modified/);
  await expect(page.locator("main")).not.toContainText(
    "No files changed under ROOT.",
  );
});

test("a commit range: no Reload button", async ({ page, startServer }) => {
  const server = await startServer({
    root: project.root,
    diff: "HEAD~1..HEAD",
  });
  await page.goto(server.base);
  await expect(page.locator("main h1")).toHaveText("Home");
  await settle(page);
  await expect(page.locator(".reload-button")).toHaveCount(0);
  await page.locator("main h1").focus();
  await page.keyboard.press("r");
  await expect(status(page)).toHaveText(
    "Reload isn't available in this session.",
  );
});

for (const t of themes) {
  test(`axe: a page with the Reload button, ${t.scheme} theme`, async ({
    page,
    startServer,
  }) => {
    const server = await startServer({ root: project.root });
    await page.emulateMedia({ colorScheme: t.scheme });
    await openDocs(page, server.base, "R");
    await expect(button(page)).toBeVisible();
    await expectNoAxeViolations(page, `docs page, ${t.scheme}`);
    await button(page).click();
    await reloaded(page);
    await expectNoAxeViolations(page, `after a reload, ${t.scheme}`);
  });
}

// R with uses of first, for refs and text search.
const withUses =
  "module R where\n\n-- | First.\nfirst :: Int\nfirst = 1\n\n" +
  "-- | T.\ntarget :: Int\ntarget = first + 2\n\nmore :: Int\nmore = first\n";

test("refs: Reload from the button keeps focus there, not on the followed row", async ({
  page,
  startServer,
}) => {
  await writeFile(join(project.root, "R.hs"), withUses);
  const server = await startServer({ root: project.root });
  await page.goto(server.base + "refs/tree/R?decl=val%3Afirst");
  const rows = page.locator("main [data-ref-key]");
  await expect(rows.first()).toBeVisible();
  await settle(page);
  await rows.first().click();
  await expect(page).toHaveURL(/\/src\/tree\/R\.hs/);
  await page.goBack();
  await expect(rows.first()).toBeFocused();
  await button(page).click();
  await reloaded(page);
  await expect(button(page)).toBeFocused();
});

test("text search: Reload from the button keeps focus there", async ({
  page,
  startServer,
}) => {
  await writeFile(join(project.root, "R.hs"), withUses);
  const server = await startServer({ root: project.root });
  await page.goto(server.base + "search/tree?q=first");
  await expect(page.locator("main [data-hit-key]").first()).toBeVisible();
  await settle(page);
  await button(page).click();
  await reloaded(page);
  await expect(button(page)).toBeFocused();
});

test("another tab's reload leaves focus in the header search box", async ({
  context,
  page,
  startServer,
}) => {
  await writeFile(join(project.root, "R.hs"), withUses);
  const server = await startServer({ root: project.root });
  await page.goto(server.base + "search/tree?q=first");
  await expect(page.locator("main [data-hit-key]").first()).toBeVisible();
  await settle(page);
  const box = page.locator(".search-box-header input");
  await box.focus();
  const other = await context.newPage();
  await openDocs(other, server.base, "R");
  await button(other).click();
  await reloaded(page);
  await expect(box).toBeFocused();
  await settle(other);
  await other.close();
});

test("the gone notice is for that landing only", async ({
  page,
  startServer,
  guard,
}) => {
  guard.allow(/\/api\/module\/tree\/Other/);
  const server = await startServer({ root: project.root });
  await openDocs(page, server.base, "Other");
  await rm(join(project.root, "Other.hs"));
  await button(page).click();
  await expect(page.locator("main .reload-gone")).toHaveText(
    "Other is not in the files any more.",
  );
  await settle(page);
  await writeFile(
    join(project.root, "Other.hs"),
    "module Other where\n\nx :: Int\nx = 1\n",
  );
  await button(page).click();
  await reloaded(page);
  await expect(page.locator("main .reload-gone")).toHaveCount(0);
  // Nor after a page load of that entry.
  await page.reload();
  await expect(page.locator("main h1")).toHaveText("Home");
  await expect(page.locator("main .reload-gone")).toHaveCount(0);
});

// The same code line (D-356).
const cursorRow = (page: Page): Locator =>
  page.locator('main .code-row:has([tabindex="0"])');
async function openSource(page: Page, base: string, hash: string) {
  await page.goto(base + "src/tree/R.hs" + hash);
  // The outline: the place is kept by declaration.
  await expect(page.locator("main .code-outline")).toBeVisible();
}

test("source: lines put in above, Reload: the same code line", async ({
  page,
  startServer,
}) => {
  const server = await startServer({ root: project.root });
  await openSource(page, server.base, "#L9");
  await expect(cursorRow(page)).toHaveAttribute("data-line", "9");
  await expect(cursorRow(page)).toContainText("target = 2");
  await writeFile(join(project.root, "R.hs"), reloadRHs({ above: three }));
  await button(page).click();
  await reloaded(page);
  await expect(page).toHaveURL(/\/src\/tree\/R\.hs#L12$/);
  await expect(cursorRow(page)).toHaveAttribute("data-line", "12");
  await expect(cursorRow(page)).toContainText("target = 2");
});

test("source: a marked range is kept", async ({ page, startServer }) => {
  const server = await startServer({ root: project.root });
  await openSource(page, server.base, "#L8-L9");
  await writeFile(join(project.root, "R.hs"), reloadRHs({ above: three }));
  await button(page).click();
  await reloaded(page);
  await expect(page).toHaveURL(/#L11-L12$/);
  await expect(cursorRow(page)).toContainText("target :: Int");
});

test("source: the declaration gone, the line kept in the file", async ({
  page,
  startServer,
}) => {
  const server = await startServer({ root: project.root });
  await openSource(page, server.base, "#L13");
  await expect(cursorRow(page)).toContainText("removed = 3");
  await writeFile(join(project.root, "R.hs"), reloadRHs({ removed: false }));
  await button(page).click();
  await reloaded(page);
  // Clamped to the last line (9).
  await expect(page).toHaveURL(/\/src\/tree\/R\.hs#L9$/);
  await expect(cursorRow(page)).toHaveAttribute("data-line", "9");
  await expect(cursorRow(page)).toContainText("target = 2");
});

test("source: no hash, Reload: still none", async ({ page, startServer }) => {
  const server = await startServer({ root: project.root });
  await openSource(page, server.base, "");
  await writeFile(join(project.root, "R.hs"), reloadRHs({ above: three }));
  await button(page).click();
  await reloaded(page);
  await expect(page).toHaveURL(/\/src\/tree\/R\.hs$/);
});

const diffRow = (page: Page): Locator =>
  page.locator('main [role="row"]:has([tabindex="0"])');

test("--diff HEAD: a head line below new lines keeps its code", async ({
  page,
  startServer,
}) => {
  await writeFile(join(project.root, "R.hs"), reloadRHs({ doc: "Edited." }));
  const server = await startServer({ root: project.root, diff: "HEAD" });
  await page.goto(server.base + "diff/R.hs#head-L9");
  await expect(diffRow(page)).toContainText("target = 2");
  await writeFile(
    join(project.root, "R.hs"),
    reloadRHs({ above: three, doc: "Edited." }),
  );
  await button(page).click();
  await reloaded(page);
  await expect(page).toHaveURL(/\/diff\/R\.hs#head-L12$/);
  await expect(diffRow(page)).toContainText("target = 2");
});

test("--diff HEAD: a base line stays", async ({ page, startServer }) => {
  await writeFile(join(project.root, "R.hs"), reloadRHs({ doc: "Edited." }));
  const server = await startServer({ root: project.root, diff: "HEAD" });
  // Base line 7: the old doc, a removed row.
  await page.goto(server.base + "diff/R.hs#base-L7");
  await expect(diffRow(page)).toContainText("Target doc.");
  await writeFile(
    join(project.root, "R.hs"),
    reloadRHs({ above: three + three, doc: "Edited." }),
  );
  await button(page).click();
  await reloaded(page);
  await expect(page).toHaveURL(/\/diff\/R\.hs#base-L7$/);
  await expect(diffRow(page)).toContainText("Target doc.");
});

test("source: the outline late, focus moved out meanwhile: it stays out", async ({
  page,
  startServer,
}) => {
  const server = await startServer({ root: project.root });
  await openSource(page, server.base, "#L9");
  await cursorRow(page).locator('[tabindex="0"]').focus();
  // After the reload, api/source answers once we let it.
  let release: (() => void) | undefined;
  const held = new Promise<void>((r) => (release = r));
  await page.route(/\/api\/source\//, async (route) => {
    await held;
    await route.continue();
  });
  await writeFile(join(project.root, "R.hs"), reloadRHs({ above: three }));
  await page.keyboard.press("r");
  await expect(status(page)).toHaveText("Files reloaded.");
  // The old line, for now; focus went with it.
  await expect(cursorRow(page)).toHaveAttribute("data-line", "9");
  await button(page).focus();
  release?.();
  await expect(page).toHaveURL(/#L12$/);
  await expect(cursorRow(page)).toContainText("target = 2");
  await expect(button(page)).toBeFocused();
  await settle(page);
});

test("source: scrolled, never clicked: the same top line (T17a review N3)", async ({
  page,
  startServer,
}) => {
  const long = (above: string) =>
    "module L where\n\n" +
    above +
    Array.from({ length: 200 }, (_, i) => `f${i} :: Int\nf${i} = ${i}\n`).join(
      "",
    );
  await writeFile(join(project.root, "L.hs"), long(""));
  const server = await startServer({ root: project.root });
  await page.goto(server.base + "src/tree/L.hs");
  await expect(page.locator("main .code-outline")).toBeVisible();
  const box = page.locator("main .code-view");
  const top = () =>
    box.evaluate((b) => {
      const t = b.getBoundingClientRect().top;
      const rows = [...b.querySelectorAll<HTMLElement>(".code-row[data-line]")];
      // The row half a row below the top edge.
      return rows.find((row) => {
        const r = row.getBoundingClientRect();
        return r.top <= t + r.height / 2 && t + r.height / 2 < r.bottom;
      })?.textContent;
    });
  // Line 93 at the top (a bit of it off screen).
  await box.evaluate((b) => {
    const r = b.querySelector(".code-row[data-line]")!.getBoundingClientRect();
    b.scrollTop = Math.round((92 + 1 / 4) * r.height);
  });
  const before = await top();
  expect(before).toBe("f45 :: Int");
  await writeFile(join(project.root, "L.hs"), long(three));
  await button(page).click();
  await reloaded(page);
  await expect(page).toHaveURL(/\/src\/tree\/L\.hs$/);
  await expect.poll(top).toBe(before);
});
