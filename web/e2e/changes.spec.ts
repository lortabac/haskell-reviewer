// Diff markers in the sidebar, ghosts and "Changed only" (D-337).
// fixtures/repos/sides.sh: base has A and B, head has A (changed), C
// and readme-link. renames.sh with ROOT pkg: renames, and d1/Same.hs
// moved to d2/Same.hs (a loose module: removed plus added).
import { join } from "node:path";
import type { Locator, Page } from "@playwright/test";
import { expectNoAxeViolations, themes } from "./support/a11y";
import { expect, showFiles, sidebar, test } from "./support/fixtures";
import { makeRenamesProject } from "./support/project";
import type { Server } from "./support/server";

const diff = "HEAD~1..HEAD";

async function start(
  startServer: (o: { root: string; diff?: string }) => Promise<Server>,
  root: string,
): Promise<Server> {
  const server = await startServer({ root, diff });
  await server.waitForLog(/indexing: done/, 20_000);
  return server;
}

function folder(page: Page, name: string): Locator {
  return sidebar(page).getByRole("button", { name, exact: true });
}

// A sidebar link by the start of its name.
function entry(page: Page, start: string): Locator {
  return sidebar(page).getByRole("link", { name: new RegExp(`^${start}`) });
}

function changedOnly(page: Page): Locator {
  return sidebar(page).getByRole("checkbox", { name: "Changed only" });
}

test("modules: markers, and a ghost for B linking to base", async ({
  page,
  startServer,
  sidesProject,
}) => {
  const server = await start(startServer, sidesProject.root);
  await page.goto(server.base);
  await folder(page, "sides 0.1").click();
  await folder(page, "lib").click();
  const b = entry(page, "B");
  await expect(b).toHaveAccessibleName("B removed (on base)");
  await expect(b).toHaveAttribute("href", /\/docs\/base\/B$/);
  await expect(b).not.toHaveAttribute("aria-current", /.*/);
  await expect(entry(page, "C")).toHaveAccessibleName(/^C added/);
  await expect(entry(page, "A")).toHaveAccessibleName(/^A modified/);
  await b.click();
  await expect(page).toHaveURL(/\/docs\/base\/B$/);
  await expect(page.locator("main h1")).toHaveText("B");
});

test("files: a ghost for src/B.hs, readme-link added", async ({
  page,
  startServer,
  sidesProject,
}) => {
  const server = await start(startServer, sidesProject.root);
  await page.goto(server.base);
  await showFiles(page);
  await expect(entry(page, "readme-link")).toHaveAccessibleName(
    /^readme-link .*added/,
  );
  await expect(entry(page, "README.md")).toHaveAccessibleName("README.md");
  await folder(page, "src").click();
  const b = entry(page, "B.hs");
  await expect(b).toHaveAccessibleName("B.hs removed (on base)");
  await expect(b).toHaveAttribute("href", /\/src\/base\/src\/B\.hs$/);
});

test("changed only: hides unchanged files, remembered across a reload", async ({
  page,
  startServer,
  sidesProject,
}) => {
  const server = await start(startServer, sidesProject.root);
  await page.goto(server.base);
  await showFiles(page);
  await expect(entry(page, "README.md")).toBeVisible();
  await changedOnly(page).check();
  await expect(entry(page, "README.md")).toHaveCount(0);
  // Folders open by default: few files left.
  await expect(entry(page, "B.hs")).toBeVisible();
  await expect(entry(page, "readme-link")).toBeVisible();
  await expect
    .poll(async () => {
      const r = await page.request.get(server.base + "api/prefs");
      return ((await r.json()) as { changedOnly: boolean }).changedOnly;
    })
    .toBe(true);
  await page.reload();
  await expect(changedOnly(page)).toBeChecked();
  await expect(entry(page, "B.hs")).toBeVisible();
  await expect(entry(page, "README.md")).toHaveCount(0);
});

test("changed only: focus stays on the sidebar link after a navigation", async ({
  page,
  startServer,
  sidesProject,
}) => {
  const server = await start(startServer, sidesProject.root);
  // README.md is unchanged: it shows only as the current file.
  await page.goto(server.base + "src/head/README.md");
  await showFiles(page);
  await changedOnly(page).check();
  await expect(entry(page, "README.md")).toBeVisible();
  const a = entry(page, "A.hs");
  await a.focus();
  await page.keyboard.press("Enter");
  await expect(page).toHaveURL(/\/src\/head\/src\/A\.hs$/);
  await expect(a).toHaveAttribute("aria-current", "page");
  await expect(a).toBeFocused();
  await expect(entry(page, "README.md")).toHaveCount(0);
});

test("browse mode: no Changed only box", async ({ page, server }) => {
  await page.goto(server.base);
  await showFiles(page);
  await expect(sidebar(page).getByRole("link").first()).toBeVisible();
  await expect(changedOnly(page)).toHaveCount(0);
  await expect(sidebar(page).locator(".change-legend")).toHaveCount(0);
});

test.describe("renames.sh, ROOT pkg", () => {
  let project: { root: string; remove: () => Promise<void> };
  test.beforeAll(async () => {
    project = await makeRenamesProject();
  });
  test.afterAll(async () => {
    await project.remove();
  });

  test("renamed from on head, renamed to on base", async ({
    page,
    startServer,
  }) => {
    const server = await start(startServer, join(project.root, "pkg"));
    await page.goto(server.base);
    await showFiles(page);
    await folder(page, "exact").click();
    const b = entry(page, "b.txt");
    await expect(b).toHaveAccessibleName(
      "b.txt renamed from exact/a.txt (100%)",
    );
    await expect(b).toHaveAttribute(
      "data-tip",
      "renamed from exact/a.txt (100%)",
    );
    await page.goto(server.base + "src/base/exact/a.txt");
    const a = entry(page, "a.txt");
    await expect(a).toHaveAccessibleName("a.txt renamed to exact/b.txt (100%)");
    await expect(a).toHaveAttribute("aria-current", "page");
  });

  test("a loose module moved: a ghost on head", async ({
    page,
    startServer,
  }) => {
    const server = await start(startServer, join(project.root, "pkg"));
    await page.goto(server.base);
    await sidebar(page)
      .getByRole("button", { name: /^(Implicit package|Other files)$/ })
      .click();
    const ghost = entry(page, "d1/Same.hs");
    await expect(ghost).toHaveAccessibleName("d1/Same.hs removed (on base)");
    await expect(ghost).toHaveAttribute("href", /\/docs\/base\//);
    await expect(entry(page, "d2/Same.hs")).toHaveAccessibleName(
      /^d2\/Same\.hs added/,
    );
  });
});

for (const t of themes) {
  test(`axe: sidebar with ghosts and changed only, ${t.scheme} theme`, async ({
    page,
    startServer,
    sidesProject,
  }) => {
    const server = await start(startServer, sidesProject.root);
    await page.emulateMedia({ colorScheme: t.scheme });
    await page.goto(server.base + "docs/head/A");
    await changedOnly(page).check();
    await expect(entry(page, "B")).toBeVisible();
    await expectNoAxeViolations(page, `modules, changed only (${t.scheme})`);
    await showFiles(page);
    await expect(entry(page, "B.hs")).toBeVisible();
    await expectNoAxeViolations(page, `files, changed only (${t.scheme})`);
  });
}
