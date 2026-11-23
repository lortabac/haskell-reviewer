// Home in diff mode and the header's outside-ROOT count (D-338).
// fixtures/repos/sides.sh: base has A and B, head has A (changed), C
// and readme-link. renames.sh with ROOT pkg: 3 changes outside ROOT
// (other/x, other/new, other/moved.hs).
import { join } from "node:path";
import type { Page } from "@playwright/test";
import { expectNoAxeViolations, noSideways, themes } from "./support/a11y";
import { expect, test } from "./support/fixtures";
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

const main = (page: Page) => page.locator("main");

// The app renders once api/session answers: wait for it before keys.
async function home(page: Page, server: Server): Promise<void> {
  await page.goto(server.base);
  await expect(
    page.getByRole("heading", { name: "Home", level: 1 }),
  ).toBeVisible();
}

test("sides.sh: counts, the list, links to the diff view", async ({
  page,
  startServer,
  sidesProject,
}) => {
  const server = await start(startServer, sidesProject.root);
  await home(page, server);
  const counts = main(page).locator(".home-counts");
  await expect(counts.first()).toHaveText(
    "5 files changed: 2 added, 1 removed, 2 modified.",
  );
  await expect(counts.nth(1)).toHaveText(
    /^3 modules changed: 1 added, 1 removed, 1 modified[.;]/,
  );
  await expect(main(page).locator(".home-revisions")).toHaveText(
    /^Diff HEAD~1\.\.HEAD\. Base: HEAD~1 \([0-9a-f]{7}\)\. Head: HEAD \([0-9a-f]{7}\)\.$/,
  );
  const list = main(page).locator(".home-files");
  const b = list.getByRole("link", { name: "src/B.hs", exact: true });
  await expect(b).toHaveAttribute("href", /\/diff\/src\/B\.hs$/);
  const c = list.getByRole("link", { name: "src/C.hs", exact: true });
  await expect(c).toHaveAttribute("href", /\/diff\/src\/C\.hs$/);
  await expect(
    list.getByRole("link", { name: "readme-link", exact: true }),
  ).toHaveAttribute("href", /\/diff\/readme-link$/);
  // A removed file: its diff, all removed lines.
  await b.click();
  await expect(page).toHaveURL(/\/diff\/src\/B\.hs$/);
  await expect(page.locator("main h1")).toHaveText("src/B.hs");
  await expect(
    page
      .getByRole("grid", { name: "Diff of src/B.hs" })
      .locator(".diff-delete")
      .first(),
  ).toBeVisible();
  // No changes outside ROOT here: no header count.
  await expect(page.locator(".outside-count")).toHaveCount(0);
});

test("browse mode: Home as before, no header count", async ({
  page,
  server,
}) => {
  await home(page, server);
  await expect(main(page)).toContainText(
    "Pick a module or file in the sidebar.",
  );
  await expect(main(page).locator("h2")).toHaveCount(0);
  await expect(page.locator(".outside-count")).toHaveCount(0);
});

test.describe("renames.sh, ROOT pkg", () => {
  let project: { root: string; remove: () => Promise<void> };
  test.beforeAll(async () => {
    project = await makeRenamesProject();
  });
  test.afterAll(async () => {
    await project.remove();
  });

  test("the outside list on Home, the header count and its tip", async ({
    page,
    startServer,
  }) => {
    const server = await start(startServer, join(project.root, "pkg"));
    await page.goto(server.base + "src/head/exact/b.txt");
    await expect(page.locator("main h1")).toHaveText("exact/b.txt");
    const count = page.getByRole("link", {
      name: "3 changed files outside ROOT",
    });
    // What the eye sees: the text without the sr-only words.
    const seen = () =>
      count.evaluate((e) =>
        [...e.childNodes]
          .filter((n) => !(n instanceof Element && n.matches(".sr-only")))
          .map((n) => n.textContent)
          .join(""),
      );
    await expect.poll(seen).toBe("3 outside ROOT");
    await expect(count).toHaveAttribute(
      "data-tip",
      "other/moved.hs\nother/new\nother/x",
    );
    // Tab from the control before it: the tip shows on keyboard focus.
    await page.getByRole("button", { name: "Copy head commit hash" }).focus();
    await page.keyboard.press("Tab");
    await expect(count).toBeFocused();
    const tip = () =>
      count.evaluate((e) => {
        const s = getComputedStyle(e, "::after");
        return { display: s.display, ws: s.whiteSpace };
      });
    await expect.poll(tip).toEqual({ display: "block", ws: "pre-line" });
    await page.keyboard.press("Escape");
    await expect.poll(async () => (await tip()).display).toBe("none");
    await page.keyboard.press("Enter");
    // The router's base has no slash; the server serves both.
    await expect(page).toHaveURL(server.base.replace(/\/$/, ""));
    await expect(main(page).locator(".home-counts").last()).toHaveText(
      "3 files changed outside ROOT.",
    );
    await main(page).locator(".home-outside summary").click();
    await expect(main(page).locator(".home-outside li")).toHaveText([
      "other/moved.hs",
      "other/new",
      "other/x",
    ]);
    await expect(
      main(page).getByText(
        "Too many files: only exact and same-name renames were looked for.",
      ),
    ).toHaveCount(0);
  });
});

for (const t of themes) {
  test(`axe: renames.sh Home with the outside list and header count, ${t.scheme} theme`, async ({
    page,
    startServer,
  }) => {
    const project = await makeRenamesProject();
    try {
      const server = await start(startServer, join(project.root, "pkg"));
      await page.emulateMedia({ colorScheme: t.scheme });
      await home(page, server);
      await expect(page.locator(".outside-count")).toBeVisible();
      await expect(
        main(page).locator(".home-files li", { hasText: "from exact/a.txt" }),
      ).toBeVisible();
      await main(page).locator(".home-outside summary").click();
      await expect(main(page).locator(".home-outside li")).toHaveCount(3);
      await expectNoAxeViolations(page, `Home, renames.sh (${t.scheme})`);
    } finally {
      await project.remove();
    }
  });

  test(`axe: Home in diff mode, ${t.scheme} theme`, async ({
    page,
    startServer,
    sidesProject,
  }) => {
    const server = await start(startServer, sidesProject.root);
    await page.emulateMedia({ colorScheme: t.scheme });
    await home(page, server);
    await expect(main(page).locator(".home-files li")).toHaveCount(5);
    await expectNoAxeViolations(page, `Home, diff mode (${t.scheme})`);
  });
}

test("320 px: Home in diff mode, no sideways scroll", async ({
  page,
  startServer,
}) => {
  const project = await makeRenamesProject();
  try {
    const server = await start(startServer, join(project.root, "pkg"));
    await page.setViewportSize({ width: 320, height: 640 });
    await home(page, server);
    await expect(main(page).locator(".home-files li").first()).toBeVisible();
    await main(page).locator(".home-outside summary").click();
    await expect(main(page).locator(".home-outside li").first()).toBeVisible();
    await noSideways(page);
  } finally {
    await project.remove();
  }
});
