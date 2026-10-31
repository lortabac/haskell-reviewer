// The module tree in the sidebar, the project notices and the file
// banners: on the main fixture (an implicit package) and on a cabal
// project.
import AxeBuilder from "@axe-core/playwright";
import type { Locator, Page } from "@playwright/test";
import { expect, showFiles, sidebar, tabKey, test } from "./support/fixtures";

const texts = {
  implicit:
    "No cabal.project or .cabal file: ROOT is read as one implicit package, with module names from file paths (GHC2021).",
  hpack:
    "Hpack isn't supported: package.yaml without a .cabal file in hp. Run hpack and commit the .cabal file; until then it's a plain directory.",
  generated: "Generated from this file by alex: shown as text, not indexed.",
  preprocessed: "Preprocessed by hspec-discover: shown as text, not indexed.",
};

const wcag = ["wcag2a", "wcag2aa", "wcag21a", "wcag21aa"];

async function axe(page: Page, what: string): Promise<void> {
  const result = await new AxeBuilder({ page }).withTags(wcag).analyze();
  const summary = result.violations.map(
    (v) =>
      `${v.id} (${v.impact ?? "?"}): ${v.nodes
        .slice(0, 3)
        .map((n) => n.target.join(" "))
        .join(", ")}`,
  );
  expect(summary, `axe violations on ${what}`).toEqual([]);
}

function toggle(page: Page, name: "Modules" | "Files"): Locator {
  return sidebar(page).getByRole("button", { name, exact: true });
}

function group(page: Page, name: string): Locator {
  return sidebar(page).getByRole("button", { name, exact: true });
}

function link(page: Page, name: string): Locator {
  return sidebar(page).getByRole("link", { name, exact: true });
}

// Opens the groups, in order, and waits for each.
async function open(page: Page, ...names: string[]): Promise<void> {
  for (const name of names) {
    const b = group(page, name);
    await b.click();
    await expect(b).toHaveAttribute("aria-expanded", "true");
  }
}

test.describe("implicit package", () => {
  test("Modules by default, the notice, open a module", async ({
    page,
    server,
  }) => {
    await page.goto(server.base);
    await expect(toggle(page, "Modules")).toHaveAttribute(
      "aria-pressed",
      "true",
    );
    await expect(toggle(page, "Files")).toHaveAttribute(
      "aria-pressed",
      "false",
    );
    await expect(page.locator("main .project-notice")).toHaveText([
      texts.implicit,
    ]);
    await open(page, "Implicit package");
    for (const path of [
      "src/Data/Deep/Nested/Mod.hs",
      "src/Main.hs",
      "xss/Evil.hs",
    ]) {
      await expect(link(page, path)).toBeVisible();
    }
    // Non-Haskell files are in the file tree only.
    await expect(link(page, "README.md")).toHaveCount(0);

    // Modules open the docs view.
    await link(page, "src/Main.hs").click();
    await expect(page).toHaveURL(server.base + "docs/tree/Main");
    await expect(page.locator("main h1")).toHaveText("Main");
    await expect(link(page, "src/Main.hs")).toHaveAttribute(
      "aria-current",
      "page",
    );
    // A plain module: no banner.
    await expect(page.locator(".docs-banner")).toHaveCount(0);
  });

  test("deep link opens the group", async ({ page, server }) => {
    await page.goto(server.base + "src/tree/xss/Evil.hs");
    await expect(group(page, "Implicit package")).toHaveAttribute(
      "aria-expanded",
      "true",
    );
    await expect(link(page, "xss/Evil.hs")).toHaveAttribute(
      "aria-current",
      "page",
    );
  });

  test("toggle by keyboard; the pick survives navigation", async ({
    page,
    server,
    browserName,
  }) => {
    const tab = tabKey(browserName);
    const back = tab.replace("Tab", "Shift+Tab");
    await page.goto(server.base);
    await expect(group(page, "Implicit package")).toBeVisible();
    // The theme switch (System/Light/Dark) comes right before the
    // sidebar in tab order: focus its last button, then one more Tab
    // reaches the toggle, with no header-content Tab count to keep in
    // sync (C7).
    await page.getByRole("button", { name: "Dark", exact: true }).focus();
    await page.keyboard.press(tab);
    await expect(toggle(page, "Modules")).toBeFocused();
    await page.keyboard.press(tab);
    await expect(toggle(page, "Files")).toBeFocused();
    await page.keyboard.press(" ");
    await expect(toggle(page, "Files")).toHaveAttribute("aria-pressed", "true");
    await expect(group(page, "Implicit package")).toBeHidden();
    await expect(link(page, "README.md")).toBeVisible();
    await page.keyboard.press(back);
    await expect(toggle(page, "Modules")).toBeFocused();
    await page.keyboard.press("Enter");
    await expect(toggle(page, "Modules")).toHaveAttribute(
      "aria-pressed",
      "true",
    );
    await expect(link(page, "README.md")).toBeHidden();

    await showFiles(page);
    await link(page, "README.md").click();
    await expect(page.locator("main h1")).toHaveText("README.md");
    await expect(toggle(page, "Files")).toHaveAttribute("aria-pressed", "true");
  });
});

test.describe("cabal project", () => {
  test("packages, components, modules, markers", async ({
    page,
    startServer,
    cabalProject,
  }) => {
    const server = await startServer({ root: cabalProject.root });
    await page.goto(server.base);
    await expect(toggle(page, "Modules")).toHaveAttribute(
      "aria-pressed",
      "true",
    );
    await expect(page.locator("main .project-notice")).toHaveText([
      texts.hpack,
    ]);
    const warnings = page.locator("main details");
    await expect(warnings.locator("summary")).toHaveText("1 project warning");

    const top = sidebar(page).locator(".module-tree > li > :first-child");
    await expect(top).toHaveText([
      "pkg 0.1.0",
      "bad/bad.cabal broken",
      "Other files",
    ]);
    await expect(link(page, "bad/bad.cabal broken")).toHaveAttribute(
      "href",
      /\/src\/tree\/bad\/bad\.cabal$/,
    );
    await expect(sidebar(page).locator(".tree-problem")).toContainText(
      "can't parse: bad/bad.cabal",
    );

    await open(page, "pkg 0.1.0");
    for (const name of ["lib", "exe:app", "test:spec"]) {
      await expect(group(page, name)).toBeVisible();
    }
    await expect(group(page, "bench:slow not buildable")).toBeVisible();

    await open(page, "lib");
    await expect(link(page, "Foo")).toBeVisible();
    await expect(link(page, "Sig signature")).toBeVisible();
    // Generated: the docs page too, it says why it's text only.
    await expect(link(page, "Lexer generated (alex)")).toHaveAttribute(
      "href",
      /\/docs\/tree\/Lexer$/,
    );
    const paths = sidebar(page).locator(".tree-entry");
    await expect(paths).toHaveText(["Paths_pkg listed, no source (autogen)"]);
    await expect(paths.locator("a")).toHaveCount(0);

    await open(page, "test:spec");
    await expect(
      link(page, "Main preprocessed (hspec-discover)"),
    ).toBeVisible();
    await open(page, "Other files");
    await expect(link(page, "scripts/Tool.hs")).toBeVisible();
  });

  test("file banners", async ({ page, startServer, cabalProject }) => {
    const server = await startServer({ root: cabalProject.root });
    const cases: [string, string | null][] = [
      ["pkg/src/Lexer.x", texts.generated],
      ["pkg/test/Spec.hs", texts.preprocessed],
      ["pkg/src/Foo.hs", null],
    ];
    for (const [path, text] of cases) {
      await page.goto(server.base + "src/tree/" + path);
      await expect(page.locator("main .file-code")).toBeVisible();
      // Wait for the tree, so a missing banner means no banner.
      await expect(
        sidebar(page).locator('.module-tree [aria-current="page"]'),
      ).toBeVisible();
      if (text === null) {
        await expect(page.locator(".file-banner")).toHaveCount(0);
      } else {
        await expect(page.locator("main h1 + .file-banner")).toHaveText(text);
      }
    }
  });
});

const themes = ["light", "dark"] as const;

for (const scheme of themes) {
  test(`axe, ${scheme} theme`, async ({
    page,
    server,
    startServer,
    cabalProject,
  }, testInfo) => {
    testInfo.setTimeout(60_000);
    await page.emulateMedia({ colorScheme: scheme });

    await page.goto(server.base);
    await open(page, "Implicit package");
    await axe(page, "implicit, open");

    const cabal = await startServer({ root: cabalProject.root });
    await page.goto(cabal.base + "src/tree/pkg/src/Lexer.x");
    await expect(page.locator(".file-banner")).toHaveText(texts.generated);
    // The deep link opened pkg and lib.
    await expect(link(page, "Foo")).toBeVisible();
    await open(page, "exe:app", "test:spec", "bench:slow not buildable");
    await open(page, "Other files");
    await page.locator("main details summary").click();
    await expect(page.locator("main details li")).toBeVisible();
    await axe(page, "cabal, generated file, groups and warnings open");

    await showFiles(page);
    await axe(page, "Files view");
  });
}
