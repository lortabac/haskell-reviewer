// The docs view: module pages from the sidebar, anchors, Haddock
// links, go to source, wrapping, text-only and not found pages.
import AxeBuilder from "@axe-core/playwright";
import type { Locator, Page } from "@playwright/test";
import { expect, moduleNotFound404, sidebar, test } from "./support/fixtures";
import { shapesLine } from "./support/project";

// The section of a declaration, by the start of its head.
function decl(page: Page, head: string): Locator {
  return page
    .locator("section.decl")
    .filter({
      has: page.locator(".decl-head .sig", {
        hasText: new RegExp("^" + escape(head)),
      }),
    })
    .first();
}

function escape(s: string): string {
  return s.replace(/[.*+?^${}()|[\]\\]/g, "\\$&");
}

async function inViewport(l: Locator): Promise<boolean> {
  return l.evaluate((el) => {
    const r = el.getBoundingClientRect();
    return r.bottom > 0 && r.top < window.innerHeight;
  });
}

test("sidebar link opens the docs view", async ({
  page,
  startServer,
  docsProject,
}) => {
  const server = await startServer({ root: docsProject.root });
  await page.goto(server.base);
  await sidebar(page)
    .getByRole("button", { name: "Implicit package", exact: true })
    .click();
  const link = sidebar(page).getByRole("link", {
    name: "Shapes.hs",
    exact: true,
  });
  await link.click();
  await expect(page).toHaveURL(server.base + "docs/tree/Shapes");
  await expect(page.locator("main h1")).toHaveText("Shapes");
  await expect(link).toHaveAttribute("aria-current", "page");
  await expect(page).toHaveTitle("Shapes — haskell-reviewer");

  // Markup in comments is text; the external link opens a new tab.
  const header = page.locator("main .doc").first();
  await expect(header).toContainText("Markup is text: <b>bold</b>.");
  await expect(header).toContainText("<script>window.__pwned=1</script>");
  expect(await page.evaluate(() => window.__pwned)).toBeUndefined();
  const ext = header.getByRole("link", { name: /the site/ });
  await expect(ext).toHaveAttribute("href", "https://example.com/shapes");
  await expect(ext).toHaveAttribute("target", "_blank");
  await expect(ext).toHaveAttribute("rel", "noopener noreferrer");

  // Heads and signatures, in source order.
  const heads = page.locator("section.decl .decl-head .sig");
  await expect(heads.first()).toHaveText("data Shape");
  await expect(decl(page, "perimeter")).toContainText(
    "perimeter :: Shape -> Double",
  );
  await expect(decl(page, "perimeter")).toContainText(
    "Shares its signature with area.",
  );
  await expect(decl(page, "(<+>)").locator(".decl-badge")).toHaveText([
    "infixl 6",
    "exported",
  ]);
  await expect(decl(page, "old").locator(".decl-warning")).toHaveText(
    "Deprecated: Use area",
  );
  await expect(decl(page, "pattern Unit")).toBeVisible();
  await expect(decl(page, "class Container f")).toBeVisible();
  await expect(decl(page, "instance Num Bool")).toBeVisible();
  await expect(decl(page, "type instance Family Shape = Double")).toBeVisible();
});

test("a link in a signature: anchor, focus, no reload", async ({
  page,
  startServer,
  docsProject,
}) => {
  const server = await startServer({ root: docsProject.root });
  await page.goto(server.base + "docs/tree/Shapes");
  await page.evaluate(() => {
    (window as Window & { __same?: boolean }).__same = true;
  });
  await decl(page, "area")
    .locator(".decl-head a", { hasText: "Shape" })
    .click();
  await expect(page).toHaveURL(server.base + "docs/tree/Shapes#type%3AShape");
  const shape = decl(page, "data Shape");
  await expect(shape).toBeFocused();
  expect(await inViewport(shape)).toBe(true);
  expect(
    await page.evaluate(() => (window as Window & { __same?: boolean }).__same),
  ).toBe(true);
  // Scroll away and click a link to the same anchor: it goes again.
  await page.evaluate(() => window.scrollTo(0, document.body.scrollHeight));
  await decl(page, "perimeter")
    .locator(".decl-head a", { hasText: "Shape" })
    .click();
  await expect(shape).toBeFocused();
  await expect.poll(() => inViewport(shape)).toBe(true);
});

test("deep link to a declaration", async ({
  page,
  startServer,
  docsProject,
}) => {
  const server = await startServer({ root: docsProject.root });
  await page.setViewportSize({ width: 1000, height: 400 });
  await page.goto(server.base + "docs/tree/Shapes#val%3Aold");
  const old = decl(page, "old");
  await expect(old).toBeFocused();
  expect(await inViewport(old)).toBe(true);
});

test("Haddock link from another module: our id instead", async ({
  page,
  startServer,
  docsProject,
}) => {
  const server = await startServer({ root: docsProject.root });
  await page.goto(server.base + "docs/tree/Other");
  await expect(page.locator("main h1")).toHaveText("Other");
  const links = page.locator("main .doc a");
  await expect(links).toHaveText(["Shapes", "Shapes"]);
  await links.first().click();
  await expect(page).toHaveURL(server.base + "docs/tree/Shapes#val%3Aarea");
  await expect(decl(page, "area")).toBeFocused();
  // Replaced, not pushed: back goes to Other.
  await page.goBack();
  await expect(page).toHaveURL(server.base + "docs/tree/Other");
  await page.goto(server.base + "docs/tree/Shapes#t:Shape");
  await expect(page).toHaveURL(server.base + "docs/tree/Shapes#type%3AShape");
  // Haddock's escaped form of <+>.
  await page.goto(server.base + "docs/tree/Shapes#v:-60--43--62-");
  await expect(page).toHaveURL(
    server.base + "docs/tree/Shapes#val%3A%3C%2B%3E",
  );
  await expect(decl(page, "(<+>)")).toBeFocused();
});

test("go to source marks the line", async ({
  page,
  startServer,
  docsProject,
}) => {
  const server = await startServer({ root: docsProject.root });
  await page.setViewportSize({ width: 1000, height: 400 });
  await page.goto(server.base + "docs/tree/Shapes");
  const line = shapesLine("old ::");
  await decl(page, "old")
    .getByRole("link", { name: /Go to source/ })
    .click();
  await expect(page).toHaveURL(server.base + `src/tree/Shapes.hs#L${line}`);
  const mark = page.locator(".file-gutter .file-line-target");
  await expect(mark).toHaveText(String(line));
  await expect.poll(() => inViewport(mark)).toBe(true);
  // The module stays current in the sidebar on its file's page.
  await expect(
    sidebar(page).getByRole("link", { name: "Shapes.hs", exact: true }),
  ).toHaveAttribute("aria-current", "page");
});

test("narrow window: signatures wrap, no sideways scroll", async ({
  page,
  startServer,
  docsProject,
}) => {
  const server = await startServer({ root: docsProject.root });
  await page.setViewportSize({ width: 600, height: 800 });
  await page.goto(server.base + "docs/tree/Shapes");
  const sig = decl(page, "longSig").locator(".decl-head .sig");
  await expect(sig).toBeVisible();
  const box = await sig.evaluate((el) => {
    const r = el.getBoundingClientRect();
    const lh = parseFloat(getComputedStyle(el).lineHeight) || 16;
    return {
      lines: Math.round(r.height / lh),
      over: el.scrollWidth - el.clientWidth,
    };
  });
  expect(box.lines).toBeGreaterThan(2);
  expect(box.over).toBeLessThanOrEqual(0);
  const doc = await page.evaluate(() => ({
    scroll: document.documentElement.scrollWidth,
    client: document.documentElement.clientWidth,
  }));
  expect(doc.scroll).toBeLessThanOrEqual(doc.client);
  // Each line after the first starts with an arrow.
  const parts = await sig.locator(".sig-part").allTextContents();
  expect(parts.slice(1).every((p) => /^(->|=>)/.test(p))).toBe(true);
});

test("a module that doesn't parse: banner and View source", async ({
  page,
  startServer,
  docsProject,
}) => {
  const server = await startServer({ root: docsProject.root });
  await page.goto(server.base + "docs/tree/Broken");
  const banner = page.locator("main .docs-banner");
  await expect(banner).toContainText("This module doesn't parse");
  const view = banner.getByRole("link", { name: "View source" });
  await expect(view).toHaveAttribute("href", /\/src\/tree\/Broken\.hs#L3$/);
  await view.click();
  await expect(page.locator(".file-line-target")).toHaveText("3");
});

test("unknown module: not found, search box prefilled", async ({
  page,
  startServer,
  docsProject,
  guard,
}) => {
  guard.allow(moduleNotFound404);
  const server = await startServer({ root: docsProject.root });
  await page.goto(server.base + "docs/tree/No.Such");
  await expect(page.locator("main h1")).toHaveText(
    "Not found in this snapshot",
  );
  const box = page.getByRole("searchbox", { name: "Search modules by name" });
  await expect(box).toHaveValue("No.Such");
  await expect(page.locator(".search-count")).toHaveText("No modules match.");
  await box.fill("shap");
  const hit = page.locator(".search-results a");
  await expect(hit).toHaveText(["Shapes"]);
  await hit.click();
  await expect(page.locator("main h1")).toHaveText("Shapes");
});

test("unknown declaration: notice and box on the page", async ({
  page,
  startServer,
  docsProject,
}) => {
  const server = await startServer({ root: docsProject.root });
  await page.goto(server.base + "docs/tree/Shapes#type%3ANope");
  const notice = page.locator("main .decl-missing");
  await expect(notice).toContainText(
    "No declaration type:Nope in this module, in this snapshot.",
  );
  await expect(notice.getByRole("searchbox")).toHaveValue("Nope");
  await expect(page.locator("section.decl").first()).toBeVisible();
});

for (const scheme of ["light", "dark"] as const) {
  test(`axe on a module page, ${scheme} theme`, async ({
    page,
    startServer,
    docsProject,
  }) => {
    const server = await startServer({ root: docsProject.root });
    await page.emulateMedia({ colorScheme: scheme });
    await page.goto(server.base + "docs/tree/Shapes");
    await expect(page.locator("section.decl").first()).toBeVisible();
    await page.locator(".docs-extensions summary").click();
    const result = await new AxeBuilder({ page })
      .withTags(["wcag2a", "wcag2aa", "wcag21a", "wcag21aa"])
      .analyze();
    const summary = result.violations.map(
      (v) =>
        `${v.id}: ${v.nodes
          .slice(0, 3)
          .map((n) => n.target.join(" "))
          .join(", ")}`,
    );
    expect(summary).toEqual([]);
  });
}
