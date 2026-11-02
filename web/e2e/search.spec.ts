// The header search box (D-209): typing, arrow keys, Enter, Escape,
// a module, a qualified query and a field; the not-found box finds a
// declaration too (amends D-183).
import type { Locator, Page } from "@playwright/test";
import { expect, moduleNotFound404, sidebar, test } from "./support/fixtures";

function escape(s: string): string {
  return s.replace(/[.*+?^${}()|[\]\\]/g, "\\$&");
}

// The section of a declaration, by the start of its head (docs.spec.ts
// has the same helper, not exported).
function decl(page: Page, head: string): Locator {
  return page
    .locator("section.decl")
    .filter({
      has: page.locator(":scope > .decl-head .sig", {
        hasText: new RegExp("^" + escape(head)),
      }),
    })
    .first();
}

function headerBox(page: Page): Locator {
  return page
    .locator(".frame-header")
    .getByRole("combobox", { name: "Search modules and names" });
}

function headerResults(page: Page): Locator {
  return page.locator(".frame-header .search-results a");
}

test("header: type, ArrowDown, Enter opens the declaration", async ({
  page,
  startServer,
  docsProject,
}) => {
  const server = await startServer({ root: docsProject.root });
  await page.goto(server.base + "docs/tree/Shapes");
  const box = headerBox(page);
  await box.fill("perimeter");
  await expect(headerResults(page).first()).toBeVisible();
  await box.press("ArrowDown");
  await box.press("Enter");
  await expect(page).toHaveURL(
    server.base + "docs/tree/Shapes#val%3Aperimeter",
  );
  await expect(decl(page, "perimeter")).toBeFocused();
  // The popup closed; the text stays.
  await expect(box).toHaveAttribute("aria-expanded", "false");
  await expect(box).toHaveValue("perimeter");
});

test("header: a module result opens its docs page", async ({
  page,
  startServer,
  docsProject,
}) => {
  const server = await startServer({ root: docsProject.root });
  await page.goto(server.base);
  const box = headerBox(page);
  await box.fill("Other");
  const hit = headerResults(page).filter({ hasText: "Other" });
  await expect(hit).toHaveCount(1);
  await hit.click();
  await expect(page).toHaveURL(server.base + "docs/tree/Other");
});

test("header: a qualified query opens the right module's declaration", async ({
  page,
  startServer,
  docsProject,
}) => {
  const server = await startServer({ root: docsProject.root });
  await page.goto(server.base);
  const box = headerBox(page);
  await box.fill("Shapes.area");
  await expect(headerResults(page).first()).toBeVisible();
  await box.press("Enter");
  await expect(page).toHaveURL(server.base + "docs/tree/Shapes#val%3Aarea");
});

test("header: a field result lands on the nested field", async ({
  page,
  startServer,
  docsProject,
}) => {
  const server = await startServer({ root: docsProject.root });
  await page.goto(server.base + "docs/tree/Shapes");
  const box = headerBox(page);
  await box.fill("width");
  await expect(headerResults(page).first()).toBeVisible();
  await box.press("Enter");
  await expect(page).toHaveURL(
    server.base + "docs/tree/Shapes#field%3AShape.width",
  );
  const field = page.locator(".subdecl").filter({
    has: page.locator(":scope > .subdecl-head .sig", {
      hasText: /^width :: !Double$/,
    }),
  });
  await expect(field).toBeFocused();
});

test("header: Escape closes the popup, keeps the text; clears when already closed", async ({
  page,
  startServer,
  docsProject,
}) => {
  const server = await startServer({ root: docsProject.root });
  await page.goto(server.base);
  const box = headerBox(page);
  await box.fill("Shapes");
  await expect(box).toHaveAttribute("aria-expanded", "true");
  await box.press("Escape");
  await expect(box).toHaveAttribute("aria-expanded", "false");
  await expect(box).toHaveValue("Shapes");
  await box.press("Escape");
  await expect(box).toHaveValue("");
});

test("not-found page: the search box finds a declaration too", async ({
  page,
  startServer,
  docsProject,
  guard,
}) => {
  guard.allow(moduleNotFound404);
  const server = await startServer({ root: docsProject.root });
  await page.goto(server.base + "docs/tree/No.Such");
  const box = page
    .locator("main")
    .getByRole("combobox", { name: "Search modules and names" });
  await box.fill("perimeter");
  const hit = page.locator("main .search-results a");
  await expect(hit).toHaveCount(1);
  await hit.click();
  await expect(page).toHaveURL(
    server.base + "docs/tree/Shapes#val%3Aperimeter",
  );
});

test("header: typing a new query then Enter at once doesn't open the previous query's result (C1)", async ({
  page,
  startServer,
  docsProject,
}) => {
  const server = await startServer({ root: docsProject.root });
  await page.goto(server.base);
  const box = headerBox(page);
  await box.fill("Other");
  await expect(headerResults(page).first()).toBeVisible();
  // No waiting for the debounce: both browsers used to open "Other"
  // (the previous query's own, already-on-screen result) instead.
  await box.fill("perimeter");
  await box.press("Enter");
  await expect(page).toHaveURL(
    server.base + "docs/tree/Shapes#val%3Aperimeter",
  );
});

test("header: Enter does nothing while the popup is closed (C2)", async ({
  page,
  startServer,
  docsProject,
}) => {
  const server = await startServer({ root: docsProject.root });
  await page.goto(server.base);
  const box = headerBox(page);
  await box.fill("Other");
  await expect(headerResults(page).first()).toBeVisible();
  await box.press("Escape"); // closes, keeps the text
  await expect(box).toHaveAttribute("aria-expanded", "false");
  await box.press("Enter");
  await expect(page).toHaveURL(server.base);
});

test("header: the operator <+> is found and its anchor is encoded correctly", async ({
  page,
  startServer,
  docsProject,
}) => {
  const server = await startServer({ root: docsProject.root });
  await page.goto(server.base + "docs/tree/Shapes");
  const box = headerBox(page);
  await box.fill("<+>");
  await expect(headerResults(page).first()).toBeVisible();
  await box.press("Enter");
  await expect(page).toHaveURL(
    server.base + "docs/tree/Shapes#val%3A%3C%2B%3E",
  );
  await expect(decl(page, "(<+>)")).toBeFocused();
});

test("header: choosing the declaration already shown re-focuses it (C7)", async ({
  page,
  startServer,
  docsProject,
}) => {
  const server = await startServer({ root: docsProject.root });
  await page.goto(server.base + "docs/tree/Shapes#val%3Aperimeter");
  await expect(decl(page, "perimeter")).toBeFocused();
  const box = headerBox(page);
  await box.fill("perimeter");
  await expect(headerResults(page).first()).toBeVisible();
  // Same URL: the router would otherwise do nothing at all.
  await box.press("Enter");
  await expect(page).toHaveURL(
    server.base + "docs/tree/Shapes#val%3Aperimeter",
  );
  await expect(decl(page, "perimeter")).toBeFocused();
});

test("header: no match shows a visible message, not an empty popup (C5)", async ({
  page,
  startServer,
  docsProject,
}) => {
  const server = await startServer({ root: docsProject.root });
  await page.goto(server.base);
  const box = headerBox(page);
  await box.fill("zzzzqqq");
  const popup = page.locator(".frame-header .search-popup");
  await expect(popup).toBeVisible();
  await expect(popup.locator(".search-status")).toHaveText("No results.");
  const box2 = await popup.boundingBox();
  expect(box2?.height ?? 0).toBeGreaterThan(10);
});

test("header box shrinks at narrow widths, no sideways scroll from it (C10)", async ({
  page,
  startServer,
  docsProject,
}) => {
  const server = await startServer({ root: docsProject.root });
  await page.setViewportSize({ width: 360, height: 700 });
  await page.goto(server.base);
  const box = headerBox(page);
  await expect(box).toBeVisible();
  await expect(box).toHaveAttribute("placeholder", "Search");
  await box.fill("Shapes");
  await expect(headerResults(page).first()).toBeVisible();
  // Scoped to the header: the sidebar's own minimum width (unrelated
  // to this task) already keeps the whole page wider than 360px, so
  // this only checks the search box's own contribution.
  const header = await page.evaluate(() => {
    const h = document.querySelector(".frame-header");
    if (h === null) throw new Error("no header");
    return { scrollWidth: h.scrollWidth, clientWidth: h.clientWidth };
  });
  expect(header.scrollWidth).toBeLessThanOrEqual(header.clientWidth);
});

test("header: a pending Enter is cancelled by a sidebar click before a delayed answer lands (round 2, #1)", async ({
  page,
  startServer,
  docsProject,
}) => {
  const server = await startServer({ root: docsProject.root });
  await page.goto(server.base);
  // "perimeter" answers for real, just 1.5 s late: long enough that
  // it's still in flight when we click away.
  await page.route("**/api/search/tree?q=perimeter", async (route) => {
    await new Promise((resolve) => setTimeout(resolve, 1500));
    await route.continue();
  });
  const box = headerBox(page);
  await box.fill("Other");
  await expect(headerResults(page).first()).toBeVisible();
  await box.fill("perimeter");
  await box.press("Enter"); // pending: the delayed fetch hasn't answered
  await sidebar(page)
    .getByRole("link", { name: "Shapes.hs", exact: true })
    .click();
  await expect(page).toHaveURL(server.base + "docs/tree/Shapes");
  // Give the delayed answer time to land; it must not navigate us
  // away from where the sidebar click took us.
  await page.waitForTimeout(2000);
  await expect(page).toHaveURL(server.base + "docs/tree/Shapes");
});

test("header: choosing the declaration already shown by mouse doesn't scroll to the top (round 2, #2)", async ({
  page,
  startServer,
  docsProject,
}) => {
  const server = await startServer({ root: docsProject.root });
  await page.goto(server.base + "docs/tree/Shapes#val%3Aperimeter");
  await expect(decl(page, "perimeter")).toBeFocused();
  // Scroll away first, so a spurious scroll-to-top (the bug) is
  // distinguishable from "it just never moved".
  await page.evaluate(() => window.scrollTo(0, document.body.scrollHeight));
  await expect
    .poll(() => page.evaluate(() => window.scrollY))
    .toBeGreaterThan(0);
  const box = headerBox(page);
  await box.fill("perimeter");
  const hit = headerResults(page).first();
  await expect(hit).toBeVisible();
  await hit.click();
  await expect(page).toHaveURL(
    server.base + "docs/tree/Shapes#val%3Aperimeter",
  );
  await expect(decl(page, "perimeter")).toBeFocused();
  await expect
    .poll(() => page.evaluate(() => window.scrollY))
    .toBeGreaterThan(0);
});
