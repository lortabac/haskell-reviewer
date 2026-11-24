// Diff mode: every page URL has its side, and links stay on it (D-332,
// SPEC §4.1, §5.1). fixtures/repos/sides.sh with --diff HEAD~1..HEAD:
// base has A (doc "One.", a = b) and B; head has A (doc "Two.",
// a = c + 1) and C.
import type { Page } from "@playwright/test";
import { expect, moduleNotFound404, sidebar, test } from "./support/fixtures";
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

// Every in-app link in main, the sidebar and the search popup stays on
// side. Hoogle links are left out. The sidebar's ghosts link to the
// other side, and say so (D-337).
async function expectLinkSides(page: Page, side: "base" | "head") {
  const links = await page.evaluate(() => {
    const token = location.pathname.split("/")[1];
    return [
      ...document.querySelectorAll(
        "main a[href], nav a[href], .frame-header .search-results a[href]",
      ),
    ]
      .map((a) => ({
        href: a.getAttribute("href") ?? "",
        ghost: a.classList.contains("tree-ghost"),
        text: a.textContent,
      }))
      .filter((l) => l.href.startsWith(`/${token}/`))
      .map((l) => ({ ...l, side: l.href.split(/[/?#]/)[3] ?? "" }));
  });
  const other = side === "base" ? "head" : "base";
  // The diff view's URLs have no side segment (D-341), but a line hash
  // names one: it must be the page's.
  const isDiff = (l: { href: string }) => l.href.includes("/diff/");
  for (const l of links.filter(isDiff)) {
    const hashSide = /#(base|head)-L/.exec(l.href)?.[1];
    if (hashSide !== undefined) expect(hashSide, l.href).toBe(side);
  }
  expect(
    new Set(links.filter((l) => !l.ghost && !isDiff(l)).map((l) => l.side)),
  ).toEqual(new Set([side]));
  for (const g of links.filter((l) => l.ghost)) {
    expect(g.side, g.href).toBe(other);
    expect(g.text, g.href).toContain(`(on ${other})`);
  }
}

test("docs on both sides; an in-app side change shows the other side", async ({
  page,
  startServer,
  sidesProject,
}) => {
  const server = await start(startServer, sidesProject.root);
  await page.goto(server.base + "docs/base/A");
  await expect(page.locator("main")).toContainText("One.");
  await page.goto(server.base + "docs/head/A");
  await expect(page.locator("main")).toContainText("Two.");
  // Same ref, other side, no reload: the page is made again.
  await page.evaluate(() => {
    const [, token] = location.pathname.split("/");
    history.pushState(null, "", `/${token}/docs/base/A`);
    dispatchEvent(new PopStateEvent("popstate"));
  });
  await expect(page.locator("main")).toContainText("One.");
  await expect(page.locator("main")).not.toContainText("Two.");
  await page.goBack();
  await expect(page.locator("main")).toContainText("Two.");
});

test("links never switch side", async ({ page, startServer, sidesProject }) => {
  const server = await start(startServer, sidesProject.root);
  for (const [side, name, to] of [
    ["base", "b", "B"],
    ["head", "c", "C"],
  ] as const) {
    await page.goto(server.base + `docs/${side}/A`);
    await expect(sidebar(page).locator("a").first()).toBeVisible();
    // Inline source: the name goes to its docs on the same side.
    await page.locator("main .decl-source-toggle").first().click();
    const link = page.locator("main pre.src").getByRole("link", {
      name: new RegExp(`^${name}\\b`),
    });
    await expect(link).toHaveAttribute(
      "href",
      new RegExp(`/docs/${side}/${to}#val%3A${name}$`),
    );
    await expectLinkSides(page, side);
    await link.click();
    await expect(page).toHaveURL(new RegExp(`/docs/${side}/${to}#`));
    await expect(page.locator("main h1")).toHaveText(to);
    await expectLinkSides(page, side);
  }
  // The source view: a token links to base.
  await page.goto(server.base + "src/base/src/A.hs");
  const b = page.locator("main .code-view").getByRole("link", { name: /^b\b/ });
  await expect(b.first()).toHaveAttribute(
    "href",
    /\/src\/base\/src\/B\.hs#L3$/,
  );
  await expectLinkSides(page, "base");
  await b.first().click();
  await expect(page).toHaveURL(/\/src\/base\/src\/B\.hs#L3$/);
});

test("search, text search and refs use the page's side", async ({
  page,
  startServer,
  sidesProject,
}) => {
  const server = await start(startServer, sidesProject.root);
  const box = page
    .locator(".frame-header")
    .getByRole("combobox", { name: "Search modules and names" });
  const names = page.locator(".frame-header .search-results a[data-kind=name]");
  // B is a module on base only.
  await page.goto(server.base + "docs/base/A");
  await box.fill("B");
  await expect(names.filter({ hasText: /^B/ }).first()).toHaveAttribute(
    "href",
    /\/docs\/base\/B$/,
  );
  await expectLinkSides(page, "base");
  await page.goto(server.base + "docs/head/A");
  await box.fill("B");
  await expect(page.locator(".frame-header .search-status")).toHaveText(
    /results?\.|No results\./,
  );
  await expect(
    page.locator(
      ".frame-header .search-results a[data-kind=name][href$='/docs/head/B']",
    ),
  ).toHaveCount(0);
  await expectLinkSides(page, "head");
  // Text search: "c + 1" is on head only.
  await page.goto(server.base + "search/head?q=c%20%2B%201");
  await expect(page.locator("main")).toContainText("1 matching line in 1 file");
  await expect(
    page.locator("main").getByRole("link", { name: /a = c \+ 1/ }),
  ).toHaveAttribute("href", /\/src\/head\/src\/A\.hs#L7$/);
  await page.goto(server.base + "search/base?q=c%20%2B%201");
  await expect(page.locator("main")).toContainText("No matches");
  // Refs of b, from base: base rows.
  await page.goto(server.base + "refs/base/B?decl=val%3Ab");
  await expect(page.locator("main")).toContainText("5 references in 2 modules");
  await expectLinkSides(page, "base");
});

test("wrong side: not found, with one link to head", async ({
  page,
  startServer,
  sidesProject,
  guard,
}) => {
  guard.allow(moduleNotFound404);
  guard.allow(
    /Failed to load resource: the server responded with a status of 404.* \[\S*\/api\/module\/head\/B/,
  );
  const server = await start(startServer, sidesProject.root);
  const requests: string[] = [];
  page.on("request", (r) => requests.push(r.url()));
  await page.goto(server.base + "docs/tree/A");
  const main = page.locator("main");
  await expect(main.locator("h1")).toHaveText("Not found in this snapshot");
  await expect(main).toContainText("there is no “tree” side");
  await expect(main.getByRole("search").getByRole("combobox")).toHaveValue("A");
  expect(requests.filter((u) => u.includes("/api/module/"))).toEqual([]);
  expect(
    requests.filter((u) => u.includes("/api/") && u.includes("/tree")),
  ).toEqual([]);
  await main.getByRole("link", { name: "Open on head" }).click();
  await expect(page).toHaveURL(/\/docs\/head\/A$/);
  await expect(main).toContainText("Two.");
  // A side that exists, a module that doesn't.
  await page.goto(server.base + "docs/head/B");
  await expect(main.locator("h1")).toHaveText("Not found in this snapshot");
});

test("browse mode: base and head are not found", async ({ page, server }) => {
  await page.goto(server.base + "docs/head/Main");
  const main = page.locator("main");
  await expect(main.locator("h1")).toHaveText("Not found in this snapshot");
  await expect(main).toContainText("Not in diff mode.");
  await expect(
    main.getByRole("link", { name: "Open on tree" }),
  ).toHaveAttribute("href", /\/docs\/tree\/Main$/);
});
