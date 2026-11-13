// Find references (T12; D-296): the refs page, the References button
// on the cursor token, the local bar. Keyboard first: Playwright's
// Firefox sends no mouse events outside the window, and WebKit drops
// the selection when focus moves (assert focus, not selection).
import type { Locator, Page } from "@playwright/test";
import {
  expectNoAxeViolations as axe,
  fakeEventSource,
  noSideways,
  themes,
} from "./support/a11y";
import { expect, test } from "./support/fixtures";
import { lineOf, linksMainHs } from "./support/project";

const docsUrl = "docs/tree/Links.Types";
const mainUrl = "src/tree/Links/Main.hs";
const refsUrl = "refs/tree/Links.Types?decl=val%3Aarea";

const view = (page: Page): Locator => page.locator("main .code-view");
const row = (page: Page, n: number): Locator =>
  view(page).locator(`.code-row[data-line="${n}"]`);
const cell = (page: Page, n: number): Locator =>
  row(page, n).locator(".code-cell");
const line = (start: string) => lineOf(linksMainHs, start);
const refsButton = (page: Page): Locator =>
  page.locator("main .code-refs-button");
const bar = (page: Page): Locator => page.locator("main .code-refs-bar");
const group = (page: Page, name: string | RegExp): Locator =>
  page.locator("main .refs-group").filter({
    has: page.getByRole("heading", { level: 2, name }),
  });

async function openSource(page: Page, base: string): Promise<void> {
  await page.goto(base + mainUrl);
  await expect(view(page).locator("a.code-link").first()).toBeAttached();
}

test("docs: Find references goes to the page; Enter on a use lands on the token", async ({
  page,
  startServer,
  linksProject,
}) => {
  const server = await startServer({ root: linksProject.root });
  await page.goto(server.base + docsUrl);
  const link = page.getByRole("link", { name: "Find references of area" });
  await link.focus();
  await page.keyboard.press("Enter");
  await expect(page).toHaveURL(server.base + refsUrl);
  await expect(page.locator("main h1")).toHaveText(
    "References to area (value) in Links.Types",
  );
  await expect(page.locator("main .refs-summary")).toHaveText(
    /^\d+ references in 2 modules\.$/,
  );
  await expect(page.locator("main .refs-tier")).toContainText("Syntactic");
  await expect(group(page, /^Links\.Types/)).toContainText("declared here");
  const main = group(page, /^Links\.Main/);
  await expect(main.locator("li")).toHaveCount(201);
  const first = main.locator("li a").first();
  await expect(first).toHaveAttribute(
    "href",
    `/${server.token}/${mainUrl}#L${line("    go ys")}`,
  );
  await expect(first).toContainText("go ys = sum (map area ys)");
  await expect(first.locator("mark")).toHaveText("area");
  await first.focus();
  await page.keyboard.press("Enter");
  const n = line("    go ys");
  await expect(page).toHaveURL(server.base + mainUrl + `#L${n}`);
  // On the token, not the line start.
  await expect(
    cell(page, n).getByRole("link", { name: /^area, / }),
  ).toBeFocused();
  // Back: the item we followed has focus again.
  await page.goBack();
  await expect(page).toHaveURL(server.base + refsUrl);
  await expect(first).toBeFocused();
});

test("a chain: refs, file, refs, file; Back focuses each row followed", async ({
  page,
  startServer,
  linksProject,
}) => {
  const server = await startServer({ root: linksProject.root });
  await page.goto(server.base + refsUrl);
  const items = group(page, /^Links\.Main/).locator("li a");
  const first = items.first();
  await first.focus();
  await page.keyboard.press("Enter");
  const n = line("    go ys");
  await expect(page).toHaveURL(server.base + mainUrl + `#L${n}`);
  // The cursor is on the area link: References goes to the page again.
  await expect(
    cell(page, n).getByRole("link", { name: /^area, / }),
  ).toBeFocused();
  await refsButton(page).focus();
  await page.keyboard.press("Enter");
  await expect(page).toHaveURL(server.base + refsUrl);
  const third = group(page, /^Links\.Main/)
    .locator("li a")
    .nth(2);
  await third.focus();
  await page.keyboard.press("Enter");
  await expect(page).toHaveURL(/#L\d+$/);
  await page.goBack();
  await expect(page).toHaveURL(server.base + refsUrl);
  await expect(third).toBeFocused();
  await page.goBack();
  await expect(page).toHaveURL(server.base + mainUrl + `#L${n}`);
  await page.goBack();
  await expect(page).toHaveURL(server.base + refsUrl);
  await expect(first).toBeFocused();
});

test("source: a guess is a guess on the page; a reload gets the shell", async ({
  page,
  startServer,
  linksProject,
}) => {
  const server = await startServer({ root: linksProject.root });
  await openSource(page, server.base);
  const f = cell(page, line("g r")).getByRole("link", { name: /^f, / });
  await f.focus();
  await expect(refsButton(page)).toHaveText("References to f");
  await refsButton(page).focus();
  await page.keyboard.press("Enter");
  await expect(page).toHaveURL(/\/refs\/tree\/Links\.Main\?decl=/);
  await expect(page.locator("main h1")).toContainText("References to f");
  const g = page.locator("main .refs-item").filter({ hasText: "guess" });
  await expect(g.first()).toContainText("Resolved heuristically:");
  await page.reload();
  await expect(page.locator("main h1")).toContainText("References to f");
  await expect(g.first()).toBeVisible();
});

test("a local: the bar, Next, Esc puts focus on the hit", async ({
  page,
  startServer,
  linksProject,
}) => {
  const server = await startServer({ root: linksProject.root });
  await openSource(page, server.base);
  const n = line("    go ys");
  const ys = cell(page, n).getByRole("link", { name: /^ys, / });
  await ys.focus();
  await expect(refsButton(page)).toHaveText("References to ys");
  await refsButton(page).focus();
  await page.keyboard.press("Enter");
  await expect(bar(page)).toBeVisible();
  await expect(bar(page)).toContainText("ys, local");
  const next = bar(page).getByRole("button", { name: "Next" });
  await expect(next).toBeFocused();
  // The use was under the cursor: 2 of 2. Next wraps to the binder.
  await expect(bar(page)).toContainText("2 of 2");
  await page.keyboard.press("Enter");
  await expect(bar(page)).toContainText("1 of 2");
  await expect(page.locator("main .code-refs-status")).toContainText(
    `Reference 1 of 2, line ${n}, the binder`,
  );
  await page.keyboard.press("Escape");
  await expect(bar(page)).toBeHidden();
  // The binder is no link: its cell has focus.
  await expect(cell(page, n)).toBeFocused();
});

test("a local used far away: go from line 14 to its binder", async ({
  page,
  startServer,
  linksProject,
}) => {
  const server = await startServer({ root: linksProject.root });
  await openSource(page, server.base);
  const go = cell(page, line("total xs")).getByRole("link", { name: /^go, / });
  await go.focus();
  await refsButton(page).focus();
  await page.keyboard.press("Enter");
  await expect(bar(page)).toContainText("go, local");
  // The use (line 14) was under the cursor: 1 of 2. Next: the binder.
  await expect(bar(page)).toContainText("1 of 2");
  await page.keyboard.press("Enter");
  await expect(bar(page)).toContainText("2 of 2");
  await bar(page).getByRole("button", { name: "Close" }).press("Enter");
  await expect(bar(page)).toBeHidden();
  await expect(cell(page, line("    go ys"))).toBeFocused();
});

test("an outside name, no token: a status, no navigation", async ({
  page,
  startServer,
  linksProject,
}) => {
  const server = await startServer({ root: linksProject.root });
  await openSource(page, server.base);
  const up = cell(page, line("up =")).getByRole("link", { name: /^toUpper, / });
  await up.focus();
  await refsButton(page).focus();
  await page.keyboard.press("Enter");
  await expect(page.locator("main .code-refs-note")).toHaveText(
    "References need a name declared in this project.",
  );
  await expect(page).toHaveURL(server.base + mainUrl);
  await cell(page, 1).click({ position: { x: 1, y: 5 } });
  await expect(refsButton(page)).toHaveText("References");
  await refsButton(page).focus();
  await page.keyboard.press("Enter");
  await expect(page.locator("main .code-refs-note")).toHaveText(
    "Put the cursor on a name first.",
  );
});

test("find and references bars close each other", async ({
  page,
  startServer,
  linksProject,
}) => {
  const server = await startServer({ root: linksProject.root });
  await openSource(page, server.base);
  const ys = cell(page, line("    go ys")).getByRole("link", { name: /^ys, / });
  await ys.focus();
  await refsButton(page).focus();
  await page.keyboard.press("Enter");
  await expect(bar(page)).toBeVisible();
  await page.getByRole("button", { name: "Find", exact: true }).click();
  await expect(bar(page)).toBeHidden();
  await expect(
    page.getByRole("search", { name: "Find in file" }),
  ).toBeVisible();
});

// api/refs answered partial the first time; then the real one.
async function partialFirst(
  page: Page,
): Promise<{ calls: () => number; generation: () => number }> {
  let calls = 0;
  let generation = 0;
  await page.route("**/api/refs/**", async (route) => {
    const res = await route.fetch();
    const v = (await res.json()) as Record<string, unknown>;
    calls++;
    generation = v.generation as number;
    if (calls === 1) v.partial = true;
    await route.fulfill({ response: res, json: v });
  });
  return { calls: () => calls, generation: () => generation };
}

test("partial: asked again at done, the focused row stays", async ({
  page,
  context,
  startServer,
  linksProject,
}) => {
  await fakeEventSource(context);
  const server = await startServer({ root: linksProject.root });
  const refs = await partialFirst(page);
  await page.goto(server.base + refsUrl);
  await page.evaluate(() => window.__hrEmit?.("hello", {}));
  await expect(page.locator("main .refs-partial")).toContainText(
    "Still indexing: more references may come.",
  );
  const item = page.locator("main .refs-item a").nth(3);
  await item.focus();
  const text = await item.textContent();
  await page.evaluate(
    (generation) =>
      window.__hrEmit?.("indexing", {
        generation,
        total: 2,
        ready: 2,
        failed: 0,
      }),
    refs.generation(),
  );
  await expect.poll(refs.calls).toBe(2);
  await expect(page.locator("main .refs-partial")).toHaveCount(0);
  await expect(item).toBeFocused();
  await expect(item).toHaveText(text ?? "");
  await expect(
    page.locator("main .code-refs-status, main [role=status]").last(),
  ).toHaveText(/references in 2 modules\./);
});

test("a name the module doesn't declare: not found", async ({
  page,
  startServer,
  linksProject,
  guard,
}) => {
  guard.allow(/status of 404/);
  const server = await startServer({ root: linksProject.root });
  await page.goto(server.base + "refs/tree/Links.Types?decl=val%3Anope");
  await expect(page.locator("main")).toContainText(
    "No such declaration in this snapshot.",
  );
});

test("320 px: no sideways scroll", async ({
  page,
  startServer,
  linksProject,
}) => {
  const server = await startServer({ root: linksProject.root });
  await page.setViewportSize({ width: 320, height: 700 });
  await page.goto(server.base + refsUrl);
  await expect(page.locator("main .refs-item").first()).toBeVisible();
  await noSideways(page);
});

for (const t of themes) {
  test(`axe, ${t.scheme} theme`, async ({
    page,
    context,
    startServer,
    linksProject,
  }) => {
    await fakeEventSource(context);
    const server = await startServer({ root: linksProject.root });
    await page.emulateMedia({ colorScheme: t.scheme });
    // The refs page of the ambiguous f: a guess, and partial.
    await partialFirst(page);
    await page.goto(server.base + "refs/tree/Links.Main?decl=field%3AP.f");
    await page.evaluate(() => window.__hrEmit?.("hello", {}));
    await expect(page.locator("main .refs-partial")).toBeVisible();
    await expect(page.locator("main .refs-guess").first()).toBeVisible();
    await axe(page, `refs page, guess, partial, ${t.scheme}`);
    // The source view with the local bar open.
    await page.goto(server.base + mainUrl);
    const ys = cell(page, line("    go ys")).getByRole("link", {
      name: /^ys, /,
    });
    await ys.focus();
    await refsButton(page).focus();
    await page.keyboard.press("Enter");
    await expect(bar(page)).toBeVisible();
    await axe(page, `local refs bar, ${t.scheme}`);
    // The docs page with the links.
    await page.goto(server.base + "docs/tree/Links.Main");
    await expect(
      page.getByRole("link", { name: /^Find references of / }).first(),
    ).toBeVisible();
    await axe(page, `docs with refs links, ${t.scheme}`);
  });
}
