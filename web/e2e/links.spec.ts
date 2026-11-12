// Token links and the token cursor in the source view (T08b, G3, G4,
// G7; D-283, D-284, D-286).
import type { Locator, Page } from "@playwright/test";
import {
  type At,
  copy,
  recordCopies,
  select,
  selectAt,
  textSlice,
} from "./support/copy";
import { Decoder, Encoder } from "cbor-x";
import {
  expectNoAxeViolations as axe,
  fakeEventSource,
  themes,
} from "./support/a11y";
import { expect, test } from "./support/fixtures";
import { lineOf, lines, linksMainHs, linksTypesHs } from "./support/project";

const mainUrl = "src/tree/Links/Main.hs";
const typesUrl = "src/tree/Links/Types.hs";

// As the server sends it: plain objects, no records.
const cborOpts = { useRecords: false, mapsAsObjects: true } as const;
const cbor = {
  decode: (b: Uint8Array) => new Decoder(cborOpts).decode(b) as unknown,
  encode: (v: unknown) => new Encoder(cborOpts).encode(v),
};

function view(page: Page): Locator {
  return page.locator("main .code-view");
}

function row(page: Page, n: number): Locator {
  return view(page).locator(`.code-row[data-line="${n}"]`);
}

function cell(page: Page, n: number): Locator {
  return row(page, n).locator(".code-cell");
}

// The line of Main.hs starting so.
const line = (start: string) => lineOf(linksMainHs, start);

async function open(page: Page, url: string): Promise<void> {
  await page.goto(url);
  await expect(view(page).locator("a.code-link").first()).toBeAttached();
}

// A gate: wait() until open().
function gate(): { wait: Promise<void>; open: () => void } {
  let open: (() => void) | undefined;
  const wait = new Promise<void>((r) => (open = r));
  return { wait, open: () => open?.() };
}

test("Shift+click on a link extends the selection, no navigation", async ({
  page,
  startServer,
  linksProject,
}) => {
  const server = await startServer({ root: linksProject.root });
  await open(page, server.base + mainUrl);
  await recordCopies(page);
  const n = line("    go ys");
  const col = lines(linksMainHs)[n - 1]!.indexOf("area");
  const a: At = [line("g r"), 0];
  await cell(page, a[0]).click({ position: { x: 1, y: 5 } });
  const link = cell(page, n).getByRole("link", { name: /^area, / });
  const box = await link.boundingBox();
  if (box === null) throw new Error("no box");
  await page.keyboard.down("Shift");
  await page.mouse.click(box.x + 1, box.y + box.height / 2);
  await page.keyboard.up("Shift");
  // Ends at the start of "area" or just after it, as the browser puts it.
  expect([
    textSlice(linksMainHs, a, [n, col]),
    textSlice(linksMainHs, a, [n, col + 1]),
  ]).toContain(await copy(page));
  await expect(page).toHaveURL(server.base + mainUrl);
  expect(page.context().pages()).toHaveLength(1);
  // The cursor is on the link: Tab leaves the view from there.
  await expect(link).toBeFocused();
});

// Answers api/source through the real server, changed by edit (the
// call number, the decoded answer) and encoded again. The first call
// waits for held.
async function editSource(
  page: Page,
  edit: (call: number, v: Record<string, unknown>) => void,
  held: Promise<void> = Promise.resolve(),
): Promise<{ calls: () => number; generation: () => number }> {
  let calls = 0;
  let generation = 0;
  await page.route("**/api/source/**", async (route) => {
    if (calls === 0) await held;
    const res = await route.fetch();
    const v = cbor.decode(await res.body()) as Record<string, unknown>;
    calls++;
    generation = v.generation as number;
    edit(calls, v);
    await route.fulfill({ response: res, body: Buffer.from(cbor.encode(v)) });
  });
  return { calls: () => calls, generation: () => generation };
}

async function emit(page: Page, type: string, data?: unknown): Promise<void> {
  await page.evaluate(({ type: t, data: d }) => window.__hrEmit?.(t, d), {
    type,
    data,
  });
}

test("a late highlight keeps focus on the line; a redraw on a link", async ({
  page,
  context,
  startServer,
  linksProject,
}) => {
  await fakeEventSource(context);
  const server = await startServer({ root: linksProject.root });
  const held = gate();
  // The first answer: partial, no outline. The refetch: the real one,
  // so the rows redraw.
  const src = await editSource(
    page,
    (call, v) => {
      if (call === 1) {
        v.partial = true;
        v.outline = [];
      }
    },
    held.wait,
  );
  await page.goto(server.base + mainUrl);
  await emit(page, "hello", {});
  const n = line("total xs");
  await expect(cell(page, n)).toBeVisible();
  await cell(page, n).click({ position: { x: 1, y: 5 } });
  await expect(cell(page, n)).toBeFocused();
  held.open();
  await expect(cell(page, n).locator("a.code-link").first()).toBeAttached();
  // Still the cell: the cursor was on it.
  await expect(cell(page, n)).toBeFocused();
  await expect(page.locator(".code-outline")).toHaveCount(0);
  // On a link, a redraw keeps focus on the new one.
  // total (a definition: docs), then go (xs is a binder: no link).
  await page.keyboard.press("ArrowRight");
  await expect(
    cell(page, n).getByRole("link", { name: "total, documentation" }),
  ).toBeFocused();
  await page.keyboard.press("ArrowRight");
  const go = cell(page, n).getByRole("link", { name: /^go, / });
  await expect(go).toBeFocused();
  const old = await go.elementHandle();
  await emit(page, "indexing", {
    generation: src.generation(),
    total: 2,
    ready: 2,
    failed: 0,
  });
  await expect.poll(src.calls).toBe(2);
  await expect(page.locator(".code-outline")).toBeVisible();
  expect(await old?.evaluate((el) => el.isConnected)).toBe(false);
  await expect(go).toBeFocused();
  await expect(go).toHaveAttribute("tabindex", "0");
});

test("cross-module go to definition: click, Back, Enter", async ({
  page,
  startServer,
  linksProject,
}) => {
  const server = await startServer({ root: linksProject.root });
  await open(page, server.base + mainUrl);
  const n = line("    go ys");
  const area = cell(page, n).getByRole("link", {
    name: "area, defined in Links/Types.hs line 5",
  });
  await expect(area).toHaveAttribute("href", `/${server.token}/${typesUrl}#L5`);
  await area.click();
  await expect(page).toHaveURL(server.base + typesUrl + "#L5");
  await expect(page.locator("main h1")).toHaveText("Links/Types.hs");
  await expect(row(page, 5)).toHaveClass(/code-line-target/);
  // Focus on the target line: its first stop (area's own name, docs),
  // or the cell if the highlight came later.
  const onLine5 = () =>
    row(page, 5).evaluate((r) => r.contains(document.activeElement));
  await expect.poll(onLine5).toBe(true);
  await expect(cell(page, lineOf(linksTypesHs, "area (Circle"))).toBeAttached();
  await page.goBack();
  await expect(page).toHaveURL(server.base + mainUrl);
  await expect(area).toBeVisible();
  // By keyboard: the link, Enter.
  await area.focus();
  await page.keyboard.press("Enter");
  await expect(page).toHaveURL(server.base + typesUrl + "#L5");
  await expect.poll(onLine5).toBe(true);
});

test("a definition links to its docs, a binder to nothing, a use to its binder", async ({
  page,
  startServer,
  linksProject,
}) => {
  const server = await startServer({ root: linksProject.root });
  await open(page, server.base + mainUrl);
  const n = line("total xs");
  // "xs" twice: the binder isn't a link, the use is.
  await expect(cell(page, n).locator("a.code-link")).toHaveText([
    "total",
    "go",
    "xs",
  ]);
  const go = cell(page, n).getByRole("link", { name: "go, bound at line 16" });
  await go.click();
  await expect(page).toHaveURL(server.base + mainUrl + "#L16");
  // The binder's line, on the cell (the binder is no link).
  await expect(cell(page, 16)).toBeFocused();
  // The page didn't jump to the top: the view is on screen.
  await expect(row(page, 16)).toBeInViewport();
  const total = cell(page, n).getByRole("link", {
    name: "total, documentation",
  });
  await total.click();
  await expect(page).toHaveURL(
    server.base + "docs/tree/Links.Main#val%3Atotal",
  );
});

test("a guess: dotted underline, a title, said in its name", async ({
  page,
  startServer,
  linksProject,
}) => {
  const server = await startServer({ root: linksProject.root });
  await open(page, server.base + mainUrl);
  const f = cell(page, line("g r")).getByRole("link", {
    name: "f, defined in Links/Main.hs line 8, resolved heuristically",
  });
  await expect(f).toHaveClass(/code-link-guess/);
  await expect(f).toHaveAttribute("title", "Resolved heuristically");
  await expect(f).toHaveCSS("text-decoration-style", "dotted");
  await expect(f).toHaveCSS("text-decoration-line", "underline");
  // An exact one: no underline at rest, no title.
  const g = cell(page, line("g r")).getByRole("link", {
    name: "g, documentation",
  });
  await expect(g).toHaveCSS("text-decoration-line", "none");
  await expect(g).not.toHaveAttribute("title", /./);
});

test("external: Hoogle in a new tab, noopener", async ({
  page,
  context,
  startServer,
  linksProject,
}) => {
  await context.route("https://hoogle.haskell.org/**", (route) =>
    route.fulfill({ status: 200, contentType: "text/plain", body: "hoogle" }),
  );
  const server = await startServer({ root: linksProject.root });
  await open(page, server.base + mainUrl);
  const sort = cell(page, line("sorted =")).getByRole("link", {
    name: "sort, Data.List, search on Hoogle, opens in a new tab",
  });
  const href =
    "https://hoogle.haskell.org/?hoogle=sort%20%2Bmodule%3AData.List";
  await expect(sort).toHaveAttribute("href", href);
  await expect(sort).toHaveAttribute("target", "_blank");
  await expect(sort).toHaveAttribute("rel", "noopener noreferrer");
  await expect(
    cell(page, line("up =")).getByRole("link", {
      name: "toUpper, one of 2 modules, search on Hoogle, resolved heuristically, opens in a new tab",
    }),
  ).toHaveAttribute("href", "https://hoogle.haskell.org/?hoogle=toUpper");
  const popup = context.waitForEvent("page");
  await sort.click();
  const tab = await popup;
  await expect(tab).toHaveURL(href);
  expect(await tab.evaluate(() => window.opener === null)).toBe(true);
  await expect(page).toHaveURL(server.base + mainUrl);
});

test("keyboard: links are the stops, one Tab stop, goal column", async ({
  page,
  startServer,
  linksProject,
}) => {
  const server = await startServer({ root: linksProject.root });
  await open(page, server.base + mainUrl);
  const at = (n: number, name: string) =>
    cell(page, n).getByRole("link", { name: new RegExp(`^${name}, `) });
  const n = line("    go ys");
  // Column 0 is spaces: the cell.
  await cell(page, n).click({ position: { x: 1, y: 5 } });
  await expect(cell(page, n)).toBeFocused();
  // Binders (go, ys) are no stops.
  await page.keyboard.press("ArrowRight");
  await expect(at(n, "sum")).toBeFocused();
  await page.keyboard.press("ArrowRight");
  await expect(at(n, "map")).toBeFocused();
  await page.keyboard.press("ArrowLeft");
  await expect(at(n, "sum")).toBeFocused();
  // Before the first link: the cell, then nothing (focus stays).
  await page.keyboard.press("ArrowLeft");
  await expect(cell(page, n)).toBeFocused();
  await page.keyboard.press("ArrowLeft");
  await expect(cell(page, n)).toBeFocused();
  // At the last link, → keeps it.
  for (let i = 0; i < 5; i++) await page.keyboard.press("ArrowRight");
  await expect(cell(page, n).locator("a.code-link").last()).toBeFocused();
  for (let i = 0; i < 3; i++) await page.keyboard.press("ArrowLeft");
  await expect(at(n, "sum")).toBeFocused();
  // Up: "  where" has no link under sum's column, the cell; then go,
  // which holds the goal column.
  await page.keyboard.press("ArrowUp");
  await expect(cell(page, n - 1)).toBeFocused();
  await page.keyboard.press("ArrowUp");
  const go = at(n - 2, "go");
  await expect(go).toBeFocused();
  // One element in the view has tabindex 0.
  await expect(view(page).locator('[tabindex="0"]')).toHaveCount(1);
  await expect(go).toHaveAttribute("tabindex", "0");
  // Shift+Tab leaves in one step, Tab comes back to the link.
  await page.keyboard.press("Shift+Tab");
  expect(
    await view(page).evaluate((v) => v.contains(document.activeElement)),
  ).toBe(false);
  await page.keyboard.press("Tab");
  await expect(go).toBeFocused();
  // PageDown: a row off screen, the link under the goal (Circle).
  await page.keyboard.press("PageDown");
  const focused = view(page).locator("a.code-link:focus");
  await expect(focused).toHaveAccessibleName(
    "Circle, defined in Links/Types.hs line 3",
  );
  await expect(focused).toBeInViewport();
  const where = await focused.evaluate((a) =>
    Number(a.closest(".code-row")?.getAttribute("data-line")),
  );
  expect(where).toBeGreaterThan(n + 10);
  await page.keyboard.press("Enter");
  await expect(page).toHaveURL(server.base + typesUrl + "#L3");
  await expect
    .poll(() =>
      row(page, 3).evaluate((r) => r.contains(document.activeElement)),
    )
    .toBe(true);
});

test("a drag from a link selects, exact copy, no navigation", async ({
  page,
  startServer,
  linksProject,
}) => {
  const server = await startServer({ root: linksProject.root });
  await open(page, server.base + mainUrl);
  await recordCopies(page);
  const n = line("    go ys");
  const col = lines(linksMainHs)[n - 1]!.indexOf("area");
  const a = await cell(page, n)
    .getByRole("link", { name: /^area, / })
    .boundingBox();
  const b = await cell(page, n + 3).boundingBox();
  if (a === null || b === null) throw new Error("no box");
  await page.mouse.move(a.x + 1, a.y + a.height / 2);
  await page.mouse.down();
  await page.mouse.move(b.x + b.width - 2, b.y + b.height / 2, { steps: 5 });
  await page.mouse.up();
  const end = lines(linksMainHs)[n + 2]!.length;
  expect([
    textSlice(linksMainHs, [n, col], [n + 3, end]),
    textSlice(linksMainHs, [n, col + 1], [n + 3, end]),
  ]).toContain(await copy(page));
  await expect(page).toHaveURL(server.base + mainUrl);
});

test("partial: refetch on indexing done, once; another snapshot: none", async ({
  page,
  context,
  startServer,
  linksProject,
}) => {
  await fakeEventSource(context);
  const server = await startServer({ root: linksProject.root });
  // Every answer partial.
  const src = await editSource(page, (_, v) => {
    v.partial = true;
  });
  await open(page, server.base + mainUrl);
  await emit(page, "hello", {});
  const done = (generation: number) => ({
    generation,
    total: 2,
    ready: 2,
    failed: 0,
  });
  await emit(page, "indexing", done(src.generation()));
  await expect.poll(src.calls).toBe(2);
  await emit(page, "indexing", done(src.generation()));
  await emit(page, "indexing", { ...done(src.generation() + 1), ready: 1 });
  await page.waitForTimeout(1200);
  await emit(page, "indexing", { ...done(src.generation() + 1), ready: 1 });
  await page.waitForTimeout(300);
  expect(src.calls()).toBe(2);
});

for (const t of themes) {
  test(`axe with a focused link, ${t.scheme} theme`, async ({
    page,
    startServer,
    linksProject,
  }) => {
    const server = await startServer({ root: linksProject.root });
    await page.emulateMedia({ colorScheme: t.scheme });
    await open(page, server.base + mainUrl);
    const f = cell(page, line("g r")).getByRole("link", { name: /^f, / });
    await f.focus();
    await page.keyboard.press("ArrowLeft");
    await page.keyboard.press("ArrowRight");
    await expect(f).toBeFocused();
    await axe(page, `links, focused, ${t.scheme}`);
  });
}

test("a drag from a link below the view scrolls it and selects on", async ({
  page,
  startServer,
  linksProject,
}) => {
  const server = await startServer({ root: linksProject.root });
  await open(page, server.base + mainUrl);
  await recordCopies(page);
  const n = line("    go ys");
  const col = lines(linksMainHs)[n - 1]!.indexOf("area");
  const a = await cell(page, n)
    .getByRole("link", { name: /^area, / })
    .boundingBox();
  const v = await view(page).boundingBox();
  if (a === null || v === null) throw new Error("no box");
  // Below the view but inside the window: Firefox sends no mousemove
  // outside it, and the view ends near the window's bottom.
  const h = page.viewportSize()!.height;
  const below = Math.min(v.y + v.height + 30, h - 2);
  expect(below).toBeGreaterThan(v.y + v.height + 2);
  await page.mouse.move(a.x + 1, a.y + a.height / 2);
  await page.mouse.down();
  await page.mouse.move(v.x + 50, below, { steps: 5 });
  await expect
    .poll(() => view(page).evaluate((el) => el.scrollTop))
    .toBeGreaterThan(200);
  await page.mouse.up();
  const text = await copy(page);
  expect(text).not.toBeNull();
  // From area on, and on past the lines first on screen.
  expect(
    [col, col + 1].map((c) => textSlice(linksMainHs, [n, c], [n + 1, 0])),
  ).toContain(text!.slice(0, text!.indexOf("\n") + 1));
  // Past the last line on screen at the start (rows are 1.5em).
  const rowH = await row(page, n).evaluate(
    (r) => r.getBoundingClientRect().height,
  );
  const end = n + text!.split("\n").length - 1;
  expect(end).toBeGreaterThan(v.height / rowH + 5);
  await expect(page).toHaveURL(server.base + mainUrl);
});

test("Shift+click on a link with focus outside the view", async ({
  page,
  startServer,
  linksProject,
}) => {
  const server = await startServer({ root: linksProject.root });
  await open(page, server.base + mainUrl);
  await recordCopies(page);
  const n = line("    go ys");
  const col = lines(linksMainHs)[n - 1]!.indexOf("area");
  const a: At = [line("g r"), 0];
  // Focus first: WebKit drops a selection when focus moves.
  await page.locator("main h1").focus();
  await selectAt(page, "start", ...a);
  const link = cell(page, n).getByRole("link", { name: /^area, / });
  const box = await link.boundingBox();
  if (box === null) throw new Error("no box");
  await page.keyboard.down("Shift");
  await page.mouse.click(box.x + 1, box.y + box.height / 2);
  await page.keyboard.up("Shift");
  expect([
    textSlice(linksMainHs, a, [n, col]),
    textSlice(linksMainHs, a, [n, col + 1]),
  ]).toContain(await copy(page));
  await expect(link).toBeFocused();
  await expect(page).toHaveURL(server.base + mainUrl);
});

test("Ctrl+click on a link with a selection is left to the browser", async ({
  page,
  startServer,
  linksProject,
}) => {
  const server = await startServer({ root: linksProject.root });
  await open(page, server.base + mainUrl);
  await page.evaluate(() => {
    // Last to see the click: note it, and stop the new tab.
    window.addEventListener("click", (e) => {
      (window as unknown as { __prevented: boolean }).__prevented =
        e.defaultPrevented;
      e.preventDefault();
    });
  });
  await select(page, [line("g r"), 0], [line("g r"), 5]);
  const link = cell(page, line("    go ys")).getByRole("link", {
    name: /^area, /,
  });
  await link.click({ modifiers: ["ControlOrMeta"] });
  expect(
    await page.evaluate(
      () => (window as unknown as { __prevented: boolean }).__prevented,
    ),
  ).toBe(false);
  await expect(page).toHaveURL(server.base + mainUrl);
});

test("a line's name: its text, links by their names", async ({
  page,
  startServer,
  linksProject,
}) => {
  const server = await startServer({ root: linksProject.root });
  await open(page, server.base + mainUrl);
  await expect(cell(page, line("sorted ="))).toHaveAccessibleName(
    "sorted, documentation = sort, Data.List, search on Hoogle, opens in a new tab",
  );
});
