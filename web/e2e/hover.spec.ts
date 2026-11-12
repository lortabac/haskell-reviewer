// The hover card on token links (T08c, G12, WCAG 1.4.13; D-285).
// Pointer targets stay inside the window: Playwright's Firefox sends
// no mouse events outside it.
import type { Locator, Page } from "@playwright/test";
import { Decoder, Encoder } from "cbor-x";
import { copy, recordCopies, textSlice } from "./support/copy";
import {
  expectNoAxeViolations as axe,
  fakeEventSource,
  noSideways,
  themes,
} from "./support/a11y";
import { expect, test } from "./support/fixtures";
import { lineOf, lines, linksMainHs } from "./support/project";

const mainUrl = "src/tree/Links/Main.hs";

function view(page: Page): Locator {
  return page.locator("main .code-view");
}

function row(page: Page, n: number): Locator {
  return view(page).locator(`.code-row[data-line="${n}"]`);
}

function cell(page: Page, n: number): Locator {
  return row(page, n).locator(".code-cell");
}

function card(page: Page): Locator {
  return page.locator("main .code-card");
}

const line = (start: string) => lineOf(linksMainHs, start);

const linkAt = (page: Page, n: number, name: string) =>
  cell(page, n).getByRole("link", { name: new RegExp(`^${name}, `) });

async function open(page: Page): Promise<void> {
  await expect(view(page).locator("a.code-link").first()).toBeAttached();
}

async function boxOf(l: Locator) {
  const b = await l.boundingBox();
  if (b === null) throw new Error("no box");
  return b;
}

// The card is in the window, and right under or over the row when it
// fits there. Too tall for either side (200% zoom): it may cover the
// row; taller than the window: at its top.
async function besideRow(page: Page, n: number): Promise<void> {
  const c = await boxOf(card(page));
  const r = await boxOf(row(page, n));
  const w = page.viewportSize()!;
  expect(c.x).toBeGreaterThanOrEqual(0);
  expect(c.x + c.width).toBeLessThanOrEqual(w.width + 0.5);
  if (c.height > w.height) {
    expect(c.y).toBeCloseTo(0, 0);
    return;
  }
  expect(c.y).toBeGreaterThanOrEqual(-0.5);
  expect(c.y + c.height).toBeLessThanOrEqual(w.height + 0.5);
  const roomBelow = w.height - (r.y + r.height);
  if (c.height <= Math.max(r.y, roomBelow)) {
    const below = c.y >= r.y + r.height - 1;
    const above = c.y + c.height <= r.y + 1;
    expect(below || above, "card not over its row").toBe(true);
  }
}

// Mouse to the middle of l, in steps.
async function hover(page: Page, l: Locator): Promise<void> {
  const b = await boxOf(l);
  await page.mouse.move(b.x + b.width / 2, b.y + b.height / 2, { steps: 3 });
}

// The pointer on the page's title: no link there.
async function away(page: Page): Promise<void> {
  const b = await boxOf(page.locator("main h1"));
  await page.mouse.move(b.x + 2, b.y + b.height / 2, { steps: 3 });
}

// Keyboard to the link name on line n: a click at column 0 (no card),
// then → until it.
async function keyTo(page: Page, n: number, name: string): Promise<Locator> {
  await cell(page, n).click({ position: { x: 1, y: 5 } });
  const a = linkAt(page, n, name);
  for (let i = 0; i < 8; i++) {
    await page.keyboard.press("ArrowRight");
    if (await a.evaluate((el) => el === document.activeElement)) break;
  }
  await expect(a).toBeFocused();
  return a;
}

test("keyboard: → shows the card at once, it describes the link, Esc", async ({
  page,
  startServer,
  linksProject,
}) => {
  const server = await startServer({ root: linksProject.root });
  await page.goto(server.base + mainUrl);
  await open(page);
  const n = line("    go ys");
  await cell(page, n).click({ position: { x: 1, y: 5 } });
  // A click: no card.
  await expect(card(page)).toBeHidden();
  await page.keyboard.press("ArrowRight");
  await expect(linkAt(page, n, "sum")).toBeFocused();
  await expect(card(page)).toBeVisible();
  await expect(card(page)).toContainText("Search on Hoogle.");
  await page.keyboard.press("ArrowRight");
  await page.keyboard.press("ArrowRight");
  const area = linkAt(page, n, "area");
  await expect(area).toBeFocused();
  await expect(card(page).locator(".code-card-code")).toHaveText(
    "area :: Shape -> Double",
  );
  await expect(card(page)).toContainText("Module Links.Types.");
  await expect(area).toHaveAccessibleDescription(
    "area :: Shape -> Double Module Links.Types.",
  );
  await besideRow(page, n);
  // Esc: gone, focus and URL stay.
  await page.keyboard.press("Escape");
  await expect(card(page)).toBeHidden();
  await expect(area).toBeFocused();
  await expect(area).not.toHaveAttribute("aria-describedby", /./);
  await expect(page).toHaveURL(server.base + mainUrl);
  // The next link: a card again (ys, a local).
  await page.keyboard.press("ArrowRight");
  await expect(linkAt(page, n, "ys")).toBeFocused();
  await expect(card(page)).toHaveText(`Local, bound at line ${n}.`);
  // Out of the view and back by Tab.
  await page.keyboard.press("Shift+Tab");
  await expect(card(page)).toBeHidden();
  await page.keyboard.press("Tab");
  await expect(linkAt(page, n, "ys")).toBeFocused();
  await expect(card(page)).toBeVisible();
  await expect(linkAt(page, n, "ys")).toHaveAccessibleDescription(
    `Local, bound at line ${n}.`,
  );
  // The cell: none.
  for (let i = 0; i < 4; i++) await page.keyboard.press("ArrowLeft");
  await expect(cell(page, n)).toBeFocused();
  await expect(card(page)).toBeHidden();
  await expect(view(page).locator("[aria-describedby]")).toHaveCount(0);
});

test("pointer: after a delay, hoverable, a grace time, not over the row", async ({
  page,
  startServer,
  linksProject,
}) => {
  const server = await startServer({ root: linksProject.root });
  await page.goto(server.base + mainUrl);
  await open(page);
  const n = line("    go ys");
  const area = linkAt(page, n, "area");
  await hover(page, area);
  await expect(card(page)).toBeVisible();
  await expect(card(page)).toContainText("area :: Shape -> Double");
  await besideRow(page, n);
  // Down into the card, through the row's lower half.
  const a = await boxOf(area);
  const c = await boxOf(card(page));
  await page.mouse.move(a.x + 2, c.y + c.height / 2, { steps: 4 });
  await page.waitForTimeout(700);
  await expect(card(page)).toBeVisible();
  await expect(card(page)).toContainText("area :: Shape -> Double");
  // Out: gone after the grace time (unit tests time it).
  await away(page);
  await expect(card(page)).toBeHidden();
  // A mousedown on the card keeps focus where it was.
  await hover(page, area);
  await expect(card(page)).toBeVisible();
  await page.locator("main h1").focus();
  const c2 = await boxOf(card(page));
  await page.mouse.move(a.x + 2, c2.y + c2.height / 2, { steps: 4 });
  await page.mouse.down();
  await page.mouse.up();
  await expect(page.locator("main h1")).toBeFocused();
});

test("a guess says why, with no title; locals and externals", async ({
  page,
  startServer,
  linksProject,
}) => {
  const server = await startServer({ root: linksProject.root });
  await page.goto(server.base + mainUrl);
  await open(page);
  const f = linkAt(page, line("g r"), "f");
  await expect(f).not.toHaveAttribute("title", /./);
  await hover(page, f);
  await expect(card(page)).toContainText("Module Links.Main.");
  await expect(card(page)).toContainText("Resolved heuristically: ");
  await hover(page, linkAt(page, line("sorted ="), "sort"));
  await expect(card(page)).toContainText(/^From Data\.List( \(base\))?\./);
  await expect(card(page)).toContainText("Search on Hoogle.");
  await hover(page, linkAt(page, line("up ="), "toUpper"));
  await expect(card(page)).toHaveText(
    "From one of 2 open imports. A guess. Search on Hoogle.",
  );
  const n = line("total xs");
  await hover(page, linkAt(page, n, "xs"));
  await expect(card(page)).toHaveText(`Local, bound at line ${n}.`);
});

test("Esc hides a pointer card with focus outside the view", async ({
  page,
  startServer,
  linksProject,
}) => {
  const server = await startServer({ root: linksProject.root });
  await page.goto(server.base + mainUrl);
  await open(page);
  const h1 = page.locator("main h1");
  await h1.focus();
  await hover(page, linkAt(page, line("    go ys"), "area"));
  await expect(card(page)).toBeVisible();
  await page.keyboard.press("Escape");
  await expect(card(page)).toBeHidden();
  await expect(h1).toBeFocused();
  // Still on the link: it stays hidden.
  await page.waitForTimeout(800);
  await expect(card(page)).toBeHidden();
});

test("scrolling: the card follows its link, hides out of view, comes back", async ({
  page,
  startServer,
  linksProject,
}) => {
  const server = await startServer({ root: linksProject.root });
  await page.goto(server.base + mainUrl);
  await open(page);
  const n = line("    go ys");
  const area = await keyTo(page, n, "area");
  await expect(card(page)).toBeVisible();
  const gap = async () =>
    (await boxOf(card(page))).y - (await boxOf(row(page, n))).y;
  const before = await gap();
  // The wheel over the gutter: no link under the pointer.
  const v = await boxOf(view(page));
  await page.mouse.move(v.x + 4, v.y + v.height / 2);
  await page.mouse.wheel(0, 3 * 21);
  await expect
    .poll(() => view(page).evaluate((el) => el.scrollTop))
    .toBeGreaterThan(0);
  await expect.poll(gap).toBeCloseTo(before, 0);
  await expect(card(page)).toBeVisible();
  // Far: the row is out of view.
  await view(page).evaluate((el) => (el.scrollTop = 2000));
  await expect(card(page)).toBeHidden();
  await expect(area).toBeFocused();
  await view(page).evaluate((el) => (el.scrollTop = 0));
  await expect(card(page)).toBeVisible();
  await expect(() => besideRow(page, n)).toPass();
});

test("a drag from a link shows no card, copy stays exact", async ({
  page,
  startServer,
  linksProject,
}) => {
  const server = await startServer({ root: linksProject.root });
  await page.goto(server.base + mainUrl);
  await open(page);
  await recordCopies(page);
  const n = line("    go ys");
  const col = lines(linksMainHs)[n - 1]!.indexOf("area");
  const a = await boxOf(linkAt(page, n, "area"));
  const b = await boxOf(cell(page, n + 3));
  await page.mouse.move(a.x + 1, a.y + a.height / 2);
  await page.mouse.down();
  await page.mouse.move(b.x + b.width - 2, b.y + b.height / 2, { steps: 5 });
  await page.waitForTimeout(800);
  await expect(card(page)).toBeHidden();
  await page.mouse.up();
  const end = lines(linksMainHs)[n + 2]!.length;
  expect([
    textSlice(linksMainHs, [n, col], [n + 3, end]),
    textSlice(linksMainHs, [n, col + 1], [n + 3, end]),
  ]).toContain(await copy(page));
  await expect(card(page)).toBeHidden();
});

for (const t of themes) {
  test(`axe with a card open, ${t.scheme} theme`, async ({
    page,
    startServer,
    linksProject,
  }) => {
    const server = await startServer({ root: linksProject.root });
    await page.emulateMedia({ colorScheme: t.scheme });
    await page.goto(server.base + mainUrl);
    await open(page);
    await keyTo(page, line("g r"), "f");
    await expect(card(page)).toBeVisible();
    await expect(card(page)).toContainText("Resolved heuristically");
    await axe(page, `hover card, ${t.scheme}`);
  });
}

for (const size of [
  { width: 320, height: 640, font: "" },
  // 200% zoom, about: half the width, twice the root font.
  { width: 640, height: 360, font: "200%" },
]) {
  test(`${size.width} px${size.font === "" ? "" : `, font ${size.font}`}: the card fits`, async ({
    page,
    startServer,
    linksProject,
  }) => {
    const server = await startServer({ root: linksProject.root });
    await page.setViewportSize({ width: size.width, height: size.height });
    await page.goto(server.base + mainUrl);
    await open(page);
    if (size.font !== "") {
      // A sheet of its own: the CSP allows it, and no inline style.
      await page.evaluate((f) => {
        const sheet = new CSSStyleSheet();
        sheet.replaceSync(`:root { font-size: ${f}; }`);
        document.adoptedStyleSheets = [...document.adoptedStyleSheets, sheet];
      }, size.font);
      await expect
        .poll(() =>
          page.evaluate(
            () => getComputedStyle(document.documentElement).fontSize,
          ),
        )
        .toBe("32px");
    }
    const n = line("g r");
    await keyTo(page, n, "f");
    await expect(card(page)).toBeVisible();
    // The row in the middle of the window: room both ways.
    await row(page, n).evaluate((r) => r.scrollIntoView({ block: "center" }));
    // It follows a frame later.
    await expect(() => besideRow(page, n)).toPass();
    await noSideways(page);
  });
}

test("a real click on a link gives no card; a key after it does", async ({
  page,
  startServer,
  linksProject,
}) => {
  const server = await startServer({ root: linksProject.root });
  await page.goto(server.base + mainUrl);
  await open(page);
  // Stay here: the click's own path (mousedown, the late check) runs,
  // the navigation doesn't.
  await page.evaluate(() =>
    window.addEventListener(
      "click",
      (e) => {
        if (e.target instanceof Element && e.target.closest("a.code-link")) {
          e.preventDefault();
        }
      },
      true,
    ),
  );
  const n = line("    go ys");
  const area = linkAt(page, n, "area");
  await area.click();
  await expect(area).toBeFocused();
  await page.waitForTimeout(900);
  await expect(card(page)).toBeHidden();
  await expect(area).not.toHaveAttribute("aria-describedby", /./);
  await expect(page).toHaveURL(server.base + mainUrl);
  await page.keyboard.press("ArrowRight");
  await expect(linkAt(page, n, "ys")).toBeFocused();
  await expect(card(page)).toBeVisible();
});

// As the server sends it: plain objects, no records.
const cborOpts = { useRecords: false, mapsAsObjects: true } as const;

// The first api/source answer partial; the refetch (after an indexing
// event) the real one, so the rows redraw.
async function partialFirst(page: Page): Promise<{
  calls: () => number;
  generation: () => number;
}> {
  let calls = 0;
  let generation = 0;
  await page.route("**/api/source/**", async (route) => {
    const res = await route.fetch();
    const v = new Decoder(cborOpts).decode(await res.body()) as Record<
      string,
      unknown
    >;
    calls++;
    generation = v.generation as number;
    if (calls === 1) {
      v.partial = true;
      v.outline = [];
    }
    const body = Buffer.from(new Encoder(cborOpts).encode(v));
    await route.fulfill({ response: res, body });
  });
  return { calls: () => calls, generation: () => generation };
}

for (const esc of [false, true]) {
  test(`a redraw while partial keeps the card ${esc ? "closed" : "open"}`, async ({
    page,
    context,
    startServer,
    linksProject,
  }) => {
    await fakeEventSource(context);
    const server = await startServer({ root: linksProject.root });
    const src = await partialFirst(page);
    await page.goto(server.base + mainUrl);
    await page.evaluate(() => window.__hrEmit?.("hello", {}));
    await open(page);
    const n = line("    go ys");
    const area = await keyTo(page, n, "area");
    await expect(card(page)).toBeVisible();
    if (esc) {
      await page.keyboard.press("Escape");
      await expect(card(page)).toBeHidden();
    }
    const old = await area.elementHandle();
    const g = src.generation();
    await page.evaluate(
      (generation) =>
        window.__hrEmit?.("indexing", {
          generation,
          total: 2,
          ready: 2,
          failed: 0,
        }),
      g,
    );
    await expect.poll(src.calls).toBe(2);
    await expect(page.locator(".code-outline")).toBeVisible();
    expect(await old?.evaluate((el) => el.isConnected)).toBe(false);
    await expect(area).toBeFocused();
    if (esc) {
      await page.waitForTimeout(800);
      await expect(card(page)).toBeHidden();
      await expect(area).not.toHaveAttribute("aria-describedby", /./);
    } else {
      await expect(card(page)).toBeVisible();
      await expect(card(page)).toContainText("area :: Shape -> Double");
      await expect(area).toHaveAccessibleDescription(
        "area :: Shape -> Double Module Links.Types.",
      );
    }
  });
}
