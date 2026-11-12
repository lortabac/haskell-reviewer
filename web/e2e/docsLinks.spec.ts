// The docs page: highlighted inline source, links out of signatures
// and names (T10b, D-291, D-292). Pointer targets stay inside the
// window: Playwright's Firefox sends no mouse events outside it. In
// WebKit focus moving drops the selection: assert focus, not selection.
import type { Locator, Page } from "@playwright/test";
import { Decoder, Encoder } from "cbor-x";
import {
  expectNoAxeViolations as axe,
  fakeEventSource,
  themes,
} from "./support/a11y";
import { expect, test } from "./support/fixtures";

const url = "docs/tree/Links.Main";

// As the server sends it: plain objects, no records.
const cborOpts = { useRecords: false, mapsAsObjects: true } as const;

// The section of a declaration, by the start of its head.
const decl = (page: Page, head: string): Locator =>
  page
    .locator("main section.decl")
    .filter({ has: page.locator(`.decl-head .sig`) })
    .filter({
      has: page.locator(".decl-head .sig", { hasText: head }),
    })
    .first();

const showSource = async (d: Locator): Promise<Locator> => {
  await d.locator(".decl-source-toggle").click();
  return d.locator("pre.src");
};

const card = (d: Locator) => d.locator(".code-card");

async function boxOf(l: Locator) {
  const b = await l.boundingBox();
  if (b === null) throw new Error("no box");
  return b;
}

test("signature: a cross-module link, guesses dotted, external in a new tab", async ({
  page,
  startServer,
  linksProject,
}) => {
  const server = await startServer({ root: linksProject.root });
  await page.goto(server.base + url);
  const sig = decl(page, "total ::").locator(".decl-head .sig");
  const shape = sig.getByRole("link", { name: "Shape" });
  await expect(shape).toHaveAttribute(
    "href",
    /docs\/tree\/Links\.Types#type%3AShape$/,
  );
  await expect(shape).not.toHaveAttribute("target", /./);
  // Double: a name from the Prelude, a guess; Hoogle in a new tab.
  const dbl = sig.getByRole("link", { name: /^Double, / });
  await expect(dbl).toHaveAttribute("target", "_blank");
  await expect(dbl).toHaveAttribute("rel", "noopener noreferrer");
  await expect(dbl).toHaveAttribute("href", /^https:\/\/hoogle\.haskell\.org/);
  await expect(dbl).toHaveAccessibleName(/, opens in a new tab$/);
  await expect(dbl).toHaveClass(/link-guess/);
  expect(
    await dbl.evaluate((e) => getComputedStyle(e).textDecorationStyle),
  ).toBe("dotted");
  // A click goes to the other module's page, and Back comes back.
  await shape.click();
  await expect(page).toHaveURL(/docs\/tree\/Links\.Types#type%3AShape$/);
  await expect(page.locator("main h1")).toHaveText("Links.Types");
  await page.goBack();
  await expect(page.locator("main h1")).toHaveText("Links.Main");
});

test("inline source: highlighted, linked, one request for two blocks", async ({
  page,
  context,
  startServer,
  linksProject,
}) => {
  // No indexing events: no refetch of a partial answer.
  await fakeEventSource(context);
  const server = await startServer({ root: linksProject.root });
  let calls = 0;
  await page.route("**/api/source/**", async (route) => {
    calls++;
    await route.continue();
  });
  await page.goto(server.base + url);
  expect(calls).toBe(0);
  const total = decl(page, "total ::");
  const up = decl(page, "up ::");
  const src = await showSource(total);
  // The text is there at once, as plain text or highlighted.
  await expect(src).toContainText("total xs = go xs");
  await expect(src.locator(".hl-keyword").first()).toBeAttached();
  const area = src.getByRole("link", { name: /^area, / });
  await expect(area).toHaveAttribute(
    "href",
    /docs\/tree\/Links\.Types#val%3Aarea$/,
  );
  // Locals are no links.
  await expect(src.getByRole("link", { name: /^xs/ })).toHaveCount(0);
  await expect(src.getByRole("link", { name: /^ys/ })).toHaveCount(0);
  await showSource(up);
  await expect(up.locator("pre.src .hl-variable").first()).toBeAttached();
  expect(calls).toBe(1);
  // Click: the other module's docs, anchor focused; Back.
  await area.click();
  await expect(page).toHaveURL(/docs\/tree\/Links\.Types#val%3Aarea$/);
  await expect(page.locator("main h1")).toHaveText("Links.Types");
  await expect(
    page.locator("main section.decl:focus, main .decl:focus-within").first(),
  ).toBeVisible();
  await page.goBack();
  await expect(page.locator("main h1")).toHaveText("Links.Main");
});

test("inline source: Tab shows a card, Esc keeps focus; one card at a time", async ({
  page,
  startServer,
  linksProject,
}) => {
  const server = await startServer({ root: linksProject.root });
  await page.goto(server.base + url);
  const total = decl(page, "total ::");
  const up = decl(page, "up ::");
  const src = await showSource(total);
  await showSource(up);
  const area = src.getByRole("link", { name: /^area, / });
  await expect(area).toBeAttached();
  await area.focus();
  await page.keyboard.press("Shift+Tab");
  await page.keyboard.press("Tab");
  await expect(area).toBeFocused();
  await expect(card(total)).toBeVisible();
  await expect(card(total)).toContainText("Module Links.Types.");
  await expect(area).toHaveAccessibleDescription(/Module Links\.Types\./);
  // Esc: hidden, focus stays.
  await page.keyboard.press("Escape");
  await expect(card(total)).toBeHidden();
  await expect(area).toBeFocused();
  // Focus away and back by keys: its card again.
  await page.keyboard.press("Shift+Tab");
  await page.keyboard.press("Tab");
  await expect(card(total)).toBeVisible();
  // A pointer card in the other block while the first has a focus
  // card: only one shows. Pointer targets stay in the window.
  const map = up.locator("pre.src").getByRole("link", { name: /^map, / });
  await map.scrollIntoViewIfNeeded();
  const m = await boxOf(map);
  await page.mouse.move(m.x + m.width / 2, m.y + m.height / 2, { steps: 3 });
  await expect(card(up)).toBeVisible();
  await expect(card(total)).toBeHidden();
  await expect(page.locator("main .code-card:visible")).toHaveCount(1);
  // The pointer leaves: the focused link's card comes back.
  const h = await boxOf(page.locator("main h1"));
  await page.mouse.move(h.x + 2, h.y + h.height / 2, { steps: 3 });
  await expect(card(up)).toBeHidden();
  await expect(card(total)).toBeVisible();
  await expect(area).toBeFocused();
});

test("inline source: a pointer card after a delay, hoverable", async ({
  page,
  startServer,
  linksProject,
}) => {
  const server = await startServer({ root: linksProject.root });
  await page.goto(server.base + url);
  const total = decl(page, "total ::");
  const src = await showSource(total);
  const area = src.getByRole("link", { name: /^area, / });
  await area.scrollIntoViewIfNeeded();
  const b = await boxOf(area);
  await page.mouse.move(b.x + b.width / 2, b.y + b.height / 2, { steps: 3 });
  await expect(card(total)).toBeVisible();
  await expect(card(total)).toContainText("area :: Shape -> Double");
  // Into the card: it stays.
  const c = await boxOf(card(total));
  await page.mouse.move(b.x + 2, c.y + c.height / 2, { steps: 4 });
  await page.waitForTimeout(400);
  await expect(card(total)).toBeVisible();
  // Out, on the page title: gone.
  const h = await boxOf(page.locator("main h1"));
  await page.mouse.move(h.x + 2, h.y + h.height / 2, { steps: 3 });
  await expect(card(total)).toBeHidden();
});

test("a refetch while indexing keeps focus on the inline source link", async ({
  page,
  context,
  startServer,
  linksProject,
}) => {
  await fakeEventSource(context);
  const server = await startServer({ root: linksProject.root });
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
    // The first answer: partial, no outline; the refetch redraws.
    if (calls === 1) {
      v.partial = true;
      v.outline = [];
    }
    const body = Buffer.from(new Encoder(cborOpts).encode(v));
    await route.fulfill({ response: res, body });
  });
  await page.goto(server.base + url);
  await page.evaluate(() => window.__hrEmit?.("hello", {}));
  const total = decl(page, "total ::");
  const src = await showSource(total);
  const area = src.getByRole("link", { name: /^area, / });
  await expect(area).toBeAttached();
  await area.focus();
  await page.keyboard.press("Shift+Tab");
  await page.keyboard.press("Tab");
  await expect(area).toBeFocused();
  await expect(card(total)).toBeVisible();
  const old = await area.elementHandle();
  await page.evaluate(
    (g) =>
      window.__hrEmit?.("indexing", {
        generation: g,
        total: 2,
        ready: 2,
        failed: 0,
      }),
    generation,
  );
  await expect.poll(() => calls).toBe(2);
  await expect.poll(() => old?.evaluate((el) => el.isConnected)).toBe(false);
  await expect(area).toBeFocused();
  await expect(card(total)).toBeVisible();
});

for (const t of themes) {
  test(`axe, source open with a card, ${t.scheme} theme`, async ({
    page,
    startServer,
    linksProject,
  }) => {
    const server = await startServer({ root: linksProject.root });
    await page.emulateMedia({ colorScheme: t.scheme });
    await page.goto(server.base + url);
    const total = decl(page, "total ::");
    const src = await showSource(total);
    const area = src.getByRole("link", { name: /^area, / });
    await expect(area).toBeAttached();
    await area.focus();
    await page.keyboard.press("Shift+Tab");
    await page.keyboard.press("Tab");
    await expect(card(total)).toBeVisible();
    await axe(page, `docs source with a card, ${t.scheme}`);
    // A focused signature link.
    const shape = decl(page, "total ::")
      .locator(".decl-head .sig")
      .getByRole("link", { name: "Shape" });
    await shape.focus();
    await axe(page, `docs signature link focused, ${t.scheme}`);
  });
}
