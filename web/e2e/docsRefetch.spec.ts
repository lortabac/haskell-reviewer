// The docs page asks again while partial (T10c, D-293): only links
// change, open sources, instance lists, scroll and focus stay.
// Keyboard focus only (Playwright's Firefox sends no mouse events
// outside the window); in WebKit focus moving drops the selection:
// assert focus, not selection.
import type { Locator, Page } from "@playwright/test";
import { Decoder, Encoder } from "cbor-x";
import { fakeEventSource } from "./support/a11y";
import { expect, test } from "./support/fixtures";

const url = "docs/tree/Links.Main";

// As the server sends it: plain objects, no records.
const cborOpts = { useRecords: false, mapsAsObjects: true } as const;

type Json = Record<string, unknown>;
type Token = unknown[];

const decl = (page: Page, head: string): Locator =>
  page
    .locator("main section.decl")
    .filter({ has: page.locator(".decl-head .sig", { hasText: head }) })
    .first();

// The tokens of total's signature, in a decoded page.
function totalSig(v: Json): Token[] {
  const ds = (v.parse as Json).declarations as Json[];
  const d = ds.find((x) => x.name === "total");
  return ((d?.signature as Json).code ?? []) as Token[];
}

// Answers the first api/module call with edit(page), later ones as
// they are. Returns the call count and the page's generation.
async function editFirst(page: Page, edit: (v: Json) => void) {
  const state = { calls: 0, generation: 0 };
  await page.route("**/api/module/**", async (route) => {
    const res = await route.fetch();
    const v = new Decoder(cborOpts).decode(await res.body()) as Json;
    state.calls++;
    state.generation = v.generation as number;
    if (state.calls === 1) {
      v.partial = true;
      edit(v);
    }
    const body = Buffer.from(new Encoder(cborOpts).encode(v));
    await route.fulfill({ response: res, body });
  });
  return state;
}

const done = (page: Page, generation: number) =>
  page.evaluate(
    (g) =>
      window.__hrEmit?.("indexing", {
        generation: g,
        total: 2,
        ready: 2,
        failed: 0,
      }),
    generation,
  );

// Focus by keyboard: focus, then back and forth with Tab.
async function keyFocus(page: Page, l: Locator): Promise<void> {
  await l.focus();
  await page.keyboard.press("Shift+Tab");
  await page.keyboard.press("Tab");
  await expect(l).toBeFocused();
}

test("a partial page asks again at done: links come, state stays", async ({
  page,
  context,
  startServer,
  linksProject,
}) => {
  await fakeEventSource(context);
  const server = await startServer({ root: linksProject.root });
  // First answer: Shape in total's signature not linked.
  const state = await editFirst(page, (v) => {
    for (const t of totalSig(v)) {
      if (t[1] === "Shape") t.splice(2);
    }
  });
  await page.goto(server.base + url);
  await page.evaluate(() => window.__hrEmit?.("hello", {}));
  const total = decl(page, "total ::");
  const sig = total.locator(".decl-head .sig");
  await expect(sig.getByRole("link", { name: "Shape" })).toHaveCount(0);
  // Open: total's source, P's instances.
  await total.locator(".decl-source-toggle").click();
  const src = total.locator("pre.src");
  await expect(src).toBeVisible();
  const pre = await src.elementHandle();
  const p = decl(page, "data P");
  const list = p.locator("details.decl-instances").first();
  await list.locator("summary").click();
  await expect(list).toHaveAttribute("open", "");
  const items = await list.locator(".decl-instance-list").elementHandle();
  const dbl = sig.getByRole("link", { name: /^Double, / });
  await keyFocus(page, dbl);
  const old = await dbl.elementHandle();
  await page.evaluate(() => window.scrollBy(0, 60));
  const y = await page.evaluate(() => window.scrollY);
  expect(y).toBeGreaterThan(0);
  await done(page, state.generation);
  await expect.poll(() => state.calls).toBe(2);
  await expect(sig.getByRole("link", { name: "Shape" })).toHaveAttribute(
    "href",
    /docs\/tree\/Links\.Types#type%3AShape$/,
  );
  expect(await src.evaluate((el, x) => el === x, pre)).toBe(true);
  await expect(list).toHaveAttribute("open", "");
  expect(
    await list
      .locator(".decl-instance-list")
      .evaluate((el, x) => el === x, items),
  ).toBe(true);
  await expect(dbl).toBeFocused();
  expect(await dbl.evaluate((el, x) => el === x, old)).toBe(true);
  expect(await page.evaluate(() => window.scrollY)).toBe(y);
});

test("a focused link that changes: focus goes to the new link", async ({
  page,
  context,
  startServer,
  linksProject,
}) => {
  await fakeEventSource(context);
  const server = await startServer({ root: linksProject.root });
  // First answer: Shape links where Double does (Hoogle).
  const state = await editFirst(page, (v) => {
    const ts = totalSig(v);
    const d = ts.find((t) => t[1] === "Double");
    for (const t of ts) {
      if (t[1] === "Shape") t.splice(2, 2, null, d?.[3]);
    }
  });
  await page.goto(server.base + url);
  await page.evaluate(() => window.__hrEmit?.("hello", {}));
  const sig = decl(page, "total ::").locator(".decl-head .sig");
  const shape = sig.getByRole("link", { name: /^Shape/ });
  await expect(shape).toHaveAttribute("href", /hoogle/);
  await keyFocus(page, shape);
  const old = await shape.elementHandle();
  await done(page, state.generation);
  await expect.poll(() => state.calls).toBe(2);
  await expect(shape).toHaveAttribute(
    "href",
    /docs\/tree\/Links\.Types#type%3AShape$/,
  );
  expect(await old?.evaluate((el) => el.isConnected)).toBe(false);
  await expect(shape).toBeFocused();
});
