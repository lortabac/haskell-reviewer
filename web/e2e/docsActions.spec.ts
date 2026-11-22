// The docs page actions and colours (D-300): short labels from CSS, a
// tip on hover and focus that Esc hides, spacing, token colours, axe.
// WebKit: a key press before .focus(), or :focus-visible does not
// apply.
import type { Locator, Page } from "@playwright/test";
import {
  expectNoAxeViolations as axe,
  noSideways,
  themes,
} from "./support/a11y";
import { expect, test } from "./support/fixtures";

function decl(page: Page, head: string): Locator {
  return page
    .locator("section.decl")
    .filter({
      has: page.locator(":scope > .decl-head .sig", {
        hasText: new RegExp("^" + head.replace(/[.*+?^${}()|[\]\\]/g, "\\$&")),
      }),
    })
    .first();
}

const tipShown = (l: Locator): Promise<boolean> =>
  l.evaluate((e) => getComputedStyle(e, "::after").display === "block");

// The tip box (a ::after, no rect): from the control's rect and the
// tip's computed width and side. Inside the window, both edges.
const tipInside = (l: Locator): Promise<boolean> =>
  l.evaluate((e) => {
    const w = Number.parseFloat(getComputedStyle(e, "::after").width);
    const r = e.getBoundingClientRect();
    const right = e.getAttribute("data-tip-side") === "right";
    const left = right ? r.right - w : r.left;
    return left >= 0 && left + w <= window.innerWidth;
  });

const labels = (row: Locator): Promise<string[]> =>
  row.locator(":scope > *").evaluateAll((els) =>
    els.map((e) => {
      const c = getComputedStyle(e, "::before").content;
      return c.replace(/^"|"$/g, "").replace(/"\s*\/.*$/, "");
    }),
  );

const url = "docs/tree/Shapes";

test("actions: short labels, full names", async ({
  page,
  startServer,
  docsProject,
}) => {
  const server = await startServer({ root: docsProject.root });
  await page.goto(server.base + url);
  const area = decl(page, "area");
  const row = area.locator(":scope > .decl-actions");
  await expect(
    row.getByRole("button", { name: "Show source of area" }),
  ).toBeVisible();
  await expect(
    row.getByRole("link", { name: "Go to source of area" }),
  ).toBeVisible();
  await expect(
    row.getByRole("link", { name: "Find references (refs) of area" }),
  ).toBeVisible();
  await expect
    .poll(async () => (await labels(row)).slice(0, 3))
    .toEqual(["+", "source", "refs"]);
  const sub = page.locator(".subdecl-refs").first();
  await expect(sub).toHaveAccessibleName(/^Find references \(refs\) of /);
});

test("tip: hover and focus show it, Esc hides it", async ({
  page,
  startServer,
  docsProject,
}) => {
  const server = await startServer({ root: docsProject.root });
  await page.goto(server.base + url);
  const toggle = decl(page, "area").locator(".decl-source-toggle");
  await toggle.scrollIntoViewIfNeeded();
  expect(await tipShown(toggle)).toBe(false);
  await toggle.hover();
  await expect.poll(() => tipShown(toggle)).toBe(true);
  // Esc: hidden, and it stays hidden while the pointer is there.
  await page.keyboard.press("Escape");
  await expect.poll(() => tipShown(toggle)).toBe(false);
  await page.mouse.move(0, 0);
  await toggle.hover();
  await expect.poll(() => tipShown(toggle)).toBe(true);
  await page.mouse.move(0, 0);
  await expect.poll(() => tipShown(toggle)).toBe(false);
  // Keyboard focus.
  const refs = decl(page, "area").locator("a.decl-refs");
  await page.keyboard.press("Shift");
  await refs.focus();
  await expect(refs).toBeFocused();
  await expect.poll(() => tipShown(refs)).toBe(true);
  await page.keyboard.press("Escape");
  await expect.poll(() => tipShown(refs)).toBe(false);
  await expect(refs).toBeFocused();
});

test("a mouse click leaves no tip once the pointer moves away", async ({
  page,
  startServer,
  docsProject,
}) => {
  const server = await startServer({ root: docsProject.root });
  await page.goto(server.base + url);
  const toggle = decl(page, "area").locator(".decl-source-toggle");
  await toggle.click();
  await page.mouse.move(0, 0);
  await expect.poll(() => tipShown(toggle)).toBe(false);
});

test("spacing and colours", async ({ page, startServer, docsProject }) => {
  const server = await startServer({ root: docsProject.root });
  for (const t of themes) {
    await page.emulateMedia({ colorScheme: t.scheme });
    await page.goto(server.base + url);
    const area = decl(page, "area");
    await expect(area).toHaveCSS("padding-top", "16px");
    const kw = page.locator(".decl-head .tok-kw").first();
    const want =
      t.scheme === "light" ? "rgb(207, 34, 46)" : "rgb(255, 123, 114)";
    await expect(kw).toHaveCSS("color", want);
    await expect(area.locator(".decl-head")).toHaveCSS(
      "border-left-width",
      "3px",
    );
  }
});

for (const t of themes) {
  test(`axe, source open and a tip shown, ${t.scheme} theme`, async ({
    page,
    startServer,
    docsProject,
  }) => {
    await page.emulateMedia({ colorScheme: t.scheme });
    const server = await startServer({ root: docsProject.root });
    await page.goto(server.base + url);
    const area = decl(page, "area");
    // By keyboard, so :focus-visible applies in every browser.
    await area.locator(".decl-source-toggle").focus();
    await page.keyboard.press("Enter");
    await expect(area.locator("pre.src")).toBeVisible();
    const refs = area.locator("a.decl-refs");
    await refs.focus();
    await expect.poll(() => tipShown(refs)).toBe(true);
    await axe(page, `docs actions, ${t.scheme}`);
  });
}

test("320 px: every tip is inside the window, no sideways scroll", async ({
  page,
  startServer,
  docsProject,
}) => {
  await page.setViewportSize({ width: 320, height: 640 });
  const server = await startServer({ root: docsProject.root });
  await page.goto(server.base + url);
  await expect(page.locator(".subdecl-refs").first()).toBeVisible();
  await page.keyboard.press("Shift");
  const all = page.locator("[data-tip]");
  const n = await all.count();
  expect(n).toBeGreaterThan(3);
  for (let i = 0; i < n; i++) {
    const c = all.nth(i);
    await c.scrollIntoViewIfNeeded();
    await c.focus();
    await expect.poll(() => tipShown(c)).toBe(true);
    await expect.poll(() => tipInside(c), `tip ${i}`).toBe(true);
    await noSideways(page);
  }
});

const selected = (page: Page) =>
  page.evaluate(() => getSelection()?.toString());
const selectMain = (page: Page) =>
  page.evaluate(() => {
    const m = document.querySelector("main");
    if (m !== null) getSelection()?.selectAllChildren(m);
  });

test("Esc after a mouse click still clears the selection", async ({
  page,
  startServer,
  docsProject,
}) => {
  const server = await startServer({ root: docsProject.root });
  await page.goto(server.base + url);
  const toggle = decl(page, "area").locator(".decl-source-toggle");
  await toggle.click();
  await page.mouse.move(0, 0);
  await selectMain(page);
  expect(await selected(page)).not.toBe("");
  await page.keyboard.press("Escape");
  await expect.poll(() => selected(page)).toBe("");
});

test("click focus, a key, then Esc: a tip that came with the key goes", async ({
  page,
  startServer,
  docsProject,
}) => {
  const server = await startServer({ root: docsProject.root });
  await page.goto(server.base + url);
  const toggle = decl(page, "area").locator(".decl-source-toggle");
  await toggle.click();
  await page.mouse.move(0, 0);
  await page.keyboard.press("Shift");
  // Chromium and WebKit show it now; Firefox does not.
  if (await tipShown(toggle)) {
    await page.keyboard.press("Escape");
    await expect.poll(() => tipShown(toggle)).toBe(false);
  }
});

test("a second Esc, after a tip was hidden, clears the selection", async ({
  page,
  startServer,
  docsProject,
}) => {
  const server = await startServer({ root: docsProject.root });
  await page.goto(server.base + url);
  const toggle = decl(page, "area").locator(".decl-source-toggle");
  await toggle.hover();
  await expect.poll(() => tipShown(toggle)).toBe(true);
  await selectMain(page);
  await page.keyboard.press("Escape");
  await expect.poll(() => tipShown(toggle)).toBe(false);
  await page.keyboard.press("Escape");
  await expect.poll(() => selected(page)).toBe("");
});

test("focus on one control, pointer on another: Esc hides both tips", async ({
  page,
  startServer,
  docsProject,
}) => {
  const server = await startServer({ root: docsProject.root });
  await page.goto(server.base + url);
  const area = decl(page, "area");
  const toggle = area.locator(".decl-source-toggle");
  const refs = area.locator("a.decl-refs");
  await expect(refs).toBeVisible();
  await page.keyboard.press("Shift");
  await refs.focus();
  await toggle.hover();
  await expect.poll(() => tipShown(refs)).toBe(true);
  await expect.poll(() => tipShown(toggle)).toBe(true);
  await page.keyboard.press("Escape");
  await expect.poll(() => tipShown(refs)).toBe(false);
  await expect.poll(() => tipShown(toggle)).toBe(false);
});

test("the ? overlay closes on Esc with the pointer on a tip control", async ({
  page,
  startServer,
  docsProject,
}) => {
  const server = await startServer({ root: docsProject.root });
  await page.goto(server.base + url);
  await decl(page, "area").locator(".decl-source-toggle").hover();
  await page.keyboard.press("?");
  const dialog = page.getByRole("dialog");
  await expect(dialog).toBeVisible();
  await page.keyboard.press("Escape");
  await expect(dialog).toBeHidden();
});

test("copy button of a nested instance is not shown by the parent's hover", async ({
  page,
  startServer,
  docsProject,
}) => {
  const server = await startServer({ root: docsProject.root });
  await page.goto(server.base + url);
  const shape = decl(page, "data Shape");
  await shape.locator("summary", { hasText: /^Instances/ }).click();
  const inner = shape.locator("section.decl .decl-actions > .copy-button");
  expect(await inner.count()).toBeGreaterThan(0);
  await shape.locator(":scope > .decl-head").hover();
  await expect(
    shape.locator(":scope > .decl-actions > .copy-button"),
  ).toHaveCSS("opacity", "1");
  await expect(inner.first()).toHaveCSS("opacity", "0");
});

// Nested "refs" (T14f): inline after the signature, shown on the
// item's own head line only.
function sub(page: Page, name: string): Locator {
  return page
    .locator("li.subdecl")
    .filter({
      has: page.locator(":scope > .subdecl-head .sig", {
        hasText: new RegExp("^" + name),
      }),
    })
    .first();
}

const opacity = (l: Locator): Promise<string> =>
  l.evaluate((e) => getComputedStyle(e).opacity);

test("nested refs: inline, hidden, shown on its own head only", async ({
  page,
  startServer,
  docsProject,
}) => {
  const server = await startServer({ root: docsProject.root });
  await page.goto(server.base + url);
  const width = sub(page, "width");
  const height = sub(page, "height");
  const rect = sub(page, "Rect");
  const link = (l: Locator) => l.locator(":scope > .subdecl-head > a");
  await expect(link(width)).toHaveAccessibleName(
    "Find references (refs) of width",
  );
  // Same line as the signature (boxes overlap in y).
  const sig = await width.locator(":scope > .subdecl-head .sig").boundingBox();
  const box = await link(width).boundingBox();
  expect(sig).not.toBeNull();
  expect(box).not.toBeNull();
  expect(box!.y).toBeLessThan(sig!.y + sig!.height);
  expect(box!.y + box!.height).toBeGreaterThan(sig!.y);
  // Hidden by default, still in the page.
  await page.mouse.move(0, 0);
  expect(await opacity(link(width))).toBe("0");
  // Own head: shown.
  await width.locator(":scope > .subdecl-head").hover();
  await expect.poll(() => opacity(link(width))).toBe("1");
  // Its docs area: not shown.
  await width.locator(":scope > .subdecl-head ~ .doc").first().hover();
  expect(await opacity(link(width))).toBe("0");
  // A sibling: shown there, not on the first.
  await height.locator(":scope > .subdecl-head").hover();
  await expect.poll(() => opacity(link(height))).toBe("1");
  expect(await opacity(link(width))).toBe("0");
  // The parent's head: not the children's links.
  await rect.locator(":scope > .subdecl-head").hover();
  await expect.poll(() => opacity(link(rect))).toBe("1");
  expect(await opacity(link(width))).toBe("0");
  expect(await opacity(link(height))).toBe("0");
  // A child's head: not the parent's link.
  await width.locator(":scope > .subdecl-head").hover();
  await expect.poll(() => opacity(link(width))).toBe("1");
  expect(await opacity(link(rect))).toBe("0");
});

test("nested refs: shown on focus, tip not while hidden", async ({
  page,
  startServer,
  docsProject,
}) => {
  const server = await startServer({ root: docsProject.root });
  await page.goto(server.base + url);
  const refs = sub(page, "width").locator(":scope > .subdecl-head > a");
  await page.mouse.move(0, 0);
  expect(await opacity(refs)).toBe("0");
  expect(await tipShown(refs)).toBe(false);
  await page.keyboard.press("Shift");
  await refs.focus();
  await expect(refs).toBeFocused();
  await expect.poll(() => opacity(refs)).toBe("1");
  await expect.poll(() => tipShown(refs)).toBe(true);
  // The tip text is not cut by an inherited indent.
  const tip = (l: Locator) =>
    l.evaluate((e) => {
      const c = getComputedStyle(e, "::after");
      return { indent: c.textIndent, width: Number.parseFloat(c.width) };
    });
  const a = await tip(refs);
  const top = decl(page, "area").locator("a.decl-refs");
  await top.focus();
  await expect.poll(() => tipShown(top)).toBe(true);
  const b = await tip(top);
  expect(a.indent).toBe("0px");
  expect(Math.abs(a.width - b.width)).toBeLessThan(2);
});

for (const t of themes) {
  test(`axe, a nested refs link shown, ${t.scheme} theme`, async ({
    page,
    startServer,
    docsProject,
  }) => {
    await page.emulateMedia({ colorScheme: t.scheme });
    const server = await startServer({ root: docsProject.root });
    await page.goto(server.base + url);
    await sub(page, "width").locator(":scope > .subdecl-head").hover();
    await expect
      .poll(() =>
        opacity(sub(page, "width").locator(":scope > .subdecl-head > a")),
      )
      .toBe("1");
    await axe(page, `nested refs, ${t.scheme}`);
  });
}

test("nested refs: 320 px, a long signature wraps, link after its last part", async ({
  page,
  startServer,
  docsProject,
}) => {
  await page.setViewportSize({ width: 320, height: 640 });
  const server = await startServer({ root: docsProject.root });
  await page.goto(server.base + url);
  const head = sub(page, "wide").locator(":scope > .subdecl-head");
  await head.hover();
  await noSideways(page);
  const parts = head.locator(".sig-part");
  const last = await parts.last().boundingBox();
  const first = await parts.first().boundingBox();
  const box = await head.locator(".subdecl-refs").boundingBox();
  // The signature wraps (more than one line), refs is on its last line.
  expect(last!.y).toBeGreaterThan(first!.y);
  expect(box!.y + box!.height).toBeGreaterThan(last!.y);
  expect(box!.y).toBeLessThan(last!.y + last!.height);
  expect(box!.x).toBeGreaterThan(last!.x);
});
