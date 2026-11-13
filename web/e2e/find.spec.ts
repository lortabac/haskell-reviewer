// Find in file (T09, G11; D-288): whole file, literal, smart case. The
// hits are drawn with the CSS Custom Highlight API. Pointer targets
// stay inside the window: Playwright's Firefox sends no mouse events
// outside it.
import type { Locator, Page } from "@playwright/test";
import {
  expectNoAxeViolations as axe,
  noSideways,
  themes,
} from "./support/a11y";
import { expect, test } from "./support/fixtures";
import { lineOf, linksMainHs } from "./support/project";

const big = "src/tree/data/lines-20k.txt";
const crlf = "src/tree/data/crlf.txt";
const linksUrl = "src/tree/Links/Main.hs";

const view = (page: Page): Locator => page.locator("main .code-view");
const row = (page: Page, n: number): Locator =>
  view(page).locator(`.code-row[data-line="${n}"]`);
const cell = (page: Page, n: number): Locator =>
  row(page, n).locator(".code-cell");
const findButton = (page: Page): Locator =>
  page.getByRole("button", { name: "Find", exact: true });
const box = (page: Page): Locator =>
  page.getByRole("textbox", { name: "Find" });
const live = (page: Page): Locator =>
  page.locator("main .code-toolbar .code-find-status");

async function open(page: Page, url: string): Promise<void> {
  await page.goto(url);
  await expect(view(page)).toBeVisible();
}

// Open the bar and search. The input has focus.
async function find(page: Page, q: string): Promise<void> {
  if ((await findButton(page).getAttribute("aria-expanded")) !== "true") {
    await findButton(page).click();
  }
  await expect(box(page)).toBeFocused();
  await box(page).fill(q);
}

// The text of the highlight's ranges.
function ranges(page: Page, name: string): Promise<string[]> {
  return page.evaluate(
    (n) =>
      [...(CSS.highlights.get(n) ?? [])].map((r) => (r as Range).toString()),
    name,
  );
}

test("the hint and the button, closed", async ({ page, server }) => {
  await open(page, server.base + big);
  await expect(findButton(page)).toHaveAttribute("aria-expanded", "false");
  await expect(page.locator("main .code-hint")).toHaveText(
    "Browser find sees only visible lines — use Find (f) to search the whole file",
  );
  await expect(page.locator("main .code-find-bar")).toBeHidden();
});

test("an off-screen match: found, announced, scrolled to; Esc focuses it", async ({
  page,
  server,
}) => {
  await open(page, server.base + big);
  await expect(row(page, 19999)).toHaveCount(0);
  await find(page, "19999");
  await expect(live(page)).toHaveText("Match 1 of 1, line 19999");
  await expect(row(page, 19999)).toBeInViewport();
  await expect.poll(() => ranges(page, "code-find-current")).toEqual(["19999"]);
  // Focus stays in the input.
  await expect(box(page)).toBeFocused();
  await page.keyboard.press("Escape");
  await expect(findButton(page)).toHaveAttribute("aria-expanded", "false");
  await expect(cell(page, 19999)).toBeFocused();
  await expect.poll(() => ranges(page, "code-find-current")).toEqual([]);
});

test("counts: smart case, astral characters; Enter and Shift-Enter wrap", async ({
  page,
  server,
}) => {
  await open(page, server.base + big);
  await find(page, "\u{1d465}y");
  await expect(live(page)).toHaveText(/^Match 1 of 2857, line 7$/);
  await expect(page.locator(".code-find-count")).toHaveText("1 of 2857");
  await box(page).fill("l");
  await expect(live(page)).toHaveText("Match 1 of 20000, line 1");
  await page.keyboard.press("Shift+Enter");
  await expect(live(page)).toHaveText("Match 20000 of 20000, line 20000");
  await expect(row(page, 20000)).toBeInViewport();
  await page.keyboard.press("Enter");
  await expect(live(page)).toHaveText("Match 1 of 20000, line 1");
  await expect(row(page, 1)).toBeInViewport();
  await expect(box(page)).toBeFocused();
  // A capital: exact.
  await box(page).fill("L");
  await expect(live(page)).toHaveText("No matches");
  await expect(page.getByRole("button", { name: "Next" })).toBeDisabled();
});

test("the same text twice is announced again", async ({ page, server }) => {
  await open(page, server.base + big);
  await find(page, "19999");
  await expect(live(page)).toHaveText("Match 1 of 1, line 19999");
  const first = await live(page).evaluate((e) => e.textContent);
  await page.keyboard.press("Enter");
  // The text itself changes (a no-break space at the end).
  await expect
    .poll(() => live(page).evaluate((e) => e.textContent))
    .not.toBe(first);
  await expect(live(page)).toHaveText("Match 1 of 1, line 19999");
});

test("the current hit has an outline box on it, none on the others", async ({
  page,
  server,
}) => {
  await open(page, server.base + big);
  await find(page, "19999");
  await expect(live(page)).toHaveText("Match 1 of 1, line 19999");
  const boxes = view(page).locator(".code-find-box");
  await expect(boxes).toHaveCount(1);
  await expect(boxes).toHaveCSS("border-top-style", "solid");
  await expect
    .poll(async () => {
      const b = await boxes.boundingBox();
      const c = await cell(page, 19999).boundingBox();
      if (b === null || c === null) return false;
      return b.y >= c.y - 1 && b.y + b.height <= c.y + c.height + 1;
    })
    .toBe(true);
  // Several hits: still one box.
  await box(page).fill("l");
  await expect(live(page)).toHaveText("Match 1 of 20000, line 1");
  await expect(boxes).toHaveCount(1);
  await page.keyboard.press("Escape");
  await expect(boxes).toHaveCount(0);
});

for (const height of [600, 800]) {
  test(`1280x${height}: opening the bar doesn't make the page taller`, async ({
    page,
    server,
  }) => {
    // Fonts differ between machines, so the page may already scroll a
    // bit: compare closed and open, not the page with the window.
    await page.setViewportSize({ width: 1280, height });
    await open(page, server.base + big);
    const toolbar = page.locator("main .code-toolbar");
    const pageHeight = () =>
      page.evaluate(() => document.documentElement.scrollHeight);
    const closed = await pageHeight();
    const before = (await toolbar.boundingBox())?.height ?? 0;
    await find(page, "l");
    await expect(live(page)).toHaveText("Match 1 of 20000, line 1");
    expect(await pageHeight()).toBeLessThanOrEqual(closed);
    // The bar fits beside the button: no new row.
    const after = (await toolbar.boundingBox())?.height ?? 999;
    expect(after).toBeLessThanOrEqual(before + 1);
  });
}

test("CRLF and a lone CR; a hit far right is brought in view", async ({
  page,
  server,
}) => {
  await open(page, server.base + crlf);
  await find(page, "half");
  await expect(live(page)).toHaveText("Match 1 of 1, line 2");
  await expect.poll(() => ranges(page, "code-find-current")).toEqual(["half"]);
  // 400 x on line 3: forty hits of ten.
  await box(page).fill("x".repeat(10));
  await expect(live(page)).toHaveText("Match 1 of 40, line 3");
  await page.keyboard.press("Shift+Enter");
  await expect(live(page)).toHaveText("Match 40 of 40, line 3");
  await expect
    .poll(() =>
      page.evaluate(() => {
        const r = [...(CSS.highlights.get("code-find-current") ?? [])][0] as
          Range | undefined;
        const v = document.querySelector("main .code-view");
        if (r === undefined || v === null) return false;
        const a = r.getBoundingClientRect();
        const b = v.getBoundingClientRect();
        return a.right <= b.right && a.left >= b.left;
      }),
    )
    .toBe(true);
  await expect
    .poll(() => view(page).evaluate((e) => e.scrollLeft))
    .toBeGreaterThan(0);
});

test("Esc: a hover card first, then the bar", async ({
  page,
  startServer,
  linksProject,
}) => {
  const server = await startServer({ root: linksProject.root });
  await open(page, server.base + linksUrl);
  const n = lineOf(linksMainHs, "    go ys");
  await find(page, "area");
  await expect(live(page)).toContainText("Match 1 of");
  const link = cell(page, n).getByRole("link", { name: /^area, / });
  const b = await link.boundingBox();
  if (b === null) throw new Error("no box");
  await page.mouse.move(b.x + b.width / 2, b.y + b.height / 2, { steps: 3 });
  const card = page.locator("main .code-card");
  await expect(card).toBeVisible();
  await page.keyboard.press("Escape");
  await expect(card).toBeHidden();
  await expect(box(page)).toBeFocused();
  await expect(findButton(page)).toHaveAttribute("aria-expanded", "true");
  await page.keyboard.press("Escape");
  await expect(findButton(page)).toHaveAttribute("aria-expanded", "false");
});

test("Close focuses the hit; the button toggles it closed", async ({
  page,
  server,
}) => {
  await open(page, server.base + big);
  await find(page, "19999");
  await expect(live(page)).toHaveText("Match 1 of 1, line 19999");
  await page.getByRole("button", { name: "Close" }).click();
  await expect(cell(page, 19999)).toBeFocused();
  // No hits: focus goes to the button.
  await find(page, "zzzz");
  await expect(live(page)).toHaveText("No matches");
  await page.keyboard.press("Escape");
  await expect(findButton(page)).toBeFocused();
  // The button closes an open bar; with no hit, focus stays on it.
  await findButton(page).click();
  await expect(findButton(page)).toHaveAttribute("aria-expanded", "true");
  await findButton(page).click();
  await expect(findButton(page)).toHaveAttribute("aria-expanded", "false");
  await expect(findButton(page)).toBeFocused();
});

// One box, on the hit, also across tokens and links (highlighted file).
for (const q of ["area", "go ys"]) {
  test(`one outline box covers the hit "${q}"`, async ({
    page,
    startServer,
    linksProject,
  }) => {
    const server = await startServer({ root: linksProject.root });
    await open(page, server.base + linksUrl);
    await expect(view(page).locator("a.code-link").first()).toBeAttached();
    const n = lineOf(linksMainHs, "    go ys");
    await find(page, q);
    await expect(live(page)).toContainText("Match 1 of");
    // The first hit after the cursor (line 1) may be elsewhere: go to
    // the one on the "go ys" line.
    for (let i = 0; i < 20; i++) {
      if ((await live(page).textContent())?.includes(`line ${n}`)) break;
      await page.keyboard.press("Enter");
    }
    await expect(live(page)).toContainText(`line ${n}`);
    const boxes = view(page).locator(".code-find-box");
    await expect(boxes).toHaveCount(1);
    const hit = await page.evaluate(() => {
      const r = [...(CSS.highlights.get("code-find-current") ?? [])][0] as
        Range | undefined;
      const rects = [...(r?.getClientRects() ?? [])].filter((x) => x.width > 0);
      return {
        left: Math.min(...rects.map((x) => x.left)),
        right: Math.max(...rects.map((x) => x.right)),
        text: r?.toString(),
      };
    });
    expect(hit.text).toBe(q);
    await expect
      .poll(async () => {
        const b = await boxes.boundingBox();
        if (b === null) return false;
        return (
          Math.abs(b.x - hit.left) <= 1.5 &&
          Math.abs(b.x + b.width - hit.right) <= 1.5
        );
      })
      .toBe(true);
  });
}

for (const t of themes) {
  test(`axe with the bar open and hits, ${t.scheme} theme`, async ({
    page,
    server,
  }) => {
    await page.emulateMedia({ colorScheme: t.scheme });
    await open(page, server.base + big);
    await find(page, "19999");
    await expect(live(page)).toHaveText("Match 1 of 1, line 19999");
    await expect(row(page, 19999)).toBeInViewport();
    await axe(page, `find bar, ${t.scheme}`);
  });
}

test("320 px: the bar wraps, no sideways scroll", async ({ page, server }) => {
  await page.setViewportSize({ width: 320, height: 700 });
  await open(page, server.base + big);
  await find(page, "l");
  await expect(live(page)).toHaveText("Match 1 of 20000, line 1");
  await noSideways(page);
});
