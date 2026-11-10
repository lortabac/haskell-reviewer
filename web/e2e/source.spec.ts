// The virtual source view (D-70, SPEC §5.4, §5.7, §11.7), in the file
// view, on the fixture project's 20,000-line file.
import type { Locator, Page } from "@playwright/test";
import {
  expectNoAxeViolations as axe,
  noSideways,
  themes,
} from "./support/a11y";
import { expect, tabKey, test } from "./support/fixtures";
import { bigLine, bigText, crlfText, lines, mainHs } from "./support/project";

const big = "src/tree/data/lines-20k.txt";

function view(page: Page): Locator {
  return page.locator("main .code-view");
}

function row(page: Page, n: number): Locator {
  return view(page).locator(`.code-row[aria-rowindex="${n}"]`);
}

async function open(page: Page, url: string): Promise<void> {
  await page.goto(url);
  await expect(view(page)).toBeVisible();
}

function rowCount(page: Page): Promise<number> {
  return view(page).locator(".code-rows > .code-row").count();
}

async function scrollTo(page: Page, where: "middle" | "end"): Promise<void> {
  await view(page).evaluate((el, w) => {
    el.scrollTop = w === "end" ? el.scrollHeight : el.scrollHeight / 2;
  }, where);
}

// Offset in a text of column col (UTF-16) of line n (from 1).
function offsetIn(text: string, n: number, col: number): number {
  const ls = lines(text);
  let off = 0;
  for (let i = 0; i < n - 1; i++) off += (ls[i]?.length ?? 0) + 1;
  return off + col;
}

function bigSlice(a: [number, number], b: [number, number]): string {
  return bigText.slice(offsetIn(bigText, ...a), offsetIn(bigText, ...b));
}

// Records what each copy put on the clipboard, after the app's own
// handler (document) has run: window sees the event last.
async function recordCopies(page: Page): Promise<void> {
  await page.evaluate(() => {
    window.__copies = [];
    window.addEventListener("copy", (e) => {
      window.__copies.push(
        e.defaultPrevented
          ? (e.clipboardData?.getData("text/plain") ?? null)
          : null,
      );
    });
  });
}

async function copy(page: Page): Promise<string | null> {
  const before = await page.evaluate(() => window.__copies.length);
  await page.keyboard.press("ControlOrMeta+c");
  await expect
    .poll(() => page.evaluate(() => window.__copies.length))
    .toBe(before + 1);
  return page.evaluate(
    () => window.__copies[window.__copies.length - 1] ?? null,
  );
}

// Put the selection's anchor ("start") or focus ("extend") at column
// col of line n (plain text: one text node per cell). Done once the
// app has seen it, so the row is pinned.
async function selectAt(
  page: Page,
  how: "start" | "extend",
  n: number,
  col: number,
): Promise<void> {
  await row(page, n)
    .locator(".code-cell")
    .evaluate(
      (el, [h, c]) => {
        const t = el.firstChild;
        if (t === null) throw new Error("no text");
        return new Promise<void>((done) => {
          document.addEventListener("selectionchange", () => done(), {
            once: true,
          });
          const sel = getSelection();
          if (h === "start") sel?.setBaseAndExtent(t, c, t, c);
          else sel?.extend(t, c);
        });
      },
      [how, col] as const,
    );
}

// Scroll line n into the view (the view's own scroll), then wait for
// its row.
async function scrollToLine(page: Page, n: number): Promise<void> {
  await view(page).evaluate((el, line) => {
    const h = el.querySelector(".code-row")?.getBoundingClientRect().height;
    if (h === undefined) throw new Error("no row");
    el.scrollTop = (line - 5) * h;
  }, n);
  await expect(row(page, n)).toBeVisible();
}

test("20k lines: under 100 rows at the top, middle and end", async ({
  page,
  server,
}) => {
  await open(page, server.base + big);
  await expect(view(page)).toHaveAttribute("aria-rowcount", "20000");
  await expect(view(page)).toHaveAttribute("role", "grid");
  await expect(row(page, 1)).toBeVisible();
  expect(await rowCount(page)).toBeLessThan(100);
  await expect(row(page, 1).locator(".code-cell")).toHaveText(bigLine(1));

  await scrollTo(page, "middle");
  await expect(row(page, 10000)).toBeVisible();
  expect(await rowCount(page)).toBeLessThan(100);
  await expect(row(page, 10000).locator(".code-cell")).toHaveText(
    bigLine(10000),
  );

  await scrollTo(page, "end");
  await expect(row(page, 20000)).toBeVisible();
  expect(await rowCount(page)).toBeLessThan(100);
  await expect(row(page, 20000).locator(".code-cell")).toHaveText(
    bigLine(20000),
  );
});

test("#L: marked, on screen, announced, Tab lands on it", async ({
  page,
  server,
  browserName,
}) => {
  await open(page, server.base + big + "#L12345");
  const target = row(page, 12345);
  await expect(target).toHaveClass(/code-line-target/);
  await expect(target).toBeInViewport();
  await expect(page.locator('main [role="status"]')).toHaveText(
    "Line 12345 marked.",
  );
  expect(await rowCount(page)).toBeLessThan(100);
  await page.locator("main h1").focus();
  await page.keyboard.press(tabKey(browserName));
  await expect(target.locator(".code-cell")).toBeFocused();

  await open(page, server.base + big + "#L100-L120");
  for (const n of [100, 110, 120]) {
    await expect(row(page, n)).toHaveClass(/code-line-target/);
  }
  await expect(row(page, 99)).not.toHaveClass(/code-line-target/);
  await expect(row(page, 121)).not.toHaveClass(/code-line-target/);
  await expect(row(page, 100)).toBeInViewport();
  await expect(page.locator('main [role="status"]')).toHaveText(
    "Lines 100–120 marked.",
  );

  await open(page, server.base + big + "#L19999");
  await expect(row(page, 19999)).toBeInViewport();
  expect(await rowCount(page)).toBeLessThan(100);
});

test("#L change with focus on a line: focus goes to the mark", async ({
  page,
  server,
}) => {
  await open(page, server.base + big);
  await row(page, 2).locator(".code-cell").click();
  await page.evaluate(() => {
    location.hash = "#L15000";
  });
  const cell = row(page, 15000).locator(".code-cell");
  await expect(cell).toBeFocused();
  await expect(cell).toBeInViewport();
});

test("copy 5,000 lines: exact text, both ways, astral kept whole", async ({
  page,
  server,
}) => {
  await open(page, server.base + big);
  await recordCopies(page);
  // Line 98 starts "98\tl", then "ab"s.
  await scrollToLine(page, 98);
  await selectAt(page, "start", 98, 3);
  // The anchor row stays mounted while it scrolls away.
  await scrollToLine(page, 5100);
  await selectAt(page, "extend", 5100, 2);
  await expect(row(page, 98)).toBeAttached();
  expect(await copy(page)).toBe(bigSlice([98, 3], [5100, 2]));

  // Backwards, ending inside the astral character of line 5103 (7 | 5103).
  const mid = bigLine(5103).indexOf("\u{1d465}") + 1;
  await selectAt(page, "start", 5103, mid);
  await scrollToLine(page, 103);
  await selectAt(page, "extend", 103, 1);
  expect(await copy(page)).toBe(bigSlice([103, 1], [5103, mid + 1]));
});

test("copy with the mouse: click, scroll, Shift+click", async ({
  page,
  server,
}) => {
  await open(page, server.base + big);
  await recordCopies(page);
  const start = row(page, 10).locator(".code-cell");
  // Left edge of the code: column 0.
  await start.click({ position: { x: 1, y: 5 } });
  await scrollToLine(page, 5010);
  // Far right of the row: the line's end.
  const end = row(page, 5010).locator(".code-cell");
  const box = await end.boundingBox();
  if (box === null) throw new Error("no box");
  await page.keyboard.down("Shift");
  await page.mouse.click(box.x + box.width - 2, box.y + box.height / 2);
  await page.keyboard.up("Shift");
  const want = bigSlice([10, 0], [5010, bigLine(5010).length]);
  expect(await copy(page)).toBe(want);
  // The click moved the cursor there; an arrow keeps the selection.
  await expect(end).toBeFocused();
  await page.keyboard.press("ArrowDown");
  await expect(row(page, 5011).locator(".code-cell")).toBeFocused();
  expect(await copy(page)).toBe(want);
  // A Shift+click on the same screen extends it back.
  const back = await row(page, 5008).locator(".code-cell").boundingBox();
  if (back === null) throw new Error("no box");
  await page.keyboard.down("Shift");
  await page.mouse.click(back.x + back.width - 2, back.y + back.height / 2);
  await page.keyboard.up("Shift");
  expect(await copy(page)).toBe(
    bigSlice([10, 0], [5008, bigLine(5008).length]),
  );
});

test("drag across lines, then keys move from the drag's end", async ({
  page,
  server,
}) => {
  await open(page, server.base + big);
  await recordCopies(page);
  const a = await row(page, 3).locator(".code-cell").boundingBox();
  const b = await row(page, 6).locator(".code-cell").boundingBox();
  if (a === null || b === null) throw new Error("no box");
  await page.mouse.move(a.x + 1, a.y + a.height / 2);
  await page.mouse.down();
  await page.mouse.move(b.x + b.width - 2, b.y + b.height / 2, { steps: 5 });
  await page.mouse.up();
  const want = bigSlice([3, 0], [6, bigLine(6).length]);
  expect(await copy(page)).toBe(want);
  await expect(row(page, 6).locator(".code-cell")).toBeFocused();
  await page.keyboard.press("ArrowUp");
  await expect(row(page, 5).locator(".code-cell")).toBeFocused();
  expect(await copy(page)).toBe(want);
});

test("a right click moves the cursor too", async ({ page, server }) => {
  await open(page, server.base + big);
  await row(page, 6).locator(".code-cell").click({ button: "right" });
  await page.keyboard.press("Escape");
  await page.keyboard.press("ArrowDown");
  await expect(row(page, 7).locator(".code-cell")).toBeFocused();
});

test("a page selection across the whole view copies the whole file", async ({
  page,
  server,
}) => {
  await open(page, server.base + big);
  await recordCopies(page);
  // From the heading to past the view.
  await page.evaluate(() => {
    const h1 = document.querySelector("main h1");
    const main = document.querySelector("main");
    if (h1 === null || main === null) throw new Error("no page");
    getSelection()?.setBaseAndExtent(h1, 0, main, main.childNodes.length);
  });
  expect(await copy(page)).toBe(bigText);
});

test("Ctrl+A in the view copies the whole file", async ({ page, server }) => {
  await open(page, server.base + big);
  await recordCopies(page);
  await row(page, 3).locator(".code-cell").click();
  await page.keyboard.press("ControlOrMeta+a");
  expect(await copy(page)).toBe(bigText);
  // Still the whole file after keys that move and scroll.
  await page.keyboard.press("PageDown");
  await page.keyboard.press("ArrowDown");
  expect(await copy(page)).toBe(bigText);
  await scrollTo(page, "middle");
  await expect(row(page, 10000)).toBeVisible();
  expect(await copy(page)).toBe(bigText);
  // The wheel keeps it too. (Headless browsers show no scrollbar: a
  // press at the box's edge hits a line. Unit tests do the scrollbar.)
  const b = await view(page).boundingBox();
  if (b === null) throw new Error("no box");
  await page.mouse.move(b.x + b.width / 2, b.y + b.height / 2);
  await page.mouse.wheel(0, 300);
  await expect(row(page, 10025)).toBeInViewport();
  expect(await copy(page)).toBe(bigText);
  // A click on a line ends it.
  await row(page, 10025).locator(".code-cell").click();
  expect(await copy(page)).toBeNull();
});

test("the real clipboard gets the text", async ({
  page,
  server,
  context,
  browserName,
}) => {
  test.skip(browserName !== "chromium", "clipboard permissions: Chromium");
  await context.grantPermissions(["clipboard-read", "clipboard-write"]);
  await open(page, server.base + big);
  await selectAt(page, "start", 2, 0);
  await scrollToLine(page, 3000);
  await selectAt(page, "extend", 3000, 1);
  await page.keyboard.press("ControlOrMeta+c");
  await expect
    .poll(() => page.evaluate(() => navigator.clipboard.readText()))
    .toBe(bigSlice([2, 0], [3000, 1]));
});

test("line numbers and CRLF: never copied, CRLF kept", async ({
  page,
  server,
}) => {
  await open(page, server.base + "src/tree/data/crlf.txt");
  await recordCopies(page);
  const crlf = (a: [number, number], b: [number, number]) =>
    crlfText.slice(offsetIn(crlfText, ...a), offsetIn(crlfText, ...b));
  // From the start of line 1 to past the end of line 2, with the mouse
  // (over the gutter: where a drag into it ends is up to the browser).
  const a = await row(page, 1).locator(".code-cell").boundingBox();
  const b = await row(page, 2).locator(".code-cell").boundingBox();
  const v = await view(page).boundingBox();
  if (a === null || b === null || v === null) throw new Error("no box");
  await page.mouse.move(a.x + 1, a.y + a.height / 2);
  await page.mouse.down();
  // Line 3 is long: its cell goes far past the view's right edge.
  await page.mouse.move(v.x + v.width - 20, b.y + b.height / 2, { steps: 5 });
  await page.mouse.up();
  // CRLF lines: the line's end is before its "\r".
  expect(await copy(page)).toBe(crlf([1, 0], [2, "two\rhalf".length]));
  // Ends in the gutters, set by hand.
  await view(page).evaluate((el) => {
    const num = (n: number) =>
      el.querySelector(`.code-row[data-line="${n}"] .code-num`);
    const x = num(2);
    const y = num(4);
    if (x === null || y === null) throw new Error("no gutter");
    getSelection()?.setBaseAndExtent(x, 0, y, 0);
  });
  expect(await copy(page)).toBe(crlf([2, 0], [4, 0]));
  // A lone CR shows as a glyph.
  await expect(row(page, 2).locator(".code-ctrl")).toHaveText("\u240d");
});

test("keyboard: arrows, pages, Ctrl+End, Ctrl+Home", async ({
  page,
  server,
}) => {
  await open(page, server.base + big);
  const focusedLine = () =>
    page.evaluate(() =>
      document.activeElement
        ?.closest(".code-row")
        ?.getAttribute("aria-rowindex"),
    );
  await row(page, 1).locator(".code-cell").click();
  for (let i = 0; i < 3; i++) await page.keyboard.press("ArrowDown");
  expect(await focusedLine()).toBe("4");
  await page.keyboard.press("PageDown");
  const paged = Number(await focusedLine());
  expect(paged).toBeGreaterThan(10);
  await expect(row(page, paged)).toBeInViewport();
  await page.keyboard.press("Control+End");
  expect(await focusedLine()).toBe("20000");
  await expect(row(page, 20000)).toBeInViewport();
  await page.keyboard.press("ArrowUp");
  expect(await focusedLine()).toBe("19999");
  // Scrolled away, then an arrow: back to the cursor.
  await scrollTo(page, "middle");
  await page.keyboard.press("ArrowUp");
  expect(await focusedLine()).toBe("19998");
  await expect(row(page, 19998)).toBeInViewport();
  await page.keyboard.press("Control+Home");
  expect(await focusedLine()).toBe("1");
  await expect(row(page, 1)).toBeInViewport();
  // One Tab stop in the view: the cursor's line, the only one with a
  // tabindex.
  await expect(view(page).locator("[tabindex]")).toHaveCount(1);
  await expect(row(page, 1).locator(".code-cell")).toHaveAttribute(
    "tabindex",
    "0",
  );
});

test("row headers are named by their numbers", async ({ page, server }) => {
  await open(page, server.base + "src/tree/src/Main.hs");
  const rows = view(page).getByRole("row");
  await expect(rows).toHaveCount(lines(mainHs).length);
  await expect(
    rows.nth(2).getByRole("rowheader", { name: "3", exact: true }),
  ).toHaveCount(1);
  await expect(
    rows.nth(2).getByRole("gridcell", { name: "main :: IO ()" }),
  ).toHaveCount(1);
});

for (const t of themes) {
  test(`axe: big file with a mark, ${t.scheme} theme`, async ({
    page,
    server,
  }) => {
    await page.emulateMedia({ colorScheme: t.scheme });
    await open(page, server.base + big + "#L200-L203");
    await expect(row(page, 200)).toBeInViewport();
    // The cursor is on the mark's first line.
    const c = row(page, 200).locator(".code-cell");
    await c.focus();
    await expect(c).toBeFocused();
    await axe(page, `source view, ${t.scheme}`);
  });
}

test("320 px: only the view scrolls sideways, the gutter sticks", async ({
  page,
  server,
}) => {
  await page.setViewportSize({ width: 320, height: 640 });
  await open(page, server.base + "src/tree/data/crlf.txt");
  await noSideways(page);
  const v = view(page);
  const wide = await v.evaluate((el) => el.scrollWidth > el.clientWidth);
  expect(wide).toBe(true);
  await v.evaluate((el) => {
    el.scrollLeft = 200;
  });
  await expect.poll(() => v.evaluate((el) => el.scrollLeft)).toBeGreaterThan(0);
  const num = row(page, 3).locator(".code-num");
  const [nb, vb] = await Promise.all([num.boundingBox(), v.boundingBox()]);
  if (nb === null || vb === null) throw new Error("no box");
  // Still at the view's left edge (1px border).
  expect(Math.abs(nb.x - vb.x)).toBeLessThanOrEqual(2);
  await noSideways(page);
});
