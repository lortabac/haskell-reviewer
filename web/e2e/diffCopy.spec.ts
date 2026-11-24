// Copy in the diff view (D-347): the text comes from the files, never
// the DOM. fixtures/repos/diffview.sh with --diff HEAD~1..HEAD:
// big.txt (20,000 lines, all changed; split pairs base i with head
// i), src/Pairs.hs (a change block), src/Calc.hs (gaps), crlf.txt,
// bom.txt. renames.sh for a rename's old path.
import { execFileSync } from "node:child_process";
import { join } from "node:path";
import type { Locator, Page } from "@playwright/test";
import {
  type At,
  copy,
  grantClipboard,
  nextWrite,
  recordCopies,
  selectIn,
  stubWriteText,
  textSlice,
} from "./support/copy";
import { expect, test } from "./support/fixtures";
import { makeRenamesProject } from "./support/project";
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

const grid = (page: Page) => page.getByRole("grid", { name: /^Diff of / });

// Open a diff and wait for its rows (never press keys before).
async function open(
  page: Page,
  server: Server,
  rest: string,
): Promise<Locator> {
  await page.setViewportSize({ width: 1400, height: 800 });
  await page.goto(server.base + "diff/" + rest);
  const g = grid(page);
  await expect(g.locator(".code-row").first()).toBeVisible();
  await recordCopies(page);
  return g;
}

// A file at a commit of the fixture repo.
function show(root: string, rev: string, path: string): string {
  return execFileSync("git", ["-C", root, "show", `${rev}:${path}`], {
    encoding: "utf8",
    env: { ...process.env, GIT_CONFIG_GLOBAL: "/dev/null" },
  });
}

// The code cell of display row d (a key) on side, split.
const splitCell = (g: Locator, d: number, side: "base" | "head") =>
  g.locator(`.code-row[data-key="${d}"] .code-cell[data-side="${side}"]`);

// The code cell whose row number says label, unified.
const unifiedCell = (g: Locator, label: string) =>
  g
    .locator(".code-row", {
      has: g.page().locator(`.code-num[aria-label="${label}"]`),
    })
    .locator(".code-cell");

// Scroll display row d into the grid, then wait for it.
async function scrollToRow(g: Locator, d: number): Promise<void> {
  await g.evaluate((el, row) => {
    const h = el.querySelector(".code-row")?.getBoundingClientRect().height;
    if (h === undefined) throw new Error("no row");
    el.scrollTop = Math.max(0, (row - 5) * h);
  }, d);
  await expect(g.locator(`.code-row[data-key="${d}"]`)).toBeVisible();
}

const copyLocation = (page: Page) =>
  page.getByRole("button", { name: "Copy location", exact: true });

const big = (side: string) =>
  Array.from(
    { length: 20000 },
    (_, i) => `${side} line ${String(i + 1).padStart(5, "0")}\n`,
  ).join("");

test("split: 5,000 lines off screen copy exactly, on the anchor's side", async ({
  page,
  startServer,
  diffviewProject,
  browserName,
}) => {
  await stubWriteText(page);
  await grantClipboard(page.context(), browserName);
  const server = await start(startServer, diffviewProject.root);
  const g = await open(page, server, "big.txt?layout=split");
  await expect(g.locator(".diff-half").first()).toBeVisible();
  // Base line 98 col 3 to base line 5100 col 2 (rows 97 and 5099).
  await scrollToRow(g, 97);
  await splitCell(g, 97, "base").click();
  await selectIn(splitCell(g, 97, "base"), "start", 3);
  await scrollToRow(g, 5099);
  await selectIn(splitCell(g, 5099, "base"), "extend", 2);
  // The anchor's row stays mounted.
  await expect(g.locator('.code-row[data-key="97"]')).toHaveCount(1);
  expect(await g.locator(".code-rows > .code-row").count()).toBeLessThan(200);
  expect(await copy(page)).toBe(textSlice(big("base"), [98, 3], [5100, 2]));
  expect(await nextWrite(page, () => copyLocation(page).click())).toBe(
    "big.txt:98-5100",
  );
  // Backwards, on head. A press there first, as a user does: WebKit
  // moves a selection off the unselectable side.
  await splitCell(g, 5099, "head").click();
  await selectIn(splitCell(g, 5099, "head"), "start", 2);
  await scrollToRow(g, 97);
  await selectIn(splitCell(g, 97, "head"), "extend", 3);
  expect(await copy(page)).toBe(textSlice(big("head"), [98, 3], [5100, 2]));
});

test("split: a drag from base into head copies base only", async ({
  page,
  startServer,
  diffviewProject,
}) => {
  const server = await start(startServer, diffviewProject.root);
  const g = await open(page, server, "big.txt?layout=split");
  const from = await splitCell(g, 10, "base").boundingBox();
  const to = await splitCell(g, 15, "head").boundingBox();
  if (from === null || to === null) throw new Error("no cells");
  await page.mouse.move(from.x + 40, from.y + from.height / 2);
  await page.mouse.down();
  await page.mouse.move(to.x + 40, to.y + to.height / 2, { steps: 10 });
  await page.mouse.up();
  await expect
    .poll(() => page.evaluate(() => getSelection()?.isCollapsed))
    .toBe(false);
  // Where the browser put the end over head's cells differs (seen: the
  // end of row 14 or 13, the start of row 14), but always in a base
  // cell: the copy is the exact base text between the painted ends.
  const ends = await page.evaluate(() => {
    const sel = getSelection()!;
    const at = (n: Node | null, o: number) => {
      const el = n instanceof Element ? n : n?.parentElement;
      const cell = el?.closest('.code-cell[data-side="base"]');
      const key = cell?.closest(".code-row")?.getAttribute("data-key");
      return key == null ? undefined : ([Number(key) + 1, o] as At);
    };
    return [
      at(sel.anchorNode, sel.anchorOffset),
      at(sel.focusNode, sel.focusOffset),
    ];
  });
  const [a, b] = ends;
  if (a == null || b == null) throw new Error("an end not in a base cell");
  expect(b[0]).toBeGreaterThan(a[0]);
  expect(await copy(page)).toBe(textSlice(big("base"), a, b));
  // Head's code can't be selected, so it isn't painted as selected.
  const us = await splitCell(g, 12, "head").evaluate((el) => {
    const s = getComputedStyle(el) as CSSStyleDeclaration & {
      webkitUserSelect?: string;
    };
    return s.userSelect || s.webkitUserSelect;
  });
  expect(us).toBe("none");
});

test("unified: across a change block, base lines then head lines", async ({
  page,
  startServer,
  diffviewProject,
}) => {
  const root = diffviewProject.root;
  const b = show(root, "HEAD~1", "src/Pairs.hs").split("\n");
  const h = show(root, "HEAD", "src/Pairs.hs").split("\n");
  const server = await start(startServer, root);
  const g = await open(page, server, "src/Pairs.hs");
  await selectIn(unifiedCell(g, "line 5, unchanged"), "start", 0);
  await selectIn(
    unifiedCell(g, "line 10, unchanged, base line 8"),
    "extend",
    0,
  );
  // Lines 5; base 6-7; head 6-9. No signs.
  const want = [h[4], b[5], b[6], h[5], h[6], h[7], h[8]].join("\n") + "\n";
  expect(await copy(page)).toBe(want);
});

test("unified: across a gap, the hidden lines too", async ({
  page,
  startServer,
  diffviewProject,
}) => {
  const root = diffviewProject.root;
  const h = show(root, "HEAD", "src/Calc.hs").split("\n");
  const server = await start(startServer, root);
  const g = await open(page, server, "src/Calc.hs");
  await expect(g.locator(".diff-gap").first()).toBeVisible();
  await selectIn(unifiedCell(g, "line 9, unchanged"), "start", 0);
  await selectIn(unifiedCell(g, "line 90, unchanged"), "extend", 0);
  expect(await copy(page)).toBe(h.slice(8, 89).join("\n") + "\n");
  // The gap stays closed.
  await expect(g.locator(".diff-gap").first()).toBeVisible();
});

test("Ctrl+A: CRLF kept per side, no BOM, all rows after a scroll", async ({
  page,
  startServer,
  diffviewProject,
}) => {
  const server = await start(startServer, diffviewProject.root);
  // crlf.txt and bom.txt have equal rows only: one gap.
  const whole = () =>
    page.getByRole("button", { name: "Show whole file" }).click();
  let g = await open(page, server, "crlf.txt");
  await g.locator(".code-cell").first().click();
  await page.keyboard.press("ControlOrMeta+a");
  expect(await copy(page)).toBe("one\r\ntwo\r\n");

  g = await open(page, server, "crlf.txt?layout=split");
  await whole();
  await splitCell(g, 0, "base").click();
  await page.keyboard.press("ControlOrMeta+a");
  expect(await copy(page)).toBe("one\ntwo\n");

  g = await open(page, server, "bom.txt?layout=split");
  await whole();
  await splitCell(g, 0, "head").click();
  await page.keyboard.press("ControlOrMeta+a");
  expect(await copy(page)).toBe("text\n");

  g = await open(page, server, "big.txt");
  await g.locator(".code-cell").first().click();
  await page.keyboard.press("ControlOrMeta+a");
  await g.evaluate((el) => (el.scrollTop = el.scrollHeight / 2));
  await expect(g.locator('.code-row[data-key="20000"]')).toBeVisible();
  expect(await copy(page)).toBe(big("base") + big("head"));
});

test("y and Copy location: path:line on each side", async ({
  page,
  startServer,
  diffviewProject,
  browserName,
}) => {
  await stubWriteText(page);
  await grantClipboard(page.context(), browserName);
  const server = await start(startServer, diffviewProject.root);
  const g = await open(page, server, "src/Pairs.hs?layout=split");
  // Base line 7 "  + n * 2", head line 8 "  + n": col 0 is a space.
  await splitCell(g, 6, "base").click({ position: { x: 2, y: 5 } });
  expect(await nextWrite(page, () => page.keyboard.press("y"))).toBe(
    "src/Pairs.hs:7",
  );
  await splitCell(g, 7, "head").click({ position: { x: 2, y: 5 } });
  expect(await nextWrite(page, () => page.keyboard.press("y"))).toBe(
    "src/Pairs.hs:8",
  );
  expect(await nextWrite(page, () => copyLocation(page).click())).toBe(
    "src/Pairs.hs:8",
  );
  // A name: the module's, from base's tree.
  await splitCell(g, 5, "base")
    .locator("a.code-link", { hasText: "double" })
    .focus();
  expect(await nextWrite(page, () => page.keyboard.press("y"))).toBe(
    "Util.double",
  );
});

test("Esc clears the selection", async ({
  page,
  startServer,
  diffviewProject,
}) => {
  const server = await start(startServer, diffviewProject.root);
  const g = await open(page, server, "src/Pairs.hs");
  await unifiedCell(g, "line 5, unchanged").click();
  await selectIn(unifiedCell(g, "line 5, unchanged"), "start", 0);
  await selectIn(unifiedCell(g, "removed line 7"), "extend", 2);
  await page.keyboard.press("Escape");
  await expect
    .poll(() => page.evaluate(() => getSelection()?.isCollapsed))
    .toBe(true);
  expect(await copy(page)).toBeNull();
});

test("the real clipboard gets the text", async ({
  page,
  startServer,
  diffviewProject,
  context,
  browserName,
}) => {
  test.skip(browserName !== "chromium", "clipboard permissions: Chromium");
  await context.grantPermissions(["clipboard-read", "clipboard-write"]);
  const server = await start(startServer, diffviewProject.root);
  const g = await open(page, server, "big.txt?layout=split");
  await splitCell(g, 1, "head").click();
  await selectIn(splitCell(g, 1, "head"), "start", 0);
  await scrollToRow(g, 2999);
  await selectIn(splitCell(g, 2999, "head"), "extend", 4);
  await page.keyboard.press("ControlOrMeta+c");
  await expect
    .poll(() => page.evaluate(() => navigator.clipboard.readText()))
    .toBe(textSlice(big("head"), [2, 0], [3000, 4]));
});

test.describe("renames.sh, ROOT pkg", () => {
  let project: { root: string; remove: () => Promise<void> };
  test.beforeAll(async () => {
    project = await makeRenamesProject();
  });
  test.afterAll(async () => {
    await project.remove();
  });

  test("Copy location on base: the old path", async ({
    page,
    startServer,
    browserName,
  }) => {
    await stubWriteText(page);
    await grantClipboard(page.context(), browserName);
    const server = await start(startServer, join(project.root, "pkg"));
    const g = await open(page, server, "sim60/new.txt");
    await unifiedCell(g, "removed line 7").click();
    expect(await nextWrite(page, () => copyLocation(page).click())).toBe(
      "sim60/old.txt:7",
    );
    await unifiedCell(g, "added line 7").click();
    expect(await nextWrite(page, () => page.keyboard.press("y"))).toBe(
      "sim60/new.txt:7",
    );
  });
});
