// The diff view (D-341, D-342). fixtures/repos/diffview.sh with --diff
// HEAD~1..HEAD. src/Calc.hs: rows 0-9 hold the changes on lines 3, 5
// and 7, then 83 equal lines (a gap of 77: lines 11 to 87), then line
// 91 changed. big.txt: 20,000 lines removed, 20,000 added.
import type { Locator, Page } from "@playwright/test";
import { expectNoAxeViolations, noSideways, themes } from "./support/a11y";
import { expect, test } from "./support/fixtures";
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

// Open a diff and wait for its rows (the app renders after
// api/session: never press keys before).
async function open(
  page: Page,
  server: Server,
  rest: string,
): Promise<Locator> {
  await page.goto(server.base + "diff/" + rest);
  const g = grid(page);
  await expect(g.locator(".code-row").first()).toBeVisible();
  return g;
}

const gapTexts = (g: Locator) => g.locator(".diff-gap-text");

// Focus is in the row: its cell, or a link there.
const focusIn = (r: Locator) => expect(r.locator(":focus")).toHaveCount(1);

// The row whose number says label.
const row = (g: Locator, label: string) =>
  g.locator(".code-row", {
    has: g.page().locator(`[role="rowheader"][aria-label="${label}"]`),
  });

test("rows, gaps and expanding them", async ({
  page,
  startServer,
  diffviewProject,
}) => {
  const server = await start(startServer, diffviewProject.root);
  const g = await open(page, server, "src/Calc.hs");
  await expect(g).toHaveAttribute("aria-rowcount", "19");
  await expect(gapTexts(g)).toHaveText(["77 lines hidden, 11 to 87"]);
  await expect(row(g, "removed line 7").locator(".code-cell")).toHaveText(
    "total xs = sum xs",
  );
  await expect(row(g, "added line 7").locator(".code-cell")).toHaveText(
    "total xs = sum (map double xs)",
  );
  // The gap: ↓ 20 shows its top, ↑ 20 its bottom. Focus stays on the
  // button that was used.
  await g.getByRole("button", { name: "20 lines, 11 to 30" }).click();
  await expect(gapTexts(g)).toHaveText(["57 lines hidden, 31 to 87"]);
  // The gap moved down: it is scrolled into view (WCAG 2.4.11).
  const down = g.getByRole("button", { name: "20 lines, 31 to 50" });
  await expect(down).toBeFocused();
  await expect(down).toBeInViewport();
  await g.getByRole("button", { name: "20 lines, 68 to 87" }).click();
  await expect(gapTexts(g)).toHaveText(["37 lines hidden, 31 to 67"]);
  await expect(
    g.getByRole("button", { name: "20 lines, 48 to 67" }),
  ).toBeFocused();
  await g.getByRole("button", { name: /^All 37 lines/ }).click();
  await expect(gapTexts(g)).toHaveCount(0);
  await expect(g).toHaveAttribute("aria-rowcount", "95");
  // Focus on the first line shown.
  await focusIn(row(g, "line 31, unchanged"));

  // Whole file, on a fresh page: every row, then back.
  await open(page, server, "src/Calc.hs");
  const whole = page.getByRole("button", { name: "Show whole file" });
  await whole.click();
  await expect(whole).toHaveAttribute("aria-pressed", "true");
  await expect(g).toHaveAttribute("aria-rowcount", "95");
  await whole.click();
  await expect(g).toHaveAttribute("aria-rowcount", "19");
});

test("links: removed lines go to base, same-file ones to the diff hash", async ({
  page,
  startServer,
  diffviewProject,
}) => {
  const server = await start(startServer, diffviewProject.root);
  const g = await open(page, server, "src/Calc.hs");
  const removed = row(g, "removed line 91");
  const double = removed.getByRole("link", {
    name: /^double, defined in src\/Util.hs/,
  });
  await expect(double).toHaveAttribute(
    "href",
    /\/src\/base\/src\/Util\.hs#L3$/,
  );
  const triple = row(g, "added line 91").getByRole("link", {
    name: /^triple, /,
  });
  await expect(triple).toHaveAttribute(
    "href",
    /\/src\/head\/src\/Util\.hs#L6$/,
  );
  // A local on a removed line: base's line, here.
  const xs = row(g, "removed line 7").getByRole("link", {
    name: /^xs, bound at line 7/,
  });
  await expect(xs).toHaveAttribute("href", /\/diff\/src\/Calc\.hs#base-L7$/);
  await xs.click();
  await expect(page).toHaveURL(/\/diff\/src\/Calc\.hs#base-L7$/);
  await expect(row(g, "removed line 7")).toHaveClass(/code-line-target/);
  await double.click();
  await expect(page).toHaveURL(/\/src\/base\/src\/Util\.hs#L3$/);
  await expect(page.locator("main h1")).toHaveText("src/Util.hs");
});

test("a hash on a hidden line opens its gap", async ({
  page,
  startServer,
  diffviewProject,
}) => {
  const server = await start(startServer, diffviewProject.root);
  const g = await open(page, server, "src/Calc.hs#head-L50");
  const marked = row(g, "line 50, unchanged");
  await expect(marked).toHaveClass(/code-line-target/);
  await expect(marked).toBeInViewport();
  await expect(gapTexts(g)).toHaveText([
    "36 lines hidden, 11 to 46",
    "34 lines hidden, 54 to 87",
  ]);
  // An equal row answers to the base hash too.
  await open(page, server, "src/Calc.hs#base-L9");
  await expect(row(g, "line 9, unchanged")).toHaveClass(/code-line-target/);
});

test("keys move the cursor; focus stays on the rows", async ({
  page,
  startServer,
  diffviewProject,
}) => {
  const server = await start(startServer, diffviewProject.root);
  const g = await open(page, server, "src/Calc.hs");
  await row(g, "line 1, unchanged").locator(".code-cell").click();
  await expect(row(g, "line 1, unchanged").locator(".code-cell")).toBeFocused();
  await page.keyboard.press("ArrowDown");
  await page.keyboard.press("ArrowDown");
  await expect(row(g, "removed line 3").locator(".code-cell")).toBeFocused();
  await page.keyboard.press("End");
  await focusIn(row(g, "added line 91"));
  await page.keyboard.press("Home");
  await page.keyboard.press("PageDown");
  // On the gap: → goes to its buttons.
  const gap = g.locator(".diff-gap-cell");
  for (let i = 0; i < 20; i++) {
    if (await gap.evaluate((el) => el === document.activeElement)) break;
    await page.keyboard.press("ArrowUp");
  }
  await expect(gap).toBeFocused();
  await page.keyboard.press("ArrowRight");
  await expect(
    g.getByRole("button", { name: "20 lines, 11 to 30" }),
  ).toBeFocused();
  await page.keyboard.press("Enter");
  await expect(gapTexts(g)).toHaveText(["57 lines hidden, 31 to 87"]);
});

test("a 20,000-line diff keeps few rows in the DOM", async ({
  page,
  startServer,
  diffviewProject,
}) => {
  const server = await start(startServer, diffviewProject.root);
  const g = await open(page, server, "big.txt");
  await expect(g).toHaveAttribute("aria-rowcount", "40000");
  const rows = g.locator(".code-row");
  expect(await rows.count()).toBeLessThan(100);
  // The middle.
  await g.evaluate((el) => {
    el.scrollTop = el.scrollHeight / 2;
  });
  await expect(row(g, "added line 1")).toBeAttached();
  expect(await rows.count()).toBeLessThan(100);
  // The end, by key.
  await row(g, "added line 1").locator(".code-cell").click();
  await page.keyboard.press("End");
  const last = row(g, "added line 20000");
  await expect(last.locator(".code-cell")).toBeFocused();
  await expect(last).toBeInViewport();
  expect(await rows.count()).toBeLessThan(100);
});

for (const t of themes) {
  test(`axe, ${t.scheme} theme`, async ({
    page,
    startServer,
    diffviewProject,
  }) => {
    const server = await start(startServer, diffviewProject.root);
    await page.emulateMedia({ colorScheme: t.scheme });
    const g = await open(page, server, "src/Calc.hs#base-L91");
    // Highlighting and links on both sides.
    await expect(
      row(g, "removed line 91").getByRole("link").first(),
    ).toBeVisible();
    await expect(
      row(g, "added line 91").getByRole("link").first(),
    ).toBeVisible();
    await row(g, "removed line 7").locator(".code-cell").click();
    await expectNoAxeViolations(page, `diff view (${t.scheme})`);
  });
}

test("320 px: no sideways scroll, gap buttons fit", async ({
  page,
  startServer,
  diffviewProject,
}) => {
  const server = await start(startServer, diffviewProject.root);
  await page.setViewportSize({ width: 320, height: 640 });
  const g = await open(page, server, "src/Calc.hs");
  await noSideways(page);
  const gap = g.locator(".diff-gap").first();
  const box = await gap.boundingBox();
  const cell = await row(g, "line 1, unchanged").boundingBox();
  expect(box?.height).toBeCloseTo(cell?.height ?? 0, 0);
  const view = await g.boundingBox();
  for (const b of await gap.getByRole("button").all()) {
    const r = await b.boundingBox();
    expect(r).not.toBeNull();
    expect(r!.x + r!.width).toBeLessThanOrEqual(view!.x + view!.width);
    expect(r!.height).toBeLessThanOrEqual(box!.height);
  }
});

// Files whose sides have text (or a symlink's target): the texts must
// fit the runs (the guard fails on the warning otherwise).
for (const [path, rows] of [
  ["crlf.txt", "1"],
  ["bom.txt", "1"],
  ["link", "2"],
  ["kind.txt", "2"],
  ["added.txt", "1"],
  ["removed.txt", "1"],
  ["ws.txt", "2"],
  ["src/Util.hs", "1"],
] as const) {
  test(`texts fit the runs: ${path}`, async ({
    page,
    startServer,
    diffviewProject,
  }) => {
    const server = await start(startServer, diffviewProject.root);
    const g = await open(page, server, path);
    await expect(g).toHaveAttribute("aria-rowcount", rows);
    await expect(page.locator("main")).not.toContainText(
      "Couldn't show this diff.",
    );
  });
}

test("browse mode: not in diff mode", async ({ page, server }) => {
  await page.goto(server.base + "diff/src/Main.hs");
  await expect(page.locator("main")).toContainText("Not in diff mode.");
});

// The notes (D-342). Pages with no lines have no grid: wait for the
// note itself.
for (const [path, note] of [
  ["data.bin", "Binary file changed (3 bytes → 4 bytes)."],
  [
    "huge.txt",
    "Too large to diff (head: 1,200,000 bytes; the limit is 1 MiB).",
  ],
  ["sub", "Subproject commit 1111111 → 2222222."],
  ["link", "Symbolic link: the lines are its target."],
  ["kind.txt", "Changed from a file to a symbolic link."],
  ["crlf.txt", "Line endings changed on 2 lines; not shown as changes."],
  ["bom.txt", "Byte order mark added."],
  ["src/Util.hs", "No changes in this file."],
  ["ws.txt?ws=ignore", "No changes when whitespace is ignored."],
] as const) {
  test(`note: ${path}`, async ({ page, startServer, diffviewProject }) => {
    const server = await start(startServer, diffviewProject.root);
    await page.goto(server.base + "diff/" + path);
    await expect(page.locator("main .file-note")).toHaveText([note]);
  });
}

test("edits.sh --diff HEAD: the filtered note", async ({
  page,
  startServer,
  editsProject,
}) => {
  const server = await startServer({ root: editsProject.root, diff: "HEAD" });
  await server.waitForLog(/indexing: done/, 20_000);
  await open(page, server, "data.bin");
  await expect(page.locator("main .file-note")).toHaveText([
    "Shown as on disk: a git filter applies; the stored content may differ.",
  ]);
});

test("the ws toggle: a new history entry, the hash kept", async ({
  page,
  startServer,
  diffviewProject,
}) => {
  const server = await start(startServer, diffviewProject.root);
  const g = await open(page, server, "ws.txt#head-L1");
  await expect(g).toHaveAttribute("aria-rowcount", "2");
  const ws = page.getByRole("button", { name: "Ignore whitespace" });
  await expect(ws).toHaveAttribute("aria-pressed", "false");
  await ws.click();
  await expect(page).toHaveURL(/\/diff\/ws\.txt\?ws=ignore#head-L1$/);
  await expect(ws).toHaveAttribute("aria-pressed", "true");
  await expect(g).toHaveAttribute("aria-rowcount", "1");
  await expect(page.locator("main .file-note")).toHaveText([
    "No changes when whitespace is ignored.",
  ]);
  await expect(ws).toBeFocused();
  await page.goBack();
  await expect(page).toHaveURL(/\/diff\/ws\.txt#head-L1$/);
  await expect(g).toHaveAttribute("aria-rowcount", "2");
  await expect(ws).toHaveAttribute("aria-pressed", "false");
});

test("j / k go by hunk, n / p by change; no more at the ends", async ({
  page,
  startServer,
  diffviewProject,
}) => {
  const server = await start(startServer, diffviewProject.root);
  const g = await open(page, server, "src/Calc.hs");
  // Focus on the row: its cell, or a link at column 0.
  const at = (label: string) => focusIn(row(g, label));
  await page.keyboard.press("j");
  await at("removed line 3");
  await page.keyboard.press("j");
  await at("removed line 91");
  await page.keyboard.press("j");
  await expect(page.locator(".keys-status")).toHaveText("No more changes.");
  await at("removed line 91");
  await page.keyboard.press("k");
  await at("removed line 3");
  await page.keyboard.press("n");
  await at("removed line 5");
  await page.keyboard.press("n");
  await at("removed line 7");
  await page.keyboard.press("n");
  await at("removed line 91");
  await page.keyboard.press("p");
  await at("removed line 7");
  // The overlay lists them.
  await page.keyboard.press("?");
  const dialog = page.getByRole("dialog");
  await expect(dialog).toContainText("Next declaration or hunk");
  await expect(dialog).toContainText("Next change");
  await expect(dialog).toContainText("Previous change");
});

// By keyboard: WebKit doesn't focus a clicked button.
test("the change buttons: the cursor moves, focus stays", async ({
  page,
  startServer,
  diffviewProject,
}) => {
  const server = await start(startServer, diffviewProject.root);
  const g = await open(page, server, "src/Calc.hs");
  const next = page.getByRole("button", { name: "Next change" });
  const prev = page.getByRole("button", { name: "Previous change" });
  const status = page.locator(".keys-status");
  await next.press("Enter");
  await expect(status).toHaveText("removed line 3");
  await next.press("Enter");
  await expect(status).toHaveText("removed line 5");
  await prev.press("Enter");
  await expect(status).toHaveText("removed line 3");
  await expect(prev).toBeFocused();
  await next.press("Enter");
  await next.press("Enter");
  await next.press("Enter");
  await expect(status).toHaveText("removed line 91");
  await expect(row(g, "removed line 91")).toBeInViewport();
  await next.press("Enter");
  await expect(status).toHaveText("No more changes.");
  await expect(next).toBeFocused();
  // Tab from the toolbar lands on the cursor's row.
  await expect(row(g, "removed line 91").locator('[tabindex="0"]')).toHaveCount(
    1,
  );
});

test("2 opens the cursor's side and line; Back comes to it", async ({
  page,
  startServer,
  diffviewProject,
}) => {
  const server = await start(startServer, diffviewProject.root);
  const g = await open(page, server, "src/Calc.hs");
  await row(g, "removed line 7").locator(".code-cell").click();
  await page.keyboard.press("2");
  await expect(page).toHaveURL(/\/src\/base\/src\/Calc\.hs#L7$/);
  await expect(page.locator("main h1")).toHaveText("src/Calc.hs");
  await page.goBack();
  await expect(page).toHaveURL(/\/diff\/src\/Calc\.hs#base-L7$/);
  await focusIn(row(g, "removed line 7"));
  await page.goBack();
  await expect(page).toHaveURL(/\/diff\/src\/Calc\.hs$/);

  // A link on an added line: Back to the link.
  await open(page, server, "src/Calc.hs");
  const triple = row(g, "added line 91").getByRole("link", {
    name: /^triple, /,
  });
  await triple.click();
  await expect(page).toHaveURL(/\/src\/head\/src\/Util\.hs#L6$/);
  await page.goBack();
  await expect(page).toHaveURL(/\/diff\/src\/Calc\.hs#head-L91$/);
  await expect(triple).toBeFocused();
});
