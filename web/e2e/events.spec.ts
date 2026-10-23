// The event stream: one per browser through the SharedWorker, the
// fallback, keep-alive, reconnect and the end of a session (Q-SSE).
// Tests tagged @slow wait for real keep-alives (15 s) or the 30 s
// give-up timer.
import type { BrowserContext, Page } from "@playwright/test";
import { expect, type Guard, showFiles, test } from "./support/fixtures";

const texts = {
  reconnecting: "Connection lost. Reconnecting…",
  ended: "This session has ended. Run haskell-reviewer again.",
  fallback:
    "This browser can't share one connection between tabs, so each tab keeps its own. With more than about five tabs open, pages may stop loading.",
};

const opened = (n: number): RegExp =>
  new RegExp(`event stream opened \\(${n} open\\)`);
const anyOpened = /event stream opened/;
const anyClosed = /event stream closed/;

// What a page with its own stream logs when the stream breaks. Only
// for /api/events. Chromium and WebKit: "Failed to load resource: ...",
// with the resource as the source URL. Firefox: its own lines.
const streamErrors = [
  /^error: Failed to load resource: .* \[\S*\/api\/events\]$/,
  /can’t establish a connection to the server at \S*\/api\/events\./,
  /The connection to \S*\/api\/events was interrupted/,
];

function allowStreamErrors(guard: Guard): void {
  for (const re of streamErrors) guard.allow(re);
}

type Mode = "shared" | "fallback";
const modes: Mode[] = ["shared", "fallback"];

// No SharedWorker: the page takes the fallback path.
async function useMode(context: BrowserContext, mode: Mode): Promise<void> {
  if (mode === "fallback") {
    await context.addInitScript(() => {
      Object.defineProperty(window, "SharedWorker", { value: undefined });
    });
  }
}

// The stream is open, so the tab has picked its path: the fallback
// notice shows in fallback mode only.
async function checkMode(page: Page, mode: Mode): Promise<void> {
  if (mode === "fallback") {
    await expect(notice(page, texts.fallback)).toBeVisible();
  } else {
    expect(await notices(page)).not.toContain(texts.fallback);
  }
}

function notices(page: Page): Promise<string[]> {
  return page.evaluate(() => window.__notices);
}

function notice(page: Page, text: string) {
  return page.locator(".connection-notice", { hasText: text });
}

// The page is up: the tree is there.
async function ready(page: Page): Promise<void> {
  await expect(page.locator(".tree-folder").first()).toBeVisible();
}

test("one stream for all tabs", async ({ context, server }) => {
  const pages: Page[] = [];
  for (let i = 0; i < 3; i++) {
    const p = await context.newPage();
    await p.goto(server.base);
    await ready(p);
    pages.push(p);
  }
  await server.waitForLog(opened(1), 10_000);
  await pages[2]!.waitForTimeout(3000);
  expect(server.count(anyOpened)).toBe(1);
  expect(server.count(anyClosed)).toBe(0);
  for (const p of pages) expect(await notices(p)).toEqual([]);
});

test("fallback: a stream per tab, and the notice", async ({
  context,
  server,
}) => {
  await useMode(context, "fallback");
  for (let i = 0; i < 2; i++) {
    const p = await context.newPage();
    await p.goto(server.base);
    await expect(notice(p, texts.fallback)).toBeVisible();
  }
  await server.waitForLog(opened(2), 10_000);
});

test("a second browser gets its own stream", async ({
  browser,
  page,
  server,
  guard,
}) => {
  await page.goto(server.base);
  await server.waitForLog(opened(1), 10_000);
  const other = await browser.newContext();
  try {
    await guard.watch(other);
    const p = await other.newPage();
    await p.goto(server.base);
    await server.waitForLog(opened(2), 10_000);
    expect(await notices(p)).toEqual([]);
  } finally {
    await other.close();
  }
});

test("keep-alive holds the stream @slow", async ({ page, server }) => {
  test.setTimeout(90_000);
  await page.goto(server.base);
  await server.waitForLog(opened(1), 10_000);
  await page.waitForTimeout(40_000);
  expect(server.count(anyClosed)).toBe(0);
  expect(await notices(page)).toEqual([]);
});

test("a closed browser is noticed @slow", async ({
  browser,
  page,
  server,
  guard,
}) => {
  test.setTimeout(90_000);
  await page.goto(server.base);
  await server.waitForLog(opened(1), 10_000);
  const other = await browser.newContext();
  try {
    await guard.watch(other);
    const p = await other.newPage();
    await p.goto(server.base);
    await server.waitForLog(opened(2), 10_000);
  } finally {
    await other.close();
  }
  await server.waitForLog(/event stream closed \(1 open\)/, 45_000);
});

for (const mode of modes) {
  test(`reconnect after a cut (${mode})`, async ({
    context,
    page,
    server,
    startProxy,
    guard,
  }) => {
    allowStreamErrors(guard);
    await useMode(context, mode);
    const proxy = await startProxy(server.port);
    await page.goto(proxy.url(server.token));
    await ready(page);
    await server.waitForLog(opened(1), 10_000);
    await checkMode(page, mode);
    proxy.cut();
    await expect.poll(() => notices(page)).toContain(texts.reconnecting);
    await server.waitForLog(anyOpened, 15_000, 2);
    await expect(notice(page, texts.reconnecting)).toHaveCount(0, {
      timeout: 15_000,
    });
    await expect(notice(page, texts.ended)).toHaveCount(0);
  });

  test(`a new run: the session has ended (${mode})`, async ({
    context,
    page,
    server,
    startServer,
    startProxy,
    guard,
  }) => {
    allowStreamErrors(guard);
    await useMode(context, mode);
    const proxy = await startProxy(server.port);
    await page.goto(proxy.url(server.token));
    await ready(page);
    await server.waitForLog(opened(1), 10_000);
    await checkMode(page, mode);
    await server.stop();
    const next = await startServer();
    proxy.retarget(next.port);
    proxy.cut();
    await expect(notice(page, texts.ended)).toBeVisible({ timeout: 15_000 });
  });
}

test("ended after 30 s without the server (fallback, fake clock)", async ({
  context,
  page,
  server,
  startProxy,
  guard,
}) => {
  allowStreamErrors(guard);
  await useMode(context, "fallback");
  await page.clock.install();
  const proxy = await startProxy(server.port);
  await page.goto(proxy.url(server.token));
  await ready(page);
  await server.waitForLog(opened(1), 10_000);
  await proxy.close();
  await expect(notice(page, texts.reconnecting)).toBeVisible();
  await page.clock.fastForward(31_000);
  await expect(notice(page, texts.ended)).toBeVisible();
  await expect(notice(page, texts.reconnecting)).toHaveCount(0);
});

test("ended after 30 s without the server @slow", async ({
  page,
  server,
  startProxy,
  guard,
}) => {
  allowStreamErrors(guard);
  test.setTimeout(90_000);
  const proxy = await startProxy(server.port);
  await page.goto(proxy.url(server.token));
  await ready(page);
  await server.waitForLog(opened(1), 10_000);
  await proxy.close();
  await expect(notice(page, texts.reconnecting)).toBeVisible();
  await expect(notice(page, texts.ended)).toBeVisible({ timeout: 45_000 });
});

test("a frozen tab keeps the stream @slow", async ({
  browserName,
  context,
  server,
}) => {
  test.skip(browserName !== "chromium", "CDP is Chromium only");
  test.setTimeout(90_000);
  const frozen = await context.newPage();
  await frozen.goto(server.base);
  await ready(frozen);
  const front = await context.newPage();
  await front.goto(server.base);
  await ready(front);
  await front.bringToFront();
  await server.waitForLog(opened(1), 10_000);

  const cdp = await context.newCDPSession(frozen);
  await cdp.send("Page.setWebLifecycleState", { state: "frozen" });
  await front.waitForTimeout(40_000);
  expect(server.count(anyClosed)).toBe(0);
  await cdp.send("Page.setWebLifecycleState", { state: "active" });

  await frozen.bringToFront();
  await showFiles(frozen);
  await frozen.getByRole("link", { name: "README.md", exact: true }).click();
  await expect(frozen.locator("main h1")).toHaveText("README.md");
  expect(await notices(frozen)).toEqual([]);
  expect(server.count(opened(2))).toBe(0);
});
