// Accessibility beyond the other specs' own checks (SPEC §5.4, D-211):
// a full keyboard tab-through, reflow at 320 px, and the header's
// indexing progress line (fake EventSource, no real indexing run to
// wait for).
import type { Page } from "@playwright/test";
import { expect, moduleNotFound404, tabKey, test } from "./support/fixtures";
import {
  expectNoAxeViolations as axe,
  fakeEventSource,
  noSideways,
  themes,
} from "./support/a11y";

interface Stop {
  lost: false;
  tag: string;
  text: string;
  // Rounded position: identifies the actual element (two different
  // links can share a tag and a truncated text, e.g. two "Shape"s in
  // a row), so "the same stop twice in a row" below means Tab really
  // didn't move, not just a text collision.
  top: number;
  left: number;
  visible: boolean;
  hasRing: boolean;
  landmark: "HEADER" | "NAV" | "MAIN" | null;
}

function key(s: Stop): string {
  return `${s.tag}|${s.text}|${s.top}|${s.left}`;
}

// A plain page doesn't wrap Tab back to its own first element: past
// the last stop, focus leaves the page (document.activeElement becomes
// body), the way it would move on to the browser's own UI in a real
// window. { lost: true } marks that, the other end of the walk.
async function focusedStop(page: Page): Promise<{ lost: true } | Stop> {
  return page.evaluate(() => {
    const el = document.activeElement;
    if (el === null || el === document.body) {
      return { lost: true as const };
    }
    const style = getComputedStyle(el);
    const rect = el.getBoundingClientRect();
    const visible =
      style.visibility !== "hidden" &&
      style.display !== "none" &&
      el.closest("[hidden]") === null &&
      rect.width > 0 &&
      rect.height > 0;
    const hasRing =
      style.outlineStyle !== "none" &&
      Number.parseFloat(style.outlineWidth) >= 2;
    const landmark = el.closest("header, nav, main");
    return {
      lost: false as const,
      tag: el.tagName,
      text: (el.textContent ?? "").trim().slice(0, 40),
      top: Math.round(rect.top),
      left: Math.round(rect.left),
      visible,
      hasRing,
      landmark:
        landmark === null
          ? null
          : (landmark.tagName as "HEADER" | "NAV" | "MAIN"),
    };
  });
}

const landmarkStage = { HEADER: 1, NAV: 2, MAIN: 3 } as const;

for (const t of themes) {
  test(`tab-through: every stop is visible with a ring, landmarks in order, ${t.scheme} theme`, async ({
    page,
    startServer,
    docsProject,
    browserName,
  }, testInfo) => {
    testInfo.setTimeout(60_000);
    const tab = tabKey(browserName);
    const server = await startServer({ root: docsProject.root });
    await page.emulateMedia({ colorScheme: t.scheme });
    await page.goto(server.base + "docs/tree/Shapes");
    await expect(page.locator("section.decl").first()).toBeVisible();

    await page.keyboard.press(tab);
    const first = await focusedStop(page);
    if (first.lost) throw new Error("focus is lost at the first stop");
    expect(first.text).toBe("Skip to main content");

    let maxStage = 0;
    let lastKey = key(first);
    // Set once the walk actually finishes: focus left the document,
    // wrapped back to the skip link, or (headless Firefox, past the
    // last stop on the page: a closed <summary> at the very end) Tab
    // stopped moving focus at all. Not just when the loop below runs
    // out of iterations (C7: the cap is a safety net, not proof the
    // page has that many stops or that it ever ends).
    let walkEnded = false;
    const maxTabs = 150;
    for (let i = 1; i <= maxTabs; i++) {
      await page.keyboard.press(tab);
      const stop = await focusedStop(page);
      if (stop.lost || stop.text === "Skip to main content") {
        walkEnded = true;
        break;
      }
      const stopKey = key(stop);
      if (stopKey === lastKey) {
        walkEnded = true;
        break;
      }
      lastKey = stopKey;
      expect(stop.visible, `stop ${i} (${stop.tag} "${stop.text}")`).toBe(true);
      expect(stop.hasRing, `stop ${i} (${stop.tag} "${stop.text}")`).toBe(true);
      if (stop.landmark !== null) {
        const stage = landmarkStage[stop.landmark];
        expect(
          stage,
          `landmark order regressed at stop ${i} (${stop.landmark})`,
        ).toBeGreaterThanOrEqual(maxStage);
        maxStage = Math.max(maxStage, stage);
      }
    }
    expect(walkEnded, `the tab walk never ended within ${maxTabs} Tabs`).toBe(
      true,
    );
    // Reached all three landmarks: header, nav (sidebar), main.
    expect(maxStage).toBe(3);
  });
}

test("skip link, then Tab, lands on the first focusable thing in main", async ({
  page,
  startServer,
  docsProject,
  browserName,
}) => {
  const tab = tabKey(browserName);
  // Home has no focusable content at all, so use a module page: it
  // starts with the "Exported only" checkbox.
  const server = await startServer({ root: docsProject.root });
  await page.goto(server.base + "docs/tree/Shapes");
  await expect(page.locator("section.decl").first()).toBeVisible();
  await page.keyboard.press(tab);
  const skip = page.getByRole("link", { name: "Skip to main content" });
  await expect(skip).toBeFocused();
  await page.keyboard.press("Enter");
  await expect(page.locator("#main")).toBeFocused();
  await page.keyboard.press(tab);
  const insideMain = await page.evaluate(
    () => document.activeElement?.closest("#main") !== null,
  );
  expect(insideMain).toBe(true);
});

// Every header child's right edge must stay inside the viewport: a
// flex-wrap header can still overflow a single child that's too wide.
async function headerFits(page: Page): Promise<void> {
  const overflow = await page.evaluate(() => {
    const header = document.querySelector(".frame-header");
    if (header === null) throw new Error("no header");
    const vw = document.documentElement.clientWidth;
    return [...header.children].map(
      (c) => c.getBoundingClientRect().right - vw,
    );
  });
  for (const over of overflow) {
    expect(over).toBeLessThanOrEqual(0.5); // sub-pixel rounding
  }
}

// The stacked sidebar (frame.css, below 40em) must size to its own
// content, not grow to fill the page on a short one like Home (C3:
// `grid-template-rows: auto 1fr` handed that row to the sidebar
// instead of main). Capped at 40vh regardless.
async function sidebarFits(page: Page): Promise<void> {
  const { navHeight, viewportHeight } = await page.evaluate(() => {
    const nav = document.querySelector("nav.frame-sidebar");
    if (nav === null) throw new Error("no sidebar");
    return {
      navHeight: nav.getBoundingClientRect().height,
      viewportHeight: window.innerHeight,
    };
  });
  expect(navHeight).toBeLessThanOrEqual(viewportHeight * 0.4 + 2);
}

test.describe("reflow at 320 px (SPEC §5.4)", () => {
  test("home, a file, a module page with source open, not found, header popup open", async ({
    page,
    startServer,
    project,
    docsProject,
    guard,
  }) => {
    guard.allow(moduleNotFound404);
    await page.setViewportSize({ width: 320, height: 640 });

    const main = await startServer({ root: project.root });
    await page.goto(main.base);
    await expect(page.locator("main h1")).toHaveText("Home");
    await noSideways(page);
    await headerFits(page);
    await sidebarFits(page);

    await page.goto(main.base + "src/tree/src/Main.hs");
    await expect(page.locator(".file-code")).toBeVisible();
    await noSideways(page);
    await headerFits(page);
    await sidebarFits(page);

    const docs = await startServer({ root: docsProject.root });
    await page.goto(docs.base + "docs/tree/Shapes");
    await expect(page.locator("section.decl").first()).toBeVisible();
    await page
      .getByRole("button", { name: /Show source/ })
      .first()
      .click();
    await expect(page.locator("pre.src")).toBeVisible();
    await noSideways(page);
    await headerFits(page);
    await sidebarFits(page);

    await page.goto(docs.base + "docs/tree/Nope");
    await expect(page.locator("main h1")).toHaveText(
      "Not found in this snapshot",
    );
    await noSideways(page);
    await headerFits(page);
    await sidebarFits(page);

    const box = page
      .locator(".frame-header")
      .getByRole("combobox", { name: "Search modules and names" });
    await box.fill("Shapes");
    await expect(
      page.locator(".frame-header .search-results a").first(),
    ).toBeVisible();
    await noSideways(page);
    await headerFits(page);
    await sidebarFits(page);
  });

  for (const t of themes) {
    test(`axe on a module page, ${t.scheme} theme`, async ({
      page,
      startServer,
      docsProject,
    }) => {
      await page.setViewportSize({ width: 320, height: 640 });
      await page.emulateMedia({ colorScheme: t.scheme });
      const server = await startServer({ root: docsProject.root });
      await page.goto(server.base + "docs/tree/Shapes");
      await expect(page.locator("section.decl").first()).toBeVisible();
      await axe(page, "module page at 320px");
    });
  }

  // One more check at 640 px: 200% zoom of a 1280 px window (SPEC §5.4).
  test("one more check at 640 px", async ({
    page,
    startServer,
    docsProject,
  }) => {
    await page.setViewportSize({ width: 640, height: 800 });
    const server = await startServer({ root: docsProject.root });
    await page.goto(server.base + "docs/tree/Shapes");
    await expect(page.locator("section.decl").first()).toBeVisible();
    await noSideways(page);
  });
});

async function emit(page: Page, type: string, data?: unknown): Promise<void> {
  await page.evaluate(({ type: t, data: d }) => window.__hrEmit?.(t, d), {
    type,
    data,
  });
}

test.describe("indexing progress in the header (SPEC §10.2)", () => {
  for (const t of themes) {
    test(`shown after 1 s, hidden when done, a failure stays, ${t.scheme} theme`, async ({
      page,
      context,
      server,
    }, testInfo) => {
      testInfo.setTimeout(30_000);
      await fakeEventSource(context);
      await page.emulateMedia({ colorScheme: t.scheme });
      await page.goto(server.base);
      await expect(page.locator(".tree-folder").first()).toBeVisible();

      await emit(page, "hello", {});
      await emit(page, "indexing", {
        generation: 1,
        total: 10,
        ready: 3,
        failed: 1,
      });

      const status = page.locator(".indexing-status");
      await expect(status).toHaveText("");
      await page.waitForTimeout(1100);
      await expect(status).toHaveText("Indexing 4 of 10 modules (1 failed)");
      await axe(page, "indexing in progress");

      await emit(page, "indexing", {
        generation: 1,
        total: 10,
        ready: 9,
        failed: 1,
      });
      await expect(status).toHaveText("1 module couldn't be indexed");
      const live = page.locator('.indexing-live[role="status"]');
      await expect(live).toHaveText(
        "Indexing done; 1 module couldn't be indexed.",
      );

      // A replay of the same settled state says nothing new.
      await emit(page, "indexing", {
        generation: 1,
        total: 10,
        ready: 9,
        failed: 1,
      });
      await expect(live).toHaveText(
        "Indexing done; 1 module couldn't be indexed.",
      );
    });
  }

  test("a fast done-with-failures run (under 1 s): the failure line still shows (C2)", async ({
    page,
    context,
    server,
  }) => {
    await fakeEventSource(context);
    await page.goto(server.base);
    await expect(page.locator(".tree-folder").first()).toBeVisible();

    await emit(page, "hello", {});
    // Done from the very first event: never "shown" by the 1 s delay.
    await emit(page, "indexing", {
      generation: 1,
      total: 10,
      ready: 9,
      failed: 1,
    });
    const status = page.locator(".indexing-status");
    await expect(status).toHaveText("1 module couldn't be indexed");
    // Never shown, so no announcement for this one (choice 3A).
    await expect(page.locator('.indexing-live[role="status"]')).toHaveText("");
  });

  test(`a real server, no failures: the header never says "Indexing"`, async ({
    page,
    server,
  }) => {
    await page.goto(server.base);
    await expect(page.locator(".tree-folder").first()).toBeVisible();
    // The fixture project is tiny: real indexing finishes well inside
    // the 1 s delay, so nothing should ever show.
    await page.waitForTimeout(1500);
    await expect(page.locator(".indexing-status")).not.toContainText(
      "Indexing",
    );
  });
});
