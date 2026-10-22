// The page frame: security headers, themes, accessibility.
import AxeBuilder from "@axe-core/playwright";
import type { Page } from "@playwright/test";
import { expect, notFound404, tabKey, test } from "./support/fixtures";

const csp =
  "default-src 'self'; img-src 'self' data:; connect-src 'self'; " +
  "script-src 'self'; object-src 'none'; base-uri 'none'; " +
  "form-action 'none'; frame-ancestors 'none'";

const wcag = ["wcag2a", "wcag2aa", "wcag21a", "wcag21aa"];

const themes = [
  { scheme: "light", bg: "rgb(255, 255, 255)" },
  { scheme: "dark", bg: "rgb(13, 17, 23)" },
] as const;

async function axe(page: Page, what: string): Promise<void> {
  const result = await new AxeBuilder({ page }).withTags(wcag).analyze();
  const summary = result.violations.map(
    (v) =>
      `${v.id} (${v.impact ?? "?"}): ${v.nodes
        .slice(0, 3)
        .map((n) => n.target.join(" "))
        .join(", ")}`,
  );
  expect(summary, `axe violations on ${what}`).toEqual([]);
}

test("CSP header, and the guard sees violations", async ({
  page,
  server,
  guard,
}) => {
  const res = await page.goto(server.base);
  expect(res?.headers()["content-security-policy"]).toBe(csp);
  await expect(page.locator(".tree-folder").first()).toBeVisible();

  // Positive control: an inline style must be blocked and seen. The
  // browser also logs it; only in this test is that fine.
  guard.allow(/Content.Security.Policy/i);
  await page.evaluate(() => {
    const s = document.createElement("style");
    s.textContent = "body { color: red; }";
    document.head.append(s);
  });
  await expect
    .poll(() => page.evaluate(() => window.__csp.map((e) => e.directive)))
    .toContain("style-src-elem");
  await expect.poll(() => guard.csp.length).toBeGreaterThan(0);
  await page.evaluate(() => {
    window.__csp = [];
  });
  guard.csp.length = 0;
});

for (const t of themes) {
  test(`${t.scheme} theme`, async ({ page, server }) => {
    await page.emulateMedia({ colorScheme: t.scheme });
    await page.goto(server.base);
    await expect(page.locator("body")).toHaveCSS("background-color", t.bg);
    await expect(page.locator("header")).toContainText("haskell-reviewer");
    await expect(page.getByRole("navigation", { name: "Files" })).toBeVisible();
  });

  test(`axe, ${t.scheme} theme`, async ({ page, server, guard }, testInfo) => {
    testInfo.setTimeout(60_000);
    guard.allow(notFound404);
    await page.emulateMedia({ colorScheme: t.scheme });
    const tree = page.getByRole("navigation", { name: "Files" });

    await page.goto(server.base);
    await tree.getByRole("button", { name: "src" }).click();
    await expect(tree.getByRole("link", { name: "Main.hs" })).toBeVisible();
    await axe(page, "home, src open");

    const pages: [string, RegExp | string][] = [
      ["src/Main.hs", "module Main where"],
      ["data/blob.bin", "not UTF-8"],
      ["link-to-main", "Symbolic link to"],
      ["vendor/nested", "Nested git repository"],
    ];
    for (const [path, text] of pages) {
      await page.goto(server.base + "src/tree/" + path);
      await expect(page.locator("main")).toContainText(text);
      await axe(page, path);
    }

    await page.goto(server.base + "src/tree/no/such.hs");
    await expect(page.locator("main h1")).toHaveText(
      "Not found in this snapshot",
    );
    await axe(page, "not found");
  });
}

test("skip link", async ({ page, server, browserName }) => {
  await page.goto(server.base);
  await expect(page.locator(".tree-folder").first()).toBeVisible();
  await page.keyboard.press(tabKey(browserName));
  const skip = page.getByRole("link", { name: "Skip to main content" });
  await expect(skip).toBeFocused();
  await page.keyboard.press("Enter");
  await expect(page.locator("#main")).toBeFocused();
});
