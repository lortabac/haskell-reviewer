// Preferences (D-197…D-205): themes, "exported only", sidebar memory,
// saved via api/prefs and read back on restart.
import AxeBuilder from "@axe-core/playwright";
import type { Page } from "@playwright/test";
import { mkdir, writeFile } from "node:fs/promises";
import { join } from "node:path";
import { expect, test } from "./support/fixtures";

const wcag = ["wcag2a", "wcag2aa", "wcag21a", "wcag21aa"];

async function writeConfig(configHome: string, toml: string): Promise<void> {
  const dir = join(configHome, "haskell-reviewer");
  await mkdir(dir, { recursive: true });
  await writeFile(join(dir, "config.toml"), toml);
}

// Set up before the action that triggers the POST (a response can
// arrive before a wait started after it): the save has reached the
// server (and been written) before the caller stops it (C5).
function prefsPostSaved(page: Page): Promise<unknown> {
  return page.waitForResponse(
    (r) => r.url().endsWith("/api/prefs") && r.request().method() === "POST",
  );
}

test("restart persistence: theme and exported only survive a restart", async ({
  page,
  startServer,
  docsProject,
}) => {
  const a = await startServer({ root: docsProject.root });
  await page.goto(a.base + "docs/tree/Shapes");
  const darkSaved = prefsPostSaved(page);
  await page.getByRole("button", { name: "Dark", exact: true }).click();
  await darkSaved;
  await expect(page.locator("html")).toHaveAttribute("data-theme", "dark");
  await expect(page.locator("body")).toHaveCSS(
    "background-color",
    "rgb(13, 17, 23)",
  );
  const box = page.getByRole("checkbox", { name: "Exported only" });
  const exportedOnlySaved = prefsPostSaved(page);
  await box.check();
  await exportedOnlySaved;
  await expect(page.locator("main")).not.toContainText("helper");
  await a.stop();

  // A new server, same state dir (the xdg fixture): a fresh port and
  // token, so this is really reading ui.json back, not the same page.
  const b = await startServer({ root: docsProject.root });
  await page.goto(b.base + "docs/tree/Shapes");
  await expect(page.locator("html")).toHaveAttribute("data-theme", "dark");
  await expect(page.locator("body")).toHaveCSS(
    "background-color",
    "rgb(13, 17, 23)",
  );
  await expect(
    page.getByRole("checkbox", { name: "Exported only" }),
  ).toBeChecked();
  await expect(page.locator("main")).not.toContainText("helper");
});

test("config default: Light wins over it, and survives a restart", async ({
  page,
  startServer,
  docsProject,
  xdg,
}) => {
  await writeConfig(xdg.configHome, '[ui]\ntheme = "dark"\n');
  const a = await startServer({ root: docsProject.root });
  await page.goto(a.base);
  await expect(page.locator("html")).toHaveAttribute("data-theme", "dark");
  const lightSaved = prefsPostSaved(page);
  await page.getByRole("button", { name: "Light", exact: true }).click();
  await lightSaved;
  await expect(page.locator("html")).toHaveAttribute("data-theme", "light");
  await a.stop();

  const b = await startServer({ root: docsProject.root });
  await page.goto(b.base);
  await expect(page.locator("html")).toHaveAttribute("data-theme", "light");
});

test("System follows the OS live, no override", async ({
  page,
  startServer,
  docsProject,
}) => {
  const server = await startServer({ root: docsProject.root });
  await page.emulateMedia({ colorScheme: "light" });
  await page.goto(server.base);
  await expect(
    page.getByRole("button", { name: "System", exact: true }),
  ).toHaveAttribute("aria-pressed", "true");
  await expect(page.locator("body")).toHaveCSS(
    "background-color",
    "rgb(255, 255, 255)",
  );
  await page.emulateMedia({ colorScheme: "dark" });
  await expect(page.locator("body")).toHaveCSS(
    "background-color",
    "rgb(13, 17, 23)",
  );
  await expect(page.locator("html")).not.toHaveAttribute("data-theme");
});

test("a good POST from the page saves right away, no restart needed", async ({
  page,
  startServer,
  docsProject,
}) => {
  const server = await startServer({ root: docsProject.root });
  await page.goto(server.base);
  const saved = prefsPostSaved(page);
  await page.getByRole("button", { name: "Dark", exact: true }).click();
  await saved;
  await expect(page.locator("html")).toHaveAttribute("data-theme", "dark");
  const prefs: unknown = await page.evaluate(() =>
    fetch("api/prefs").then((r) => r.json()),
  );
  expect(prefs).toMatchObject({ theme: "dark" });
  // No "couldn't save" status.
  await expect(page.locator(".prefs-status")).toHaveText("");
});

test("ThemeSwitch is keyboard operable", async ({
  page,
  startServer,
  docsProject,
}) => {
  const server = await startServer({ root: docsProject.root });
  await page.goto(server.base);
  const dark = page.getByRole("button", { name: "Dark", exact: true });
  await dark.focus();
  const darkSaved = prefsPostSaved(page);
  await page.keyboard.press("Enter");
  await darkSaved;
  await expect(dark).toHaveAttribute("aria-pressed", "true");
  await expect(page.locator("html")).toHaveAttribute("data-theme", "dark");
  // Space activates a focused button too.
  const light = page.getByRole("button", { name: "Light", exact: true });
  await light.focus();
  const lightSaved = prefsPostSaved(page);
  await page.keyboard.press(" ");
  await lightSaved;
  await expect(light).toHaveAttribute("aria-pressed", "true");
  await expect(dark).toHaveAttribute("aria-pressed", "false");
  await expect(page.locator("html")).toHaveAttribute("data-theme", "light");
});

test("a POST with a foreign Origin: 403, nothing written", async ({
  request,
  startServer,
  docsProject,
}) => {
  const server = await startServer({ root: docsProject.root });
  const res = await request.post(server.base + "api/prefs", {
    headers: {
      Origin: "http://evil.example",
      "Content-Type": "application/json",
    },
    data: { theme: "dark" },
  });
  expect(res.status()).toBe(403);
  const prefs: unknown = await (
    await request.get(server.base + "api/prefs")
  ).json();
  expect(prefs).toMatchObject({ theme: "system" });
});

test("bad config.toml: a warning, the page still works", async ({
  page,
  startServer,
  docsProject,
  xdg,
}) => {
  await writeConfig(xdg.configHome, "not valid toml ===\n[ui\n");
  const server = await startServer({ root: docsProject.root });
  await page.goto(server.base + "docs/tree/Shapes");
  await expect(page.locator("section.decl").first()).toBeVisible();
  // Built-in defaults, since the bad file is ignored (D-199).
  await expect(
    page.getByRole("button", { name: "System", exact: true }),
  ).toHaveAttribute("aria-pressed", "true");
  await expect(
    page.getByRole("checkbox", { name: "Exported only" }),
  ).not.toBeChecked();
  expect(
    server.log.some((l) => /config\.toml:\d+:\d+:/.test(l)),
    server.log.join("\n"),
  ).toBe(true);
});

for (const [button, bg] of [
  ["Dark", "rgb(13, 17, 23)"],
  ["Light", "rgb(255, 255, 255)"],
] as const) {
  test(`axe: theme forced to ${button} under the opposite OS`, async ({
    page,
    startServer,
    docsProject,
  }) => {
    const server = await startServer({ root: docsProject.root });
    await page.emulateMedia({
      colorScheme: button === "Dark" ? "light" : "dark",
    });
    await page.goto(server.base + "docs/tree/Shapes");
    await page.getByRole("button", { name: button, exact: true }).click();
    await expect(page.locator("body")).toHaveCSS("background-color", bg);
    await expect(page.locator("section.decl").first()).toBeVisible();
    const result = await new AxeBuilder({ page }).withTags(wcag).analyze();
    const summary = result.violations.map(
      (v) => `${v.id}: ${v.nodes.map((n) => n.target.join(" ")).join(", ")}`,
    );
    expect(summary).toEqual([]);
  });
}
