import { defineConfig, devices } from "@playwright/test";

// End-to-end tests against the real binary. Run with `make e2e`: it
// builds the binary and sets HASKELL_REVIEWER_EXE. Each test starts its
// own server (e2e/support/fixtures.ts), so there is no webServer and no
// baseURL.
const ci = process.env.CI !== undefined && process.env.CI !== "";

export default defineConfig({
  testDir: "e2e",
  fullyParallel: true,
  forbidOnly: ci,
  retries: ci ? 1 : 0,
  workers: ci ? 2 : undefined,
  // "github" shows flaky tests (passed on retry) as annotations.
  reporter: ci ? [["list"], ["github"], ["html", { open: "never" }]] : "list",
  outputDir: "test-results",
  use: {
    trace: "retain-on-failure",
  },
  projects: [
    { name: "chromium", use: { ...devices["Desktop Chrome"] } },
    { name: "firefox", use: { ...devices["Desktop Firefox"] } },
    { name: "webkit", use: { ...devices["Desktop Safari"] } },
  ],
});
