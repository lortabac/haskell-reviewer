import type { Page } from "@playwright/test";
import { expect } from "@playwright/test";
import { sidebar } from "./fixtures";

// Call before stopping a server the page still uses: wait until both
// sidebar trees are loaded, so no request is in flight (else the
// guard sees ERR_CONNECTION_REFUSED). Both panels are always mounted.
export async function settle(page: Page): Promise<void> {
  const nav = sidebar(page);
  await expect(nav).toBeAttached();
  await expect(nav).not.toContainText("Loading modules");
  await expect(nav).not.toContainText("Loading files");
}
