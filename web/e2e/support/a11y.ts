// Shared accessibility helpers (SPEC §5.4, D-211): the axe check every
// spec runs (WCAG plus axe's "best practice" rules, choice 6), the
// light/dark themes those checks run in, a sideways-scroll check, and
// a fake EventSource so a spec can drive the indexing progress UI by
// hand, with no real indexing run to wait for.
import AxeBuilder from "@axe-core/playwright";
import type { BrowserContext, Page } from "@playwright/test";
import { expect } from "@playwright/test";

export const wcagTags = [
  "wcag2a",
  "wcag2aa",
  "wcag21a",
  "wcag21aa",
  "best-practice",
];

export const themes = [
  { scheme: "light", bg: "rgb(255, 255, 255)" },
  { scheme: "dark", bg: "rgb(13, 17, 23)" },
] as const;

// Rules turned off for the whole app, with why (a real issue gets
// fixed, not silenced). Empty: nothing needed one yet.
const disabledRules: Record<string, string> = {};

export interface AxeOptions {
  // Extra rules to turn off for this one check, each with its own
  // reason (SPEC §5.4: listed in the task report, not just here).
  disable?: Record<string, string>;
}

export async function expectNoAxeViolations(
  page: Page,
  what: string,
  opts?: AxeOptions,
): Promise<void> {
  const disable = { ...disabledRules, ...(opts?.disable ?? {}) };
  let builder = new AxeBuilder({ page }).withTags(wcagTags);
  const ids = Object.keys(disable);
  if (ids.length > 0) builder = builder.disableRules(ids);
  const result = await builder.analyze();
  const summary = result.violations.map(
    (v) =>
      `${v.id} (${v.impact ?? "?"}): ${v.nodes
        .slice(0, 3)
        .map((n) => n.target.join(" "))
        .join(", ")}`,
  );
  expect(summary, `axe violations on ${what}`).toEqual([]);
}

// No sideways scroll on the whole page (SPEC §5.4 reflow).
export async function noSideways(page: Page): Promise<void> {
  const doc = await page.evaluate(() => ({
    scroll: document.documentElement.scrollWidth,
    client: document.documentElement.clientWidth,
  }));
  expect(doc.scroll).toBeLessThanOrEqual(doc.client);
}

declare global {
  interface Window {
    // Sends `type` (e.g. "hello", "indexing") to every fake
    // EventSource on the page, with `data` as its JSON payload (or no
    // payload at all for "open").
    __hrEmit?: (type: string, data?: unknown) => void;
  }
}

// No SharedWorker (so the tab runs its own stream, as events.spec.ts's
// `useMode("fallback")` does) and a fake EventSource in its place, so
// a spec can emit "indexing" events without a real indexing run. Must
// be added before the page's own scripts run (before page.goto).
export async function fakeEventSource(context: BrowserContext): Promise<void> {
  await context.addInitScript(() => {
    Object.defineProperty(window, "SharedWorker", { value: undefined });

    const sources: FakeEventSource[] = [];

    class FakeEventSource {
      static readonly CONNECTING = 0;
      static readonly OPEN = 1;
      static readonly CLOSED = 2;
      readyState = FakeEventSource.OPEN;
      private listeners = new Map<string, ((e: MessageEvent) => void)[]>();

      constructor(url: string) {
        void url; // SourceCtor's shape (hub.ts); this fake ignores it.
        sources.push(this);
        // Same tick as hub.ts's own "open" listener attaches: a
        // microtask, not a macrotask, so tests don't need a real wait.
        queueMicrotask(() => this.dispatch("open"));
      }

      addEventListener(
        type: string,
        listener: (e: MessageEvent) => void,
      ): void {
        const list = this.listeners.get(type) ?? [];
        list.push(listener);
        this.listeners.set(type, list);
      }

      close(): void {
        this.readyState = FakeEventSource.CLOSED;
      }

      dispatch(type: string, data?: string): void {
        for (const l of this.listeners.get(type) ?? []) {
          l({ data } as MessageEvent);
        }
      }
    }

    window.EventSource =
      FakeEventSource as unknown as typeof window.EventSource;
    window.__hrEmit = (type, data) => {
      const payload = data === undefined ? undefined : JSON.stringify(data);
      for (const s of sources) s.dispatch(type, payload);
    };
  });
}
