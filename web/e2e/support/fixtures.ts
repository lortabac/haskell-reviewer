// Test fixtures: the fixture project, servers, proxies, and the guard
// that fails a test on any CSP violation or browser error.
import {
  type BrowserContext,
  expect,
  type Locator,
  type Page,
  test as base,
} from "@playwright/test";
import { mkdtemp, rm } from "node:fs/promises";
import { tmpdir } from "node:os";
import { join } from "node:path";
import {
  makeCabalProject,
  makeDocsProject,
  makeProject,
  type Project,
} from "./project";
import { type Proxy, startProxy } from "./proxy";
import { type Server, startServer } from "./server";

export interface CspEntry {
  directive: string;
  blockedURI: string;
  sourceFile: string;
}

export interface Guard {
  // Let browser errors matching re pass, for this test.
  allow: (re: RegExp) => void;
  // CSP violations seen so far, on every watched page.
  csp: CspEntry[];
  // Watch one more context (the test's own context is watched).
  watch: (context: BrowserContext) => Promise<void>;
}

declare global {
  interface Window {
    __csp: CspEntry[];
    __notices: string[];
    __hrCsp?: (entry: CspEntry) => void;
    __pwned?: unknown;
  }
}

// Runs in every page before its own scripts.
function initScript(): void {
  window.__csp = [];
  window.__notices = [];
  document.addEventListener("securitypolicyviolation", (e) => {
    const entry = {
      directive: e.effectiveDirective,
      blockedURI: e.blockedURI,
      sourceFile: e.sourceFile,
    };
    window.__csp.push(entry);
    window.__hrCsp?.(entry);
  });
  const seen = new WeakSet<Element>();
  const record = (): void => {
    for (const el of document.querySelectorAll(".connection-notice")) {
      if (seen.has(el)) continue;
      seen.add(el);
      window.__notices.push(el.textContent);
    }
  };
  new MutationObserver(record).observe(document, {
    childList: true,
    subtree: true,
    characterData: true,
  });
}

interface TestFixtures {
  server: Server;
  // Default root: the main fixture project. Default XDG dirs: the
  // test's own, empty at start (xdg), shared by its servers, so a
  // restart sees what the first server saved.
  startServer: (opts?: {
    root?: string;
    configHome?: string;
    stateHome?: string;
  }) => Promise<Server>;
  xdg: { configHome: string; stateHome: string };
  startProxy: (backend: number) => Promise<Proxy>;
  guard: Guard;
}

interface WorkerFixtures {
  project: Project;
  cabalProject: Project;
  docsProject: Project;
}

export const test = base.extend<TestFixtures, WorkerFixtures>({
  project: [
    // eslint-disable-next-line no-empty-pattern
    async ({}, use) => {
      const p = await makeProject();
      await use(p);
      await p.remove();
    },
    { scope: "worker" },
  ],

  cabalProject: [
    // eslint-disable-next-line no-empty-pattern
    async ({}, use) => {
      const p = await makeCabalProject();
      await use(p);
      await p.remove();
    },
    { scope: "worker" },
  ],

  docsProject: [
    // eslint-disable-next-line no-empty-pattern
    async ({}, use) => {
      const p = await makeDocsProject();
      await use(p);
      await p.remove();
    },
    { scope: "worker" },
  ],

  // eslint-disable-next-line no-empty-pattern
  xdg: async ({}, use) => {
    const dir = await mkdtemp(join(tmpdir(), "hr-e2e-xdg-"));
    await use({
      configHome: join(dir, "config"),
      stateHome: join(dir, "state"),
    });
    await rm(dir, { recursive: true, force: true });
  },

  startServer: async ({ project, xdg }, use) => {
    const servers: Server[] = [];
    await use(async (opts) => {
      const s = await startServer({
        root: opts?.root ?? project.root,
        configHome: opts?.configHome ?? xdg.configHome,
        stateHome: opts?.stateHome ?? xdg.stateHome,
      });
      servers.push(s);
      return s;
    });
    await Promise.all(servers.map((s) => s.stop()));
  },

  server: async ({ startServer }, use) => {
    await use(await startServer());
  },

  // eslint-disable-next-line no-empty-pattern
  startProxy: async ({}, use) => {
    const proxies: Proxy[] = [];
    await use(async (backend) => {
      const p = await startProxy(backend);
      proxies.push(p);
      return p;
    });
    await Promise.all(proxies.map((p) => p.close()));
  },

  // Depends on the servers and proxies so it is checked before they
  // stop: a stopped server makes the pages log errors.
  guard: [
    async ({ context, server, startProxy }, use, testInfo) => {
      void server;
      void startProxy;
      const allowed: RegExp[] = [];
      const errors: string[] = [];
      const warnings: string[] = [];
      const csp: CspEntry[] = [];
      const pages: Page[] = [];

      const watchPage = (page: Page): void => {
        pages.push(page);
        page.on("console", (m) => {
          // The source URL: for a failed load, the resource.
          const text = `${m.type()}: ${m.text()} [${m.location().url}]`;
          if (m.type() === "error") errors.push(text);
          else if (m.type() === "warning") warnings.push(text);
        });
        page.on("pageerror", (e) => errors.push(`pageerror: ${e.message}`));
      };
      const watch = async (ctx: BrowserContext): Promise<void> => {
        await ctx.exposeFunction("__hrCsp", (entry: CspEntry) => {
          csp.push(entry);
        });
        await ctx.addInitScript(initScript);
        ctx.on("page", watchPage);
        for (const p of ctx.pages()) watchPage(p);
      };
      await watch(context);

      await use({ allow: (re) => allowed.push(re), csp, watch });

      if (warnings.length > 0) {
        await testInfo.attach("browser warnings", {
          body: warnings.join("\n"),
          contentType: "text/plain",
        });
      }
      for (const p of pages) {
        if (p.isClosed()) continue;
        const inPage = await p
          .evaluate(() => window.__csp)
          .catch(() => [] as CspEntry[]);
        expect(inPage, `CSP violations on ${p.url()}`).toEqual([]);
      }
      expect(csp, "CSP violations").toEqual([]);
      const unexpected = errors.filter((e) => !allowed.some((r) => r.test(e)));
      expect(unexpected, "browser errors").toEqual([]);
    },
    { auto: true },
  ],
});

export { expect };

// Chromium's console line for a 404 from the file API. Only there: a
// 404 on an asset must fail.
export const notFound404 =
  /Failed to load resource: the server responded with a status of 404.* \[\S*\/api\/file\/tree\//;

// The same for the module API (an unknown module).
export const moduleNotFound404 =
  /Failed to load resource: the server responded with a status of 404.* \[\S*\/api\/module\/tree\//;

// The sidebar: the Project nav.
export function sidebar(page: Page): Locator {
  return page.getByRole("navigation", { name: "Project" });
}

// Switch the sidebar to the file tree.
export async function showFiles(page: Page): Promise<void> {
  const b = sidebar(page).getByRole("button", { name: "Files", exact: true });
  await b.click();
  await expect(b).toHaveAttribute("aria-pressed", "true");
}

// On macOS, Tab skips links in WebKit and Chromium; Alt+Tab reaches them.
export function tabKey(browserName: string): string {
  return process.platform === "darwin" && browserName !== "firefox"
    ? "Alt+Tab"
    : "Tab";
}
