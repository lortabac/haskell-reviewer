import { defineConfig } from "vitest/config";
import solid from "vite-plugin-solid";

// The server serves the bundle under /<token>/. Asset refs stay relative
// ("./assets/...") and the server rewrites them in index.html. A <base>
// tag would be simpler, but the CSP has base-uri 'none'.
export default defineConfig({
  plugins: [solid()],
  base: "./",
  // No public/ dir: every file in dist must have a hashed name.
  publicDir: false,
  build: {
    outDir: "dist",
    emptyOutDir: true,
    sourcemap: false,
    // The polyfill is an inline script. The CSP blocks it.
    modulePreload: { polyfill: false },
    // No data: URLs for assets.
    assetsInlineLimit: 0,
  },
  // Classic (iife) workers: module SharedWorkers are not supported
  // everywhere yet. A classic worker can't use import, so Vite bundles
  // it into one file.
  worker: { format: "iife" },
  test: {
    environment: "happy-dom",
    // The persistent module cache keeps the import.meta.glob list of
    // test/fixtures.ts, so an added or removed fixture would go
    // unseen. The whole run takes a few seconds anyway.
    fsModuleCache: false,
  },
});
