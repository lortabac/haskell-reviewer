// Home in diff mode and the header's outside-ROOT count (D-338).
import { afterEach, describe, expect, it, vi } from "vitest";
import { stubFetch } from "../../test/fixtures";
import { mountAppAt, unmountApp } from "../../test/mount";
import { diffSession } from "../../test/sessions";
import { defaultPrefs } from "../api/prefs";
import type { Session } from "../api/session";
import { firstRows } from "./DiffSummary";

const entry = (name: string, change: string | null, settings = false) => ({
  name,
  id: `p:lib:${name}`,
  ref: name,
  role: "exposed",
  source: { kind: "module", path: `src/${name}.hs` },
  change,
  settingsChanged: settings,
});

const modules = (mods: unknown[]) => ({
  generation: 1,
  notices: [],
  packages: [
    {
      name: "p",
      version: "1",
      cabalFile: "p.cabal",
      components: [{ name: "lib", buildable: true, modules: mods }],
    },
  ],
  broken: [],
  other: [],
  warnings: [],
});

const change = (
  path: string,
  status: string,
  oldPath: string | null = null,
) => ({
  path,
  oldPath,
  status,
  similarity: status === "renamed" ? 90 : null,
  note: null,
});

const outsidePaths = Array.from({ length: 12 }, (_, i) => `other/f${i}`);

const diffFiles = (over: Record<string, unknown> = {}) => ({
  generation: 1,
  base: { label: "main", commit: "a".repeat(40) },
  head: { label: "feature", commit: "b".repeat(40) },
  files: [
    change("docs/new.md", "renamed", "docs/old.md"),
    change("src/A.hs", "modified"),
    change("src/B.hs", "removed"),
    change("src/C.hs", "added"),
  ],
  outside: { total: 12, paths: outsidePaths, truncated: false },
  renameLimitHit: false,
  truncated: false,
  ...over,
});

function mount(
  opts: {
    diff?: [number, unknown];
    browse?: boolean;
    pending?: string[];
    baseModules?: [number, unknown];
    headModules?: [number, unknown];
    session?: Session;
  } = {},
): HTMLElement {
  const fetch = stubFetch({
    "/tok/api/modules/head": opts.headModules ?? [
      200,
      modules([
        entry("A", "modified", true),
        entry("C", "added"),
        entry("D", null),
      ]),
    ],
    "/tok/api/modules/base": opts.baseModules ?? [
      200,
      modules([
        entry("A", "modified"),
        entry("B", "removed"),
        entry("D", null),
      ]),
    ],
    "/tok/api/modules/tree": [200, modules([entry("A", null)])],
    "/tok/api/diff-files": opts.diff ?? [200, diffFiles()],
    "/tok/api/prefs": [200, defaultPrefs],
  });
  const pending = opts.pending ?? [];
  if (pending.length > 0) {
    const answer = fetch.getMockImplementation();
    fetch.mockImplementation((input: RequestInfo | URL) => {
      const url = typeof input === "string" ? input : "";
      return pending.some((p) => url.endsWith(p))
        ? new Promise<Response>(() => undefined)
        : (answer?.(input) ?? Promise.reject(new Error("no stub")));
    });
  }
  return mountAppAt(
    "/tok",
    defaultPrefs,
    undefined,
    null,
    opts.browse === true ? undefined : (opts.session ?? diffSession),
  );
}

const main = (root: HTMLElement): HTMLElement => {
  const m = root.querySelector<HTMLElement>("main");
  if (m === null) throw new Error("no main");
  return m;
};

const text = (e: Element | null | undefined): string =>
  (e?.textContent ?? "").replace(/\s+/g, " ").trim();

const counts = (root: HTMLElement): string[] =>
  [...main(root).querySelectorAll(".home-counts")].map(text);

const rows = (root: HTMLElement) => [
  ...main(root).querySelectorAll<HTMLLIElement>(".home-files li"),
];

afterEach(() => {
  unmountApp();
  vi.unstubAllGlobals();
  vi.restoreAllMocks();
});

describe("Home in diff mode", () => {
  it("revisions as text, file and module counts", async () => {
    const root = mount();
    expect(text(main(root).querySelector(".home-revisions"))).toBe(
      "Diff main..feature. Base: main (aaaaaaa). Head: feature (bbbbbbb).",
    );
    expect(main(root).querySelector("button.copy-button")).toBeNull();
    await vi.waitFor(() => {
      expect(counts(root)).toEqual([
        "4 files changed: 1 added, 1 removed, 1 modified, 1 renamed.",
        "3 modules changed: 1 added, 1 removed, 1 modified; settings changed in 1 module.",
        "12 files changed outside ROOT.",
      ]);
    });
  });

  it("the list: signs, links on the right side, removed ones on base", async () => {
    const root = mount();
    await vi.waitFor(() => {
      expect(rows(root)).toHaveLength(4);
    });
    const [ren, mod, rem, add] = rows(root);
    expect(text(ren)).toBe("→renamed docs/new.md from docs/old.md (90%)");
    expect(ren?.querySelector("a")?.getAttribute("href")).toBe(
      "/tok/src/head/docs/new.md",
    );
    expect(mod?.querySelector("a")?.getAttribute("href")).toBe(
      "/tok/src/head/src/A.hs",
    );
    const a = rem?.querySelector("a");
    expect(a?.getAttribute("href")).toBe("/tok/src/base/src/B.hs");
    expect(text(a)).toBe("src/B.hs (on base)");
    expect(rem?.querySelector(".sr-only")?.textContent).toBe("removed");
    expect(add?.querySelector("[aria-hidden=true]")?.textContent).toBe("+");
  });

  it("outside ROOT: the paths in a details, as text", async () => {
    const root = mount({
      diff: [
        200,
        diffFiles({
          outside: { total: 2, paths: ["<b>x</b>", "o/y"], truncated: false },
        }),
      ],
    });
    await vi.waitFor(() => {
      expect(main(root).querySelector(".home-outside")).not.toBeNull();
    });
    const items = [...main(root).querySelectorAll(".home-outside li")];
    expect(items.map((i) => i.textContent)).toEqual(["<b>x</b>", "o/y"]);
    expect(main(root).querySelector(".home-outside b")).toBeNull();
  });

  it("outside ROOT: not known, none, truncated", async () => {
    let root = mount({ diff: [200, diffFiles({ outside: null })] });
    await vi.waitFor(() => {
      expect(text(main(root))).toContain("Outside ROOTNot known.");
    });
    unmountApp();
    root = mount({
      diff: [
        200,
        diffFiles({ outside: { total: 0, paths: [], truncated: false } }),
      ],
    });
    await vi.waitFor(() => {
      expect(text(main(root))).toContain("No files changed outside ROOT.");
    });
    unmountApp();
    root = mount({
      diff: [
        200,
        diffFiles({ outside: { total: 100, paths: ["a"], truncated: true } }),
      ],
    });
    await vi.waitFor(() => {
      expect(counts(root)).toContain("100+ files changed outside ROOT.");
    });
  });

  it("notes: rename limit, truncated; nothing changed", async () => {
    const root = mount({
      diff: [
        200,
        diffFiles({ files: [], renameLimitHit: true, truncated: true }),
      ],
    });
    await vi.waitFor(() => {
      expect(counts(root)[0]).toBe("No files changed under ROOT.");
    });
    const t = text(main(root));
    expect(t).toContain("only exact and same-name renames were looked for");
    expect(t).toContain("A side has over 100,000 files");
    expect(main(root).querySelector(".home-files")).toBeNull();
  });

  it(`${firstRows} rows, then Show all`, async () => {
    const files = Array.from({ length: firstRows + 5 }, (_, i) =>
      change(`f/${String(i).padStart(5, "0")}`, "added"),
    );
    const root = mount({ diff: [200, diffFiles({ files })] });
    await vi.waitFor(() => {
      expect(rows(root)).toHaveLength(firstRows);
    });
    const button =
      main(root).querySelector<HTMLButtonElement>(".home-show-all");
    expect(text(button)).toBe("Show all 1,005");
    button?.click();
    expect(rows(root)).toHaveLength(firstRows + 5);
    expect(main(root).querySelector(".home-show-all")).toBeNull();
    // Focus goes to the first new row, not body.
    expect(document.activeElement).toBe(
      rows(root)[firstRows]?.querySelector("a"),
    );
  });

  it("loading: a note, no counts; failed: a note", async () => {
    let root = mount({ pending: ["/api/diff-files"] });
    await vi.waitFor(() => {
      expect(text(main(root))).toContain("Loading changes…");
    });
    expect(counts(root)).toEqual([]);
    expect(root.querySelector(".outside-count")).toBeNull();
    unmountApp();
    root = mount({ diff: [500, "cannot pair files"] });
    await vi.waitFor(() => {
      expect(text(main(root))).toContain("Couldn't load the changes.");
    });
    expect(counts(root)).toEqual([]);
    expect(root.querySelector(".outside-count")).toBeNull();
  });

  it("base modules loading: no module count yet; failed: a note", async () => {
    let root = mount({ pending: ["/api/modules/base"] });
    await vi.waitFor(() => {
      expect(text(main(root))).toContain("Loading module changes…");
    });
    expect(counts(root).some((c) => c.includes("module"))).toBe(false);
    unmountApp();
    root = mount({ baseModules: [500, "no"] });
    await vi.waitFor(() => {
      expect(counts(root)[1]).toBe(
        "2 modules changed: 1 added, 1 modified; settings changed in 1 module.",
      );
    });
    expect(text(main(root))).toContain("Couldn't load base's modules");
    unmountApp();
    root = mount({ headModules: [500, "no"] });
    await vi.waitFor(() => {
      expect(text(main(root))).toContain(
        "Couldn't load the modules: they aren't counted.",
      );
    });
    expect(text(main(root))).not.toContain("Loading module changes");
  });

  it("revisions: merge base, (1 of N), working tree", () => {
    const root = mount({
      session: {
        mode: "diff",
        spec: "main...feature",
        base: { label: "main", commit: "c".repeat(40) },
        head: { label: "working tree", commit: null },
        mergeBases: 2,
      },
    });
    expect(text(main(root).querySelector(".home-revisions"))).toBe(
      "Diff main...feature. Base: merge base of main and working tree (1 of 2, ccccccc). Head: working tree.",
    );
  });

  it("browse mode: Home as before", async () => {
    const root = mount({ browse: true });
    await vi.waitFor(() => {
      expect(root.querySelector(".module-tree")).not.toBeNull();
    });
    expect(main(root).querySelector("h2")).toBeNull();
    expect(root.querySelector(".outside-count")).toBeNull();
  });
});

describe("header: changed files outside ROOT", () => {
  it("a link to Home, its name, a tip with the first 10 paths", async () => {
    const root = mount();
    let a: HTMLAnchorElement | null = null;
    await vi.waitFor(() => {
      a = root.querySelector<HTMLAnchorElement>("header a.outside-count");
      expect(a).not.toBeNull();
    });
    const link = a as unknown as HTMLAnchorElement;
    expect(link.getAttribute("href")).toBe("/tok");
    expect(text(link)).toBe("12 changed files outside ROOT");
    expect(link.querySelector(".sr-only")?.textContent).toBe(" changed files");
    expect(link.getAttribute("data-tip")).toBe(
      [...outsidePaths.slice(0, 10), "and 2 more, listed on Home"].join("\n"),
    );
  });

  it("hidden when 0 or null, N+ when truncated", async () => {
    let root = mount({
      diff: [
        200,
        diffFiles({ outside: { total: 0, paths: [], truncated: false } }),
      ],
    });
    await vi.waitFor(() => {
      expect(counts(root).length).toBeGreaterThan(0);
    });
    expect(root.querySelector(".outside-count")).toBeNull();
    unmountApp();
    root = mount({ diff: [200, diffFiles({ outside: null })] });
    await vi.waitFor(() => {
      expect(counts(root).length).toBeGreaterThan(0);
    });
    expect(root.querySelector(".outside-count")).toBeNull();
    unmountApp();
    root = mount({
      diff: [
        200,
        diffFiles({ outside: { total: 100, paths: ["a"], truncated: true } }),
      ],
    });
    await vi.waitFor(() => {
      expect(text(root.querySelector(".outside-count"))).toBe(
        "100+ changed files outside ROOT",
      );
    });
  });
});
