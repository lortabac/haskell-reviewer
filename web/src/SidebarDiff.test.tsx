// The sidebar in diff mode (D-337): markers, ghosts, "Changed only".
import { afterEach, describe, expect, it, vi } from "vitest";
import { stubFetch } from "../test/fixtures";
import { mountAppAt, unmountApp } from "../test/mount";
import { diffSession } from "../test/sessions";
import { defaultPrefs, type Prefs } from "./api/prefs";

const entry = (name: string, change: string | null) => ({
  name,
  id: `p:lib:${name}`,
  ref: name,
  role: "exposed",
  source: { kind: "module", path: `src/${name}.hs` },
  change,
  settingsChanged: false,
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

const files = (paths: string[]) => ({
  generation: 1,
  source: "git",
  entries: paths.map((path) => ({ path, kind: "file" })),
  skippedNames: 0,
  truncated: false,
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

const diffFiles = {
  generation: 1,
  base: { label: "main", commit: "a".repeat(40) },
  head: { label: "feature", commit: "b".repeat(40) },
  files: [
    change("docs/new.md", "renamed", "docs/old.md"),
    change("src/A.hs", "modified"),
    change("src/B.hs", "removed"),
    change("src/C.hs", "added"),
  ],
  outside: null,
  renameLimitHit: false,
  truncated: false,
};

function mount(
  opts: {
    prefs?: Partial<Prefs>;
    diff?: [number, unknown];
    at?: string;
    browse?: boolean;
    // Both sides' modules, if not the usual ones.
    sameModules?: unknown[];
    // These routes never answer.
    pending?: string[];
  } = {},
) {
  const fetch = stubFetch({
    "/tok/api/modules/head": [
      200,
      modules(
        opts.sameModules ?? [
          entry("A", "modified"),
          entry("C", "added"),
          entry("D", null),
        ],
      ),
    ],
    "/tok/api/modules/base": [
      200,
      modules(
        opts.sameModules ?? [
          entry("A", "modified"),
          entry("B", "removed"),
          entry("D", null),
        ],
      ),
    ],
    "/tok/api/modules/tree": [200, modules([entry("A", null)])],
    "/tok/api/files/head": [
      200,
      files([
        "README.md",
        "docs/new.md",
        "lib/U.hs",
        "src/A.hs",
        "src/C.hs",
        "src/D.hs",
      ]),
    ],
    "/tok/api/files/base": [
      200,
      files(["README.md", "docs/old.md", "src/A.hs", "src/B.hs", "src/D.hs"]),
    ],
    "/tok/api/files/tree": [200, files(["README.md"])],
    "/tok/api/diff-files": opts.diff ?? [200, diffFiles],
    "/tok/api/prefs": [200, { ...defaultPrefs, changedOnly: true }],
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
  const root = mountAppAt(
    opts.at ?? "/tok",
    { ...defaultPrefs, ...opts.prefs },
    undefined,
    null,
    opts.browse === true ? undefined : diffSession,
  );
  const nav = root.querySelector<HTMLElement>("nav.frame-sidebar");
  if (nav === null) throw new Error("no sidebar");
  return { nav, fetch };
}

const panel = (nav: HTMLElement, which: "modules" | "files") => {
  const p = nav.querySelector<HTMLElement>(`.${which}-panel`);
  if (p === null) throw new Error(`no ${which} panel`);
  return p;
};

function button(root: HTMLElement, text: string): HTMLButtonElement {
  const b = [...root.querySelectorAll("button")].find((x) =>
    x.textContent.startsWith(text),
  );
  if (b === undefined) throw new Error(`no button ${text}`);
  return b;
}

function link(root: HTMLElement, start: string): HTMLAnchorElement {
  const a = [...root.querySelectorAll("a")].find((x) =>
    x.textContent.startsWith(start),
  );
  if (a === undefined) throw new Error(`no link ${start}`);
  return a;
}

const linkTexts = (root: HTMLElement): string[] =>
  [...root.querySelectorAll("a")].map((a) => a.textContent);

afterEach(() => {
  unmountApp();
  vi.unstubAllGlobals();
  vi.restoreAllMocks();
});

describe("Sidebar in diff mode", () => {
  it("a Changed only box and a legend; none in browse mode", async () => {
    const { nav } = mount();
    const box = nav.querySelector<HTMLInputElement>(
      ".changed-only input[type=checkbox]",
    );
    expect(box?.checked).toBe(false);
    expect(nav.querySelector(".changed-only")?.textContent?.trim()).toBe(
      "Changed only",
    );
    expect(nav.querySelector(".change-legend")?.textContent).toContain("added");
    unmountApp();
    const b = mount({ browse: true });
    await vi.waitFor(() => {
      expect(b.nav.querySelector(".module-tree")).not.toBeNull();
    });
    expect(b.nav.querySelector(".changed-only")).toBeNull();
    expect(b.nav.querySelector(".change-legend")).toBeNull();
  });

  it("modules: signs with a word, a ghost linking to base", async () => {
    const { nav } = mount();
    const p = panel(nav, "modules");
    await vi.waitFor(() => {
      expect(p.querySelector(".module-tree")).not.toBeNull();
    });
    button(p, "p 1").click();
    button(p, "lib").click();
    await vi.waitFor(() => {
      expect(linkTexts(p).some((t) => t.startsWith("B"))).toBe(true);
    });
    const c = link(p, "C");
    const sign = c.querySelector(".change-sign");
    expect(sign?.querySelector("[aria-hidden=true]")?.textContent).toBe("+");
    expect(sign?.querySelector(".sr-only")?.textContent).toBe("added");
    expect(link(p, "D").querySelector(".change-sign")).toBeNull();
    const b = link(p, "B");
    expect(b.getAttribute("href")).toBe("/tok/docs/base/B");
    expect(b.textContent).toContain("(on base)");
    expect(b.querySelector(".sr-only")?.textContent).toBe("removed");
    expect(b.classList.contains("tree-ghost")).toBe(true);
    // Modules in name order, the ghost with them.
    expect(linkTexts(p).map((t) => t[0])).toEqual(["A", "B", "C", "D"]);
  });

  it("a ghost is never current, even at a clashing ref", async () => {
    const { nav } = mount({ at: "/tok/docs/head/B" });
    const p = panel(nav, "modules");
    await vi.waitFor(() => {
      expect(p.querySelector(".module-tree")).not.toBeNull();
    });
    button(p, "p 1").click();
    button(p, "lib").click();
    await vi.waitFor(() => {
      expect(linkTexts(p).some((t) => t.startsWith("B"))).toBe(true);
    });
    expect(link(p, "B").getAttribute("aria-current")).toBeNull();
  });

  it("files: a ghost, signs, rename text in a tip and for screen readers", async () => {
    const { nav } = mount({ prefs: { sidebar: "files" } });
    const p = panel(nav, "files");
    await vi.waitFor(() => {
      expect(p.querySelector(".file-tree")).not.toBeNull();
    });
    await vi.waitFor(() => {
      expect(p.textContent).not.toContain("Loading changes");
    });
    button(p, "src").click();
    button(p, "docs").click();
    const b = link(p, "B.hs");
    expect(b.getAttribute("href")).toBe("/tok/src/base/src/B.hs");
    expect(b.textContent).toContain("(on base)");
    expect(b.getAttribute("aria-current")).toBeNull();
    expect(link(p, "C.hs").querySelector(".sr-only")?.textContent).toBe(
      "added",
    );
    expect(link(p, "A.hs").querySelector(".sr-only")?.textContent).toBe(
      "modified",
    );
    const r = link(p, "new.md");
    expect(r.getAttribute("data-tip")).toBe("renamed from docs/old.md (90%)");
    expect(r.querySelector(".sr-only")?.textContent).toBe(
      "renamed from docs/old.md (90%)",
    );
    expect(link(p, "README.md").querySelector(".change-sign")).toBeNull();
  });

  it("base: renamed to, and the added file is a ghost linking to head", async () => {
    const { nav } = mount({
      prefs: { sidebar: "files" },
      at: "/tok/src/base/README.md",
    });
    const p = panel(nav, "files");
    await vi.waitFor(() => {
      expect(p.querySelector(".file-tree")).not.toBeNull();
      expect(p.textContent).not.toContain("Loading changes");
    });
    button(p, "src").click();
    button(p, "docs").click();
    expect(link(p, "C.hs").getAttribute("href")).toBe("/tok/src/head/src/C.hs");
    expect(link(p, "C.hs").textContent).toContain("(on head)");
    expect(link(p, "old.md").getAttribute("data-tip")).toBe(
      "renamed to docs/new.md (90%)",
    );
    expect(link(p, "B.hs").querySelector(".sr-only")?.textContent).toBe(
      "removed",
    );
  });

  it("changed only: unchanged files go, folders open, the pick is saved", async () => {
    const { nav, fetch } = mount({ prefs: { sidebar: "files" } });
    const p = panel(nav, "files");
    await vi.waitFor(() => {
      expect(linkTexts(p)).toContain("README.md");
    });
    const box = nav.querySelector<HTMLInputElement>(".changed-only input");
    box?.click();
    await vi.waitFor(() => {
      expect(linkTexts(p)).not.toContain("README.md");
    });
    // Open by default: few files left.
    expect(link(p, "A.hs")).toBeDefined();
    expect(linkTexts(p).some((t) => t.startsWith("D.hs"))).toBe(false);
    const calls = fetch.mock.calls as unknown as [string, RequestInit?][];
    const post = calls.find((c) => c[1]?.method === "POST");
    expect(JSON.parse(post?.[1]?.body as string)).toEqual({
      changedOnly: true,
    });
    // A folder the user closes stays closed.
    button(p, "src").click();
    expect(button(p, "src").getAttribute("aria-expanded")).toBe("false");
  });

  it("changed only keeps the current file", async () => {
    const { nav } = mount({
      prefs: { sidebar: "files", changedOnly: true },
      at: "/tok/src/head/src/D.hs",
    });
    const p = panel(nav, "files");
    await vi.waitFor(() => {
      expect(linkTexts(p).some((t) => t.startsWith("A.hs"))).toBe(true);
    });
    expect(link(p, "D.hs").getAttribute("aria-current")).toBe("page");
    expect(linkTexts(p)).not.toContain("README.md");
  });

  it("changed only, modules: unchanged ones go", async () => {
    const { nav } = mount({ prefs: { changedOnly: true } });
    const p = panel(nav, "modules");
    await vi.waitFor(() => {
      expect(linkTexts(p).some((t) => t.startsWith("B"))).toBe(true);
    });
    expect(linkTexts(p).map((t) => t[0])).toEqual(["A", "B", "C"]);
  });

  it("changed only, modules: nothing changed is a note", async () => {
    const { nav } = mount({
      prefs: { changedOnly: true },
      sameModules: [entry("A", null), entry("D", null)],
    });
    const p = panel(nav, "modules");
    await vi.waitFor(() => {
      expect(p.textContent).toContain("No changed modules.");
    });
  });

  it("changed only, while api/diff-files loads: only a note", async () => {
    const { nav } = mount({
      prefs: { sidebar: "files", changedOnly: true },
      pending: ["/api/diff-files"],
    });
    const p = panel(nav, "files");
    await vi.waitFor(() => {
      expect(p.textContent).toContain("Loading changes…");
    });
    expect(p.querySelector(".file-tree")).toBeNull();
    expect(p.textContent).not.toContain("No changed files");
  });

  it("api/diff-files fails: a note, and every file", async () => {
    const { nav } = mount({
      prefs: { sidebar: "files", changedOnly: true },
      diff: [500, "cannot pair files"],
    });
    const p = panel(nav, "files");
    await vi.waitFor(() => {
      expect(p.textContent).toContain(
        "Couldn't load the changes. See the terminal for details.",
      );
    });
    expect(linkTexts(p)).toContain("README.md");
    expect(p.querySelector(".change-sign")).toBeNull();
  });

  it("changed only, modules: a clicked link stays, with focus", async () => {
    const { nav } = mount({
      prefs: { changedOnly: true },
      at: "/tok/docs/head/D",
    });
    const p = panel(nav, "modules");
    await vi.waitFor(() => {
      expect(linkTexts(p).some((t) => t.startsWith("C"))).toBe(true);
    });
    // D is unchanged: there only as the current module.
    expect(link(p, "D").getAttribute("aria-current")).toBe("page");
    const c = link(p, "C");
    c.focus();
    c.click();
    await vi.waitFor(() => {
      expect(c.getAttribute("aria-current")).toBe("page");
    });
    expect(c.isConnected).toBe(true);
    expect(document.activeElement).toBe(c);
    expect(linkTexts(p).some((t) => t.startsWith("D"))).toBe(false);
  });

  it("changed only, files: from an unchanged current file, focus stays", async () => {
    // lib/ has no change: it shows only for the current file.
    const { nav } = mount({
      prefs: { sidebar: "files", changedOnly: true },
      at: "/tok/src/head/lib/U.hs",
    });
    const p = panel(nav, "files");
    await vi.waitFor(() => {
      expect(linkTexts(p).some((t) => t.startsWith("A.hs"))).toBe(true);
    });
    const folders = [...p.querySelectorAll("button.tree-folder")];
    const a = link(p, "A.hs");
    a.focus();
    a.click();
    await vi.waitFor(() => {
      expect(a.getAttribute("aria-current")).toBe("page");
    });
    expect(a.isConnected).toBe(true);
    expect(document.activeElement).toBe(a);
    // lib/U.hs was only there as the current file.
    expect(linkTexts(p).some((t) => t.startsWith("U.hs"))).toBe(false);
    expect(folders.find((b) => b.textContent === "lib")?.isConnected).toBe(
      false,
    );
    // The other folders are the same elements: nothing mounted again.
    expect(
      folders
        .filter((b) => b.textContent !== "lib")
        .every((b) => b.isConnected),
    ).toBe(true);
  });

  it("pairing failed: modules unfiltered, with the note", async () => {
    const { nav } = mount({
      prefs: { changedOnly: true },
      diff: [500, "cannot pair files"],
      sameModules: [entry("A", null), entry("D", null)],
    });
    const p = panel(nav, "modules");
    await vi.waitFor(() => {
      expect(p.textContent).toContain(
        "Couldn't load the changes. See the terminal for details.",
      );
    });
    expect(p.textContent).not.toContain("No changed modules.");
    button(p, "p 1").click();
    button(p, "lib").click();
    expect(linkTexts(p)).toEqual(["A", "D"]);
  });

  it("changed only, a cut list: a note under the legend", async () => {
    const { nav } = mount({
      prefs: { changedOnly: true },
      diff: [200, { ...diffFiles, truncated: true }],
    });
    await vi.waitFor(() => {
      expect(nav.textContent).toContain(
        "A side has over 100,000 files: changes past that point aren't shown.",
      );
    });
  });

  it("changed only, nothing changed: the note, the current row, kept groups", async () => {
    const { nav } = mount({
      prefs: { changedOnly: true },
      diff: [200, { ...diffFiles, files: [] }],
      sameModules: [entry("A", null), entry("D", null)],
      at: "/tok/docs/head/D",
    });
    const p = panel(nav, "modules");
    await vi.waitFor(() => {
      expect(p.textContent).toContain("No changed modules.");
      expect(link(p, "D").getAttribute("aria-current")).toBe("page");
    });
    expect(linkTexts(p)).toEqual(["D"]);
    // The user closes lib; toggling the box keeps it closed.
    const lib = button(p, "lib");
    lib.click();
    expect(lib.getAttribute("aria-expanded")).toBe("false");
    const box = nav.querySelector<HTMLInputElement>(".changed-only input");
    box?.click();
    await vi.waitFor(() => {
      expect(p.textContent).not.toContain("No changed modules.");
    });
    expect(lib.isConnected).toBe(true);
    expect(lib.getAttribute("aria-expanded")).toBe("false");
    box?.click();
    await vi.waitFor(() => {
      expect(p.textContent).toContain("No changed modules.");
    });
    expect(lib.isConnected).toBe(true);
    expect(lib.getAttribute("aria-expanded")).toBe("false");
  });

  it("changed only, files, nothing changed: the note and the current file", async () => {
    const { nav } = mount({
      prefs: { sidebar: "files", changedOnly: true },
      diff: [200, { ...diffFiles, files: [] }],
      at: "/tok/src/head/src/D.hs",
    });
    const p = panel(nav, "files");
    await vi.waitFor(() => {
      expect(p.textContent).toContain("No changed files under ROOT.");
      expect(link(p, "D.hs").getAttribute("aria-current")).toBe("page");
    });
    expect(linkTexts(p)).toEqual(["D.hs"]);
  });

  it("changed only, other side's modules loading: not 'No changed modules.'", async () => {
    const { nav } = mount({
      prefs: { changedOnly: true },
      sameModules: [entry("A", null), entry("D", null)],
      pending: ["/api/modules/base"],
    });
    const p = panel(nav, "modules");
    await vi.waitFor(() => {
      expect(p.textContent).toContain("Loading changes…");
    });
    expect(p.textContent).not.toContain("No changed modules.");
  });
});
