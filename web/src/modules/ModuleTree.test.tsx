import { createSignal } from "solid-js";
import { render } from "solid-js/web";
import { afterEach, describe, expect, it, vi } from "vitest";
import { fixture, stubFetch } from "../../test/fixtures";
import { mountAppAt, unmountApp } from "../../test/mount";
import { decodeModuleTree, type ModuleTree as Tree } from "../api/modules";
import { type Current, ModuleTree } from "./ModuleTree";
import { indexRefs, indexSources } from "./store";

// Mounts the app with a module tree, and waits for the tree.
async function panel(name: string, at = "/tok"): Promise<HTMLElement> {
  stubFetch({
    "/tok/api/files/tree": [200, fixture("files")],
    "/tok/api/modules/tree": [200, fixture(name)],
  });
  const p = mountAppAt(at).querySelector<HTMLElement>("nav .modules-panel");
  if (p === null) throw new Error("no modules panel");
  await vi.waitFor(() => {
    expect(p.querySelector(".module-tree")).not.toBeNull();
  });
  return p;
}

function button(p: HTMLElement, name: string): HTMLButtonElement {
  const b = [...p.querySelectorAll("button")].find(
    (el) => el.textContent === name,
  );
  if (b === undefined) throw new Error(`no button ${name}`);
  return b;
}

// Direct children of a group's list, as text.
function items(b: HTMLElement): string[] {
  const ul = b.parentElement?.querySelector(":scope > ul");
  return [...(ul?.children ?? [])].map((li) => li.textContent);
}

function link(p: HTMLElement, text: string): HTMLAnchorElement | undefined {
  return [...p.querySelectorAll("a")].find((a) => a.textContent === text);
}

let dispose: (() => void) | undefined;
let setCurrent: (c: Current | string | undefined) => void = () => undefined;

const asCurrent = (c: Current | string | undefined): Current | undefined =>
  typeof c === "string" ? { kind: "path", path: c } : c;

// Renders the tree alone, with a current file (a path) or module we can
// change.
function mountTree(tree: Tree, start?: Current | string): HTMLElement {
  const root = document.createElement("div");
  document.body.append(root);
  const [current, set] = createSignal(asCurrent(start));
  setCurrent = (c) => set(asCurrent(c));
  const bySource = indexSources(tree);
  const byRef = indexRefs(tree);
  dispose = render(
    () => (
      <ModuleTree
        tree={tree}
        bySource={bySource}
        byRef={byRef}
        current={current()}
        docsHref={(r) => `/tok/docs/tree/${r}`}
        srcHref={(p) => `/tok/src/tree/${p}`}
      />
    ),
    root,
  );
  return root;
}

afterEach(() => {
  dispose?.();
  dispose = undefined;
  unmountApp();
  vi.unstubAllGlobals();
  vi.restoreAllMocks();
});

describe("ModuleTree", () => {
  it("top level: packages, broken, other files, all closed", async () => {
    const p = await panel("modules");
    const top = [...p.querySelectorAll(".module-tree > li")];
    expect(top.map((li) => li.firstElementChild?.textContent)).toEqual([
      "pkg 0.1.0",
      "bad/bad.cabal broken",
      "Other files",
    ]);
    for (const b of p.querySelectorAll("button")) {
      expect(b.getAttribute("aria-expanded")).toBe("false");
    }
    // Only the broken package's link: modules render when opened.
    expect(p.querySelectorAll("a")).toHaveLength(1);
  });

  it("broken package: links the .cabal file, with the problem", async () => {
    const p = await panel("modules");
    const a = link(p, "bad/bad.cabal broken");
    expect(a?.getAttribute("href")).toBe("/tok/src/tree/bad/bad.cabal");
    expect(a?.parentElement?.querySelector(".tree-problem")?.textContent).toBe(
      "can't parse: bad/bad.cabal:3:1: unexpected end of input",
    );
  });

  it("components, not buildable", async () => {
    const p = await panel("modules");
    const pkg = button(p, "pkg 0.1.0");
    pkg.click();
    expect(pkg.getAttribute("aria-expanded")).toBe("true");
    expect(items(pkg)).toEqual([
      "lib",
      "exe:app",
      "exe:outside",
      "test:spec",
      "test:detailed",
      "bench:slow not buildable",
    ]);
    expect(button(p, "bench:slow not buildable").textContent).toBe(
      "bench:slow not buildable",
    );
  });

  it("modules: sorted, markers, docs links, no source as text", async () => {
    const p = await panel("modules");
    button(p, "pkg 0.1.0").click();
    const lib = button(p, "lib");
    lib.click();
    expect(items(lib)).toEqual([
      "Foo",
      "Gen listed, no source (autogen)",
      "Gone listed, no source (file missing)",
      "Lexer generated (alex)",
      "Linked listed, no source (symlink)",
      "Lit",
      "Paths_pkg listed, no source (autogen)",
      "Sig signature",
    ]);
    expect(link(p, "Foo")?.getAttribute("href")).toBe("/tok/docs/tree/Foo");
    // Generated and preprocessed: docs too, the page says why.
    expect(link(p, "Lexer generated (alex)")?.getAttribute("href")).toBe(
      "/tok/docs/tree/Lexer",
    );
    expect(link(p, "Sig signature")?.getAttribute("href")).toBe(
      "/tok/docs/tree/Sig",
    );
    // No source: plain text, no link.
    const entries = [...p.querySelectorAll(".tree-entry")];
    expect(entries.map((e) => e.textContent)).toContain(
      "Paths_pkg listed, no source (autogen)",
    );
    for (const e of entries) expect(e.closest("a")).toBeNull();

    button(p, "exe:outside").click();
    expect(p.textContent).toContain("Main listed, no source (outside ROOT)");
    button(p, "test:spec").click();
    expect(
      link(p, "Main preprocessed (hspec-discover)")?.getAttribute("href"),
    ).toBe("/tok/docs/tree/pkg%3Atest%3Aspec%3AMain");
    button(p, "exe:app").click();
    expect(link(p, "Main")?.getAttribute("href")).toBe(
      "/tok/docs/tree/pkg%3Aexe%3Aapp%3AMain",
    );
  });

  it("other files: paths, boot marker, boot keeps the file link", async () => {
    const p = await panel("modules");
    const other = button(p, "Other files");
    other.click();
    expect(items(other)).toEqual([
      "pkg/src/Boot.hs-boot boot",
      "pkg/src/Lexer.hs",
      "scripts/Tool.hs",
      "scripts/build-docs.hs",
    ]);
    expect(link(p, "scripts/Tool.hs")?.getAttribute("href")).toBe(
      "/tok/docs/tree/Tool",
    );
    expect(link(p, "scripts/build-docs.hs")?.getAttribute("href")).toBe(
      "/tok/docs/tree/scripts/build-docs.hs",
    );
    expect(link(p, "pkg/src/Boot.hs-boot boot")?.getAttribute("href")).toBe(
      "/tok/src/tree/pkg/src/Boot.hs-boot",
    );
  });

  it("implicit package", async () => {
    const p = await panel("modules-implicit");
    const b = button(p, "Implicit package");
    b.click();
    expect(items(b)).toEqual([
      "Main.hs",
      "src/Data/Deep/Mod.hs",
      "src/Data/Deep/Mod.hs-boot boot",
    ]);
  });

  it("deep link: its groups are open, the module is current", async () => {
    const p = await panel("modules", "/tok/src/tree/pkg/src/Foo.hs");
    await vi.waitFor(() => {
      expect(p.querySelector('[aria-current="page"]')?.textContent).toBe("Foo");
    });
    expect(button(p, "pkg 0.1.0").getAttribute("aria-expanded")).toBe("true");
    expect(button(p, "lib").getAttribute("aria-expanded")).toBe("true");
    expect(button(p, "exe:app").getAttribute("aria-expanded")).toBe("false");
    expect(button(p, "Other files").getAttribute("aria-expanded")).toBe(
      "false",
    );
  });

  it.each(["Foo", "pkg%3Alib%3AFoo", "pkg:lib:Foo"])(
    "docs deep link by ref or id (%s): groups open, module current",
    async (ref) => {
      const p = await panel("modules", `/tok/docs/tree/${ref}`);
      await vi.waitFor(() => {
        expect(p.querySelector('[aria-current="page"]')?.textContent).toBe(
          "Foo",
        );
      });
      expect(button(p, "lib").getAttribute("aria-expanded")).toBe("true");
      expect(button(p, "exe:app").getAttribute("aria-expanded")).toBe("false");
    },
  );

  it("docs deep link to a loose module by path ref", async () => {
    const p = await panel("modules", "/tok/docs/tree/scripts/build-docs.hs");
    await vi.waitFor(() => {
      expect(p.querySelector('[aria-current="page"]')?.textContent).toBe(
        "scripts/build-docs.hs",
      );
    });
    expect(button(p, "Other files").getAttribute("aria-expanded")).toBe("true");
  });

  it("deep link to an other file opens Other files", async () => {
    const p = await panel("modules", "/tok/src/tree/scripts/Tool.hs");
    await vi.waitFor(() => {
      expect(p.querySelector('[aria-current="page"]')?.textContent).toBe(
        "scripts/Tool.hs",
      );
    });
    expect(button(p, "pkg 0.1.0").getAttribute("aria-expanded")).toBe("false");
  });

  it("names are text, links are encoded", async () => {
    const tree = {
      ...(fixture("modules-implicit") as Record<string, unknown>),
      other: [{ kind: "boot", path: "<b>x #.hs-boot" }],
    };
    stubFetch({
      "/tok/api/files/tree": [200, fixture("files")],
      "/tok/api/modules/tree": [200, tree],
    });
    const p = mountAppAt("/tok").querySelector<HTMLElement>(".modules-panel");
    if (p === null) throw new Error("no modules panel");
    await vi.waitFor(() => {
      expect(p.querySelector(".module-tree")).not.toBeNull();
    });
    button(p, "Implicit package").click();
    const a = p.querySelector(".module-tree a");
    expect(a?.textContent).toBe("<b>x #.hs-boot boot");
    expect(a?.querySelector("b")).toBeNull();
    expect(a?.getAttribute("href")).toBe(
      "/tok/src/tree/%3Cb%3Ex%20%23.hs-boot",
    );
  });
});

describe("ModuleTree alone", () => {
  const modules = decodeModuleTree(fixture("modules"));

  it("scrolls to the current module on a path change only", () => {
    const scroll = vi
      .spyOn(HTMLElement.prototype, "scrollIntoView")
      .mockImplementation(() => undefined);
    const root = mountTree(modules, "pkg/src/Foo.hs");
    expect(scroll).toHaveBeenCalledTimes(1);
    expect(scroll.mock.contexts[0]).toBe(link(root, "Foo"));
    // Close and reopen lib: no scroll.
    button(root, "lib").click();
    button(root, "lib").click();
    expect(scroll).toHaveBeenCalledTimes(1);
    // Another module in an open group: scroll.
    setCurrent("pkg/src/Sig.hsig");
    expect(scroll).toHaveBeenCalledTimes(2);
    // A module in a closed group: scroll once it is shown.
    setCurrent("pkg/app/Main.hs");
    expect(scroll).toHaveBeenCalledTimes(3);
    expect(scroll.mock.contexts[2]).toBe(link(root, "Main"));
    // Not in the tree: nothing opens, no scroll.
    setCurrent("README.md");
    expect(scroll).toHaveBeenCalledTimes(3);
  });

  it("a source in two components: the first group opens, both are current", () => {
    const shared = { kind: "module", path: "src/Foo.hs" } as const;
    const entry = (comp: string) => ({
      name: "Foo",
      id: `p:${comp}:Foo`,
      ref: `p:${comp}:Foo`,
      role: "other" as const,
      source: shared,
    });
    const tree: Tree = {
      generation: 1,
      notices: [],
      packages: [
        {
          name: "p",
          version: "1",
          cabalFile: "p.cabal",
          components: [
            { name: "lib", buildable: true, modules: [entry("lib")] },
            { name: "test:t", buildable: true, modules: [entry("test:t")] },
          ],
        },
      ],
      broken: [],
      other: [],
      warnings: [],
    };
    const root = mountTree(tree, "src/Foo.hs");
    expect(button(root, "lib").getAttribute("aria-expanded")).toBe("true");
    expect(button(root, "test:t").getAttribute("aria-expanded")).toBe("false");
    button(root, "test:t").click();
    const current = [...root.querySelectorAll('[aria-current="page"]')];
    expect(current.map((a) => a.getAttribute("href"))).toEqual([
      "/tok/docs/tree/p:lib:Foo",
      "/tok/docs/tree/p:test:t:Foo",
    ]);
    // On a docs page: only that module.
    setCurrent({ kind: "ref", ref: "p:test:t:Foo" });
    const docs = [...root.querySelectorAll('[aria-current="page"]')];
    expect(docs.map((a) => a.getAttribute("href"))).toEqual([
      "/tok/docs/tree/p:test:t:Foo",
    ]);
  });

  it("current by ref scrolls to the module, by id too", () => {
    const scroll = vi
      .spyOn(HTMLElement.prototype, "scrollIntoView")
      .mockImplementation(() => undefined);
    const root = mountTree(modules, { kind: "ref", ref: "pkg:lib:Sig" });
    expect(scroll).toHaveBeenCalledTimes(1);
    expect(scroll.mock.contexts[0]).toBe(link(root, "Sig signature"));
    expect(link(root, "Sig signature")?.getAttribute("aria-current")).toBe(
      "page",
    );
    setCurrent({ kind: "ref", ref: "Nope" });
    expect(scroll).toHaveBeenCalledTimes(1);
    expect(root.querySelector('[aria-current="page"]')).toBeNull();
  });

  it("component keys can't clash on ':'", () => {
    const comp = (name: string, path: string) => ({
      name,
      buildable: true,
      modules: [
        {
          name: "M",
          id: name,
          ref: name,
          role: "other" as const,
          source: { kind: "module" as const, path },
        },
      ],
    });
    const tree: Tree = {
      generation: 1,
      notices: [],
      packages: [
        {
          name: "a",
          version: "1",
          cabalFile: "a:b",
          components: [comp("c", "x.hs")],
        },
        {
          name: "b",
          version: "1",
          cabalFile: "a",
          components: [comp("b:c", "y.hs")],
        },
      ],
      broken: [],
      other: [],
      warnings: [],
    };
    const root = mountTree(tree);
    button(root, "a 1").click();
    button(root, "b 1").click();
    button(root, "c").click();
    expect(button(root, "b:c").getAttribute("aria-expanded")).toBe("false");
  });
});
