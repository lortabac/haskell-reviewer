import { afterEach, describe, expect, it, vi } from "vitest";
import { fixture, stubFetch } from "../../test/fixtures";
import { mountAppAt, unmountApp } from "../../test/mount";

afterEach(() => {
  unmountApp();
  vi.unstubAllGlobals();
});

async function notFound(path: string, modules = true): Promise<HTMLElement> {
  stubFetch({
    "/tok/api/files/tree": [200, fixture("files")],
    "/tok/api/modules/tree": modules ? [200, fixture("modules")] : [500, "no"],
  });
  const main = mountAppAt(path).querySelector("main");
  if (main === null) throw new Error("no main");
  // The box, and the module tree loaded (or failed).
  await vi.waitFor(() => {
    expect(main.querySelector('[role="search"] input')).not.toBeNull();
    expect(document.querySelector("nav .modules-panel")?.textContent).not.toBe(
      "Loading modules…",
    );
  });
  return main;
}

function input(main: HTMLElement): HTMLInputElement {
  const i = main.querySelector<HTMLInputElement>('[role="search"] input');
  if (i === null) throw new Error("no input");
  return i;
}

function type(main: HTMLElement, text: string): void {
  const i = input(main);
  i.value = text;
  i.dispatchEvent(new Event("input", { bubbles: true }));
}

const results = (main: HTMLElement) =>
  [...main.querySelectorAll(".search-results li")].map((li) => [
    li.textContent,
    li.querySelector("a")?.getAttribute("href"),
  ]);

describe("NotFound search box", () => {
  it("a labelled search input, not in a form, prefilled from the URL", async () => {
    const main = await notFound("/tok/docs/tree/No.Such");
    const i = input(main);
    expect(i.type).toBe("search");
    expect(i.value).toBe("No.Such");
    expect(i.closest("form")).toBeNull();
    const label = main.querySelector(`label[for="${i.id}"]`);
    expect(label?.textContent).toBe("Search modules by name");
    expect(main.querySelector(".search-count")?.textContent).toBe(
      "No modules match.",
    );
    expect(main.querySelector(".search-count")?.getAttribute("role")).toBe(
      "status",
    );
  });

  it("filters module names, case-insensitive, links to docs", async () => {
    const main = await notFound("/tok/nope");
    expect(input(main).value).toBe("");
    expect(main.querySelector(".search-count")?.textContent).toBe("");
    type(main, "MAIN");
    expect(results(main)).toEqual([
      ["Main (pkg:exe:app:Main)", "/tok/docs/tree/pkg%3Aexe%3Aapp%3AMain"],
      ["Main (pkg:test:spec:Main)", "/tok/docs/tree/pkg%3Atest%3Aspec%3AMain"],
      [
        "Main (pkg:bench:slow:Main)",
        "/tok/docs/tree/pkg%3Abench%3Aslow%3AMain",
      ],
    ]);
    expect(main.querySelector(".search-count")?.textContent).toBe(
      "3 modules match.",
    );
    type(main, "foo");
    expect(results(main)).toEqual([["Foo", "/tok/docs/tree/Foo"]]);
    expect(main.querySelector(".search-count")?.textContent).toBe(
      "1 module matches.",
    );
    // No source: not listed.
    type(main, "Gone");
    expect(results(main)).toEqual([]);
    // Loose files: by name or path.
    type(main, "build-docs");
    expect(results(main)).toEqual([
      ["scripts/build-docs.hs", "/tok/docs/tree/scripts/build-docs.hs"],
    ]);
  });

  it("shows the first 50, and says so", async () => {
    const many = {
      ...(fixture("modules-implicit") as Record<string, unknown>),
      other: Array.from({ length: 60 }, (_, i) => ({
        kind: "module",
        path: `M${i}.hs`,
        name: `M${i}`,
        id: `M${i}.hs`,
        ref: `M${i}`,
      })),
    };
    stubFetch({
      "/tok/api/files/tree": [200, fixture("files")],
      "/tok/api/modules/tree": [200, many],
    });
    const main = mountAppAt("/tok/docs/tree/M").querySelector("main");
    if (main === null) throw new Error("no main");
    await vi.waitFor(() => {
      expect(main.querySelectorAll(".search-results li")).toHaveLength(50);
    });
    expect(main.querySelector(".search-count")?.textContent).toBe(
      "60 modules match. Showing the first 50.",
    );
  });

  it("a new bad URL gives a new box with its text", async () => {
    const main = await notFound("/tok/docs/tree/One");
    expect(input(main).value).toBe("One");
    type(main, "typed");
    const a = document.createElement("a");
    a.href = "/tok/docs/tree/Two";
    main.append(a);
    // Two is unknown too: the module API answers 404.
    a.click();
    await vi.waitFor(() => {
      expect(input(main).value).toBe("Two");
    });
    const b = document.createElement("a");
    b.href = "/tok/nope/Three";
    main.append(b);
    b.click();
    await vi.waitFor(() => {
      expect(input(main).value).toBe("");
    });
  });

  it("module tree failed: no matches, no crash", async () => {
    const main = await notFound("/tok/docs/tree/Foo", false);
    expect(input(main).value).toBe("Foo");
    expect(results(main)).toEqual([]);
  });
});
