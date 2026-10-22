import { afterEach, describe, expect, it, vi } from "vitest";
import { fixture, stubFetch } from "../../test/fixtures";
import { mountAppAt, unmountApp } from "../../test/mount";

function mount(modules: [number, unknown]): HTMLElement {
  stubFetch({
    "/tok/api/files/tree": [200, fixture("files")],
    "/tok/api/modules/tree": modules,
  });
  const main = mountAppAt("/tok").querySelector("main");
  if (main === null) throw new Error("no main");
  return main;
}

function notices(main: HTMLElement): string[] {
  return [...main.querySelectorAll(".project-notice")].map(
    (p) => p.textContent,
  );
}

afterEach(() => {
  unmountApp();
  vi.unstubAllGlobals();
});

describe("ProjectNotices", () => {
  it("one per notice, before the page", async () => {
    const main = mount([200, fixture("modules-implicit")]);
    await vi.waitFor(() => {
      expect(notices(main)).toHaveLength(3);
    });
    expect(notices(main)[0]).toMatch(/^stack\.yaml isn't supported/);
    expect(notices(main)[1]).toMatch(/^No cabal\.project or \.cabal file/);
    expect(notices(main)[2]).toContain("without a .cabal file in ROOT.");
    expect(main.firstElementChild?.className).toBe("project-notice");
    expect(main.textContent).toContain("Pick a file in the sidebar.");
    // No warnings, no details.
    expect(main.querySelector("details")).toBeNull();
  });

  it("warnings in a details element", async () => {
    const main = mount([200, fixture("modules")]);
    await vi.waitFor(() => {
      expect(main.querySelector("details")).not.toBeNull();
    });
    expect(notices(main)).toEqual([
      "Hpack isn't supported: package.yaml without a .cabal file in hp. Run hpack and commit the .cabal file; until then it's a plain directory.",
    ]);
    const details = main.querySelector("details");
    expect(details?.open).toBe(false);
    expect(details?.querySelector("summary")?.textContent).toBe(
      "1 project warning",
    );
    expect(
      [...(details?.querySelectorAll("li") ?? [])].map((li) => li.textContent),
    ).toEqual(["cabal.project:3: no package matches foo/"]);
  });

  it("several warnings", async () => {
    const tree = {
      ...(fixture("modules") as Record<string, unknown>),
      warnings: ["a", "b"],
    };
    const main = mount([200, tree]);
    await vi.waitFor(() => {
      expect(main.querySelector("summary")?.textContent).toBe(
        "2 project warnings",
      );
    });
  });

  it.each([
    [500, "cannot list files"],
    [404, "not found"],
    [200, { notices: 1 }],
  ])("nothing on error %i", async (status, body) => {
    const fetch = mount([status, body]);
    await new Promise((r) => setTimeout(r, 20));
    expect(fetch.querySelector(".project-notice, details")).toBeNull();
    expect(fetch.textContent).toBe("Pick a file in the sidebar.");
  });
});
