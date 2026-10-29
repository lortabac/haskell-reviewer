import { afterEach, describe, expect, it, vi } from "vitest";
import { cborFixture, fixture, stubFetch } from "../test/fixtures";
import { mountAppAt, unmountApp } from "../test/mount";

interface Parts {
  nav: HTMLElement;
  modules: HTMLButtonElement;
  files: HTMLButtonElement;
  modulesPanel: HTMLElement;
  filesPanel: HTMLElement;
}

function mount(
  modules: [number, unknown],
  at = "/tok",
  files: unknown = fixture("files"),
): Parts {
  stubFetch({
    "/tok/api/files/tree": [200, files],
    "/tok/api/modules/tree": modules,
    "/tok/api/file/tree/app/Main.hs": [200, cborFixture("file-text")],
  });
  const nav = mountAppAt(at).querySelector("nav");
  const group = nav?.querySelector('[role="group"]');
  const [m, f] = [...(group?.querySelectorAll("button") ?? [])];
  const modulesPanel = nav?.querySelector<HTMLElement>(".modules-panel");
  const filesPanel = nav?.querySelector<HTMLElement>(".files-panel");
  if (!nav || !m || !f || !modulesPanel || !filesPanel) {
    throw new Error("no sidebar");
  }
  return { nav, modules: m, files: f, modulesPanel, filesPanel };
}

// Which view shows, checked on the buttons and the panels.
function expectView(s: Parts, view: "modules" | "files"): void {
  const m = view === "modules";
  expect(s.modules.getAttribute("aria-pressed")).toBe(String(m));
  expect(s.files.getAttribute("aria-pressed")).toBe(String(!m));
  expect(s.modulesPanel.hidden).toBe(!m);
  expect(s.filesPanel.hidden).toBe(m);
}

afterEach(() => {
  unmountApp();
  vi.unstubAllGlobals();
  vi.restoreAllMocks();
});

describe("Sidebar", () => {
  it("the toggle: a labelled group of two buttons", () => {
    const s = mount([200, fixture("modules")]);
    const group = s.nav.querySelector('[role="group"]');
    expect(group?.getAttribute("aria-label")).toBe("Sidebar view");
    expect(s.modules.textContent).toBe("Modules");
    expect(s.files.textContent).toBe("Files");
    expect(s.modules.type).toBe("button");
    // Comes before the panels.
    expect(s.nav.querySelector("button")).toBe(s.modules);
  });

  it("Modules while loading and when there are modules", async () => {
    const s = mount([200, fixture("modules")]);
    expectView(s, "modules");
    expect(s.modulesPanel.textContent).toBe("Loading modules…");
    await vi.waitFor(() => {
      expect(s.modulesPanel.querySelector(".module-tree")).not.toBeNull();
    });
    expectView(s, "modules");
  });

  it.each([
    ["404", [404, "not found"]],
    ["500", [500, "cannot list files"]],
    ["bad payload", [200, { packages: 1 }]],
    [
      "an empty tree",
      [
        200,
        {
          ...(fixture("modules-implicit") as Record<string, unknown>),
          notices: [],
          other: [],
        },
      ],
    ],
  ] as [string, [number, unknown]][])("Files on %s", async (_, modules) => {
    const s = mount(modules);
    await vi.waitFor(() => {
      expectView(s, "files");
    });
  });

  it("the Modules view says why it is empty", async () => {
    const s = mount([500, "cannot list files"]);
    await vi.waitFor(() => {
      expectView(s, "files");
    });
    s.modules.click();
    expectView(s, "modules");
    expect(s.modulesPanel.textContent).toBe(
      "Couldn't load the module tree. See the terminal for details.",
    );
  });

  it("clicks switch, and the pick stays after a navigation", async () => {
    const s = mount([200, fixture("modules")]);
    await vi.waitFor(() => {
      expect(s.filesPanel.querySelector(".file-tree")).not.toBeNull();
    });
    s.files.click();
    expectView(s, "files");
    s.filesPanel.querySelector<HTMLElement>(".file-tree button")?.click();
    s.filesPanel.querySelector<HTMLElement>(".file-tree a")?.click();
    await vi.waitFor(() => {
      expect(window.location.pathname).toBe("/tok/src/tree/app/Main.hs");
    });
    expectView(s, "files");
    s.modules.click();
    expectView(s, "modules");
  });

  it("a pick made while loading stays", async () => {
    const s = mount([500, "cannot list files"]);
    s.modules.click();
    await new Promise((r) => setTimeout(r, 20));
    expectView(s, "modules");
  });

  it("showing a view scrolls to its current item", async () => {
    const scroll = vi
      .spyOn(HTMLElement.prototype, "scrollIntoView")
      .mockImplementation(() => undefined);
    const files = {
      ...(fixture("files") as Record<string, unknown>),
      entries: [{ path: "pkg/src/Foo.hs", kind: "file" }],
    };
    const s = mount(
      [200, fixture("modules")],
      "/tok/src/tree/pkg/src/Foo.hs",
      files,
    );
    const current = (panel: HTMLElement) =>
      panel.querySelector('[aria-current="page"]');
    await vi.waitFor(() => {
      expect(current(s.modulesPanel)).not.toBeNull();
      expect(current(s.filesPanel)).not.toBeNull();
    });
    scroll.mockClear();
    s.files.click();
    expect(scroll).toHaveBeenCalledTimes(1);
    expect(scroll.mock.contexts[0]).toBe(current(s.filesPanel));
    // Same view again: no scroll.
    s.files.click();
    expect(scroll).toHaveBeenCalledTimes(1);
    s.modules.click();
    expect(scroll).toHaveBeenCalledTimes(2);
    expect(scroll.mock.contexts[1]).toBe(current(s.modulesPanel));
  });
});
