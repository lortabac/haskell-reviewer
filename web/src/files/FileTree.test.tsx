import { createSignal } from "solid-js";
import { render } from "solid-js/web";
import { afterEach, describe, expect, it, vi } from "vitest";
import { fixture } from "../../test/fixtures";
import { decodeFileList } from "../api/files";
import { FileTree } from "./FileTree";
import { buildTree } from "./tree";

let dispose: (() => void) | undefined;

const tree = buildTree(decodeFileList(fixture("files")).entries);

let setCurrent: (path: string | undefined) => void = () => undefined;

function mount(start?: string): HTMLElement {
  const root = document.createElement("div");
  document.body.append(root);
  const [current, set] = createSignal(start);
  setCurrent = set;
  dispose = render(
    () => (
      <FileTree
        root={tree}
        current={current()}
        href={(p) => `/tok/src/tree/${p}`}
      />
    ),
    root,
  );
  return root;
}

function folder(root: HTMLElement, name: string): HTMLButtonElement {
  const b = [...root.querySelectorAll("button")].find(
    (x) => x.textContent === name,
  );
  if (b === undefined) throw new Error(`no folder ${name}`);
  return b;
}

function links(root: HTMLElement): string[] {
  return [...root.querySelectorAll("a")].map((a) => a.textContent);
}

afterEach(() => {
  vi.restoreAllMocks();
  dispose?.();
  document.body.replaceChildren();
});

describe("FileTree", () => {
  it("folders are buttons, collapsed, children not rendered", () => {
    const root = mount();
    const buttons = [...root.querySelectorAll("button")];
    expect(buttons.map((b) => b.textContent)).toEqual([
      "app",
      "assets",
      "data",
      "docs",
      "shared",
      "vendor",
    ]);
    for (const b of buttons) {
      expect(b.getAttribute("type")).toBe("button");
      expect(b.getAttribute("aria-expanded")).toBe("false");
    }
    expect(root.querySelectorAll("a")).toHaveLength(0);
    expect(root.querySelector("[role=tree]")).toBeNull();
  });

  it("a click opens and closes a folder", () => {
    const root = mount();
    const docs = folder(root, "docs");
    docs.click();
    expect(docs.getAttribute("aria-expanded")).toBe("true");
    expect(links(root)).toEqual(["guide.md link", "old.md link"]);
    const a = root.querySelector("a");
    expect(a?.getAttribute("href")).toBe("/tok/src/tree/docs/guide.md");
    docs.click();
    expect(docs.getAttribute("aria-expanded")).toBe("false");
    expect(links(root)).toEqual([]);
  });

  it("marks submodules", () => {
    const root = mount();
    folder(root, "vendor").click();
    expect(links(root)).toEqual(["dep submodule", "nested submodule"]);
  });

  it("opens the folders of the current file and marks it", () => {
    const root = mount("app/Main.hs");
    expect(folder(root, "app").getAttribute("aria-expanded")).toBe("true");
    expect(folder(root, "docs").getAttribute("aria-expanded")).toBe("false");
    const marked = () =>
      [...root.querySelectorAll('[aria-current="page"]')].map(
        (a) => a.textContent,
      );
    expect(marked()).toEqual(["Main.hs"]);

    setCurrent("shared/guide.md");
    expect(folder(root, "shared").getAttribute("aria-expanded")).toBe("true");
    expect(marked()).toEqual(["guide.md"]);

    // The user can close an open folder again.
    folder(root, "shared").click();
    expect(folder(root, "shared").getAttribute("aria-expanded")).toBe("false");
  });

  it("scrolls to the current file on a path change only", () => {
    const scroll = vi
      .spyOn(HTMLElement.prototype, "scrollIntoView")
      .mockImplementation(() => undefined);
    const root = mount("app/Main.hs");
    expect(scroll).toHaveBeenCalledTimes(1);
    expect(scroll.mock.contexts[0]).toBe(root.querySelector("a"));
    // Close and reopen the folder: no scroll.
    folder(root, "app").click();
    folder(root, "app").click();
    expect(scroll).toHaveBeenCalledTimes(1);
    // Another file in an open folder: scroll.
    folder(root, "docs").click();
    setCurrent("docs/old.md");
    expect(scroll).toHaveBeenCalledTimes(2);
    // A file in a closed folder: scroll once it is shown.
    setCurrent("shared/guide.md");
    expect(scroll).toHaveBeenCalledTimes(3);
  });
});
