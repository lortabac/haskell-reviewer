import { render } from "solid-js/web";
import { afterEach, describe, expect, it, vi } from "vitest";
import { browseSession, type Session } from "../api/session";
import { SessionContext } from "../session";
import type { Side } from "../sides";
import { diffSession } from "../../test/sessions";
import { RevisionInfo } from "./RevisionInfo";

let dispose: (() => void) | undefined;

function mount(session: Session, current?: Side): HTMLElement {
  const root = document.createElement("div");
  document.body.append(root);
  dispose = render(
    () => (
      <SessionContext.Provider value={session}>
        <RevisionInfo current={current} />
      </SessionContext.Provider>
    ),
    root,
  );
  return root;
}

afterEach(() => {
  dispose?.();
  dispose = undefined;
  document.body.replaceChildren();
  vi.unstubAllGlobals();
});

// Each item's text, spaces as one.
const items = (root: HTMLElement) =>
  [...root.querySelectorAll(".rev-item")].map((e) =>
    e.textContent.replace(/\s+/g, " ").trim(),
  );

const copies = (root: HTMLElement) =>
  [...root.querySelectorAll("button.copy-button")].map((b) =>
    b.getAttribute("aria-label"),
  );

// The diff branch, so a spread keeps its type.
const d = diffSession as Extract<Session, { mode: "diff" }>;

const hex40 = "0123456789abcdef0123456789abcdef01234567";

describe("RevisionInfo", () => {
  it("browse mode: nothing", () => {
    const root = mount(browseSession, "tree");
    expect(root.childNodes).toHaveLength(0);
  });

  it("two dots: spec, both ends with short hashes", () => {
    const root = mount(diffSession);
    expect(items(root)).toEqual([
      "Diff main..feature",
      "base: main aaaaaaa",
      "head: feature bbbbbbb",
    ]);
    expect(copies(root)).toEqual([
      "Copy base commit hash",
      "Copy head commit hash",
    ]);
  });

  it("the current side, when given", () => {
    const root = mount(diffSession, "base");
    expect(root.querySelector(".rev-current")?.textContent).toBe(
      "Viewing: base",
    );
  });

  it("three dots, one merge base: no count; the tip is a Tab stop", () => {
    const root = mount({ ...d, spec: "main...feature", mergeBases: 1 });
    expect(items(root)[1]).toBe("base: merge base of main and feature aaaaaaa");
    const mb = root.querySelector(".rev-merge-base");
    expect(mb?.getAttribute("data-tip")).toBe("merge base of main and feature");
    expect(mb?.getAttribute("tabindex")).toBe("0");
  });

  it("three dots, two merge bases: (1 of 2)", () => {
    const root = mount({ ...d, mergeBases: 2 });
    expect(items(root)[1]).toBe(
      "base: merge base of main and feature aaaaaaa (1 of 2)",
    );
  });

  it("working tree: the label, no head hash, no head copy", () => {
    const root = mount({
      mode: "diff",
      spec: "HEAD~1",
      base: { label: "HEAD~1", commit: "c".repeat(40) },
      head: { label: "working tree", commit: null },
      mergeBases: null,
    });
    expect(items(root)).toEqual([
      "Diff HEAD~1",
      "base: HEAD~1 ccccccc",
      "head: working tree",
    ]);
    expect(copies(root)).toEqual(["Copy base commit hash"]);
  });

  it("copy writes the full hash", () => {
    const writeText = vi.fn(() => Promise.resolve());
    vi.stubGlobal("navigator", { clipboard: { writeText } });
    const root = mount({ ...d, head: { label: "feature", commit: hex40 } });
    root
      .querySelector<HTMLButtonElement>(
        'button[aria-label="Copy head commit hash"]',
      )
      ?.click();
    expect(writeText).toHaveBeenCalledWith(hex40);
  });

  it("a SHA-256 hash is cut to 7", () => {
    const root = mount({
      ...d,
      base: { label: "main", commit: "e".repeat(64) },
    });
    expect(root.querySelector(".rev-hash")?.textContent).toBe("eeeeeee");
  });

  it("a label gets its tip and a Tab stop only while cut", () => {
    const observers: (() => void)[] = [];
    vi.stubGlobal(
      "ResizeObserver",
      class {
        constructor(cb: () => void) {
          observers.push(cb);
        }
        observe(): void {}
        disconnect(): void {}
      },
    );
    const long = "feature/" + "x".repeat(80);
    const root = mount({ ...d, head: { label: long, commit: hex40 } });
    const cut = root.querySelectorAll<HTMLElement>(".rev-cut")[2];
    const inner = cut?.querySelector<HTMLElement>(".rev-cut-text");
    if (cut === undefined || inner === undefined || inner === null) {
      throw new Error("no label");
    }
    // No layout here: fake the widths, then report a resize.
    let scroll = 100;
    Object.defineProperty(inner, "scrollWidth", { get: () => scroll });
    Object.defineProperty(inner, "clientWidth", { value: 100 });
    const resize = () => {
      for (const cb of observers) cb();
    };
    resize();
    expect(cut.hasAttribute("data-tip")).toBe(false);
    expect(cut.hasAttribute("tabindex")).toBe(false);
    scroll = 500;
    resize();
    expect(cut.getAttribute("data-tip")).toBe(long);
    expect(cut.getAttribute("tabindex")).toBe("0");
    scroll = 100;
    resize();
    expect(cut.hasAttribute("data-tip")).toBe(false);
    expect(cut.hasAttribute("tabindex")).toBe(false);
    // Focused when it stops being cut: kept until focus leaves.
    scroll = 500;
    resize();
    cut.focus();
    expect(document.activeElement).toBe(cut);
    scroll = 100;
    resize();
    expect(cut.getAttribute("tabindex")).toBe("0");
    expect(document.activeElement).toBe(cut);
    cut.blur();
    expect(cut.hasAttribute("tabindex")).toBe(false);
    expect(cut.hasAttribute("data-tip")).toBe(false);
  });
});
