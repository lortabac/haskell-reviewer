import { afterEach, beforeEach, describe, expect, it, vi } from "vitest";
import { cbor, fixture, stubFetch } from "../../test/fixtures";
import { fakeLayout } from "../../test/layout";
import { mountAppAt, unmountApp } from "../../test/mount";
import { noteTexts } from "../ui/notes";

// Mounts the app at /tok/md/tree/README.md, with the API answering the
// Markdown page with `page`.
async function open(page: Record<string, unknown>): Promise<HTMLElement> {
  const fetch = stubFetch({
    "/tok/api/files/tree": [200, fixture("files")],
    "/tok/api/modules/tree": [404, "not found"],
    "/tok/api/markdown/tree/README.md": [
      200,
      cbor({ generation: 1, filtered: false, ...page }),
    ],
  });
  const main = mountAppAt("/tok/md/tree/README.md").querySelector("main");
  if (main === null) throw new Error("no main");
  await vi.waitFor(() => {
    expect(fetch.mock.calls.map((c) => c[0] as string)).toContain(
      "/tok/api/markdown/tree/README.md",
    );
  });
  // Let the resource settle.
  await vi.waitFor(() => {
    expect(main.querySelector(".md-page, main a")).not.toBeNull();
  });
  return main;
}

beforeEach(() => fakeLayout());

afterEach(() => {
  unmountApp();
  vi.unstubAllGlobals();
  vi.restoreAllMocks();
  document.title = "";
});

// The "See the file" link.
const seeFile = (main: HTMLElement) =>
  [...main.querySelectorAll("a")].find((a) => a.textContent === "See the file");

const doc = {
  blocks: [
    {
      kind: "heading",
      level: 1,
      inlines: [{ kind: "text", text: "Title" }],
      anchor: "title",
    },
    {
      kind: "paragraph",
      inlines: [{ kind: "text", text: "Some words." }],
    },
  ],
  since: null,
};

describe("MarkdownView", () => {
  it("a doc", async () => {
    const main = await open({ kind: "doc", doc });
    expect(main.querySelector("h1.file-path")?.textContent).toBe("README.md");
    const page = main.querySelector(".md-page");
    expect(page?.querySelector("h1, h2")?.textContent).toContain("Title");
    expect(page?.textContent).toContain("Some words.");
    expect(main.querySelector(".file-note")).toBeNull();
  });

  it("not text", async () => {
    const main = await open({ kind: "not-text" });
    expect(main.textContent).toContain(
      "This file is not text, so it can't be shown rendered.",
    );
    expect(main.querySelector(".md-page")).toBeNull();
    expect(seeFile(main)?.getAttribute("href")).toBe("/tok/src/tree/README.md");
  });

  it("not local", async () => {
    const main = await open({ kind: "not-local" });
    expect(main.textContent).toContain(noteTexts.notLocal);
    expect(main.querySelector(".md-page")).toBeNull();
    expect(seeFile(main)?.getAttribute("href")).toBe("/tok/src/tree/README.md");
  });

  it("too slow", async () => {
    const main = await open({ kind: "too-slow" });
    expect(main.textContent).toContain("This file took too long to render.");
    expect(main.querySelector(".md-page")).toBeNull();
    expect(seeFile(main)?.getAttribute("href")).toBe("/tok/src/tree/README.md");
  });

  it("the filtered note, on a doc and on a notice", async () => {
    let main = await open({ kind: "doc", doc, filtered: true });
    expect(main.querySelector(".file-note")?.textContent).toBe(
      noteTexts.filtered,
    );
    expect(main.querySelector(".md-page")).not.toBeNull();
    unmountApp();
    main = await open({ kind: "too-slow", filtered: true });
    expect(main.querySelector(".file-note")?.textContent).toBe(
      noteTexts.filtered,
    );
  });
});
