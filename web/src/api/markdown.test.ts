import { describe, expect, it } from "vitest";
import { DecodeError } from "./decode";
import { decodeMarkdownPage } from "./markdown";

const page = (blocks: unknown[]) => ({
  generation: 3,
  kind: "doc",
  doc: { blocks, since: null },
});

describe("decodeMarkdownPage", () => {
  it("every Markdown node", () => {
    const v = decodeMarkdownPage(
      page([
        {
          kind: "heading",
          level: 1,
          inlines: [{ kind: "text", text: "T" }],
          anchor: "t",
        },
        {
          kind: "quote",
          blocks: [{ kind: "rule" }],
        },
        {
          kind: "listing",
          lang: "haskell",
          pieces: [
            [2, "x"],
            [null, " = "],
            [4, "1"],
          ],
        },
        {
          kind: "tasks",
          items: [{ done: true, blocks: [] }],
        },
        {
          kind: "paragraph",
          inlines: [
            { kind: "strike", inlines: [] },
            { kind: "file", path: "a/B.hs", fragment: "L3", label: null },
            { kind: "anchor", anchor: "t", label: null },
          ],
        },
      ]),
    );
    expect(v).toEqual({
      generation: 3,
      kind: "doc",
      doc: {
        since: null,
        blocks: [
          {
            kind: "heading",
            level: 1,
            inlines: [{ kind: "text", text: "T" }],
            anchor: "t",
          },
          { kind: "quote", blocks: [{ kind: "rule" }] },
          {
            kind: "listing",
            lang: "haskell",
            pieces: [
              [2, "x"],
              [null, " = "],
              [4, "1"],
            ],
          },
          { kind: "tasks", items: [{ done: true, blocks: [] }] },
          {
            kind: "paragraph",
            inlines: [
              { kind: "strike", inlines: [] },
              { kind: "file", path: "a/B.hs", fragment: "L3", label: null },
              { kind: "anchor", anchor: "t", label: null },
            ],
          },
        ],
      },
    });
  });

  it("not text, too slow", () => {
    expect(decodeMarkdownPage({ generation: 1, kind: "not-text" })).toEqual({
      generation: 1,
      kind: "not-text",
    });
    expect(decodeMarkdownPage({ generation: 1, kind: "too-slow" })).toEqual({
      generation: 1,
      kind: "too-slow",
    });
  });

  it("rejects a bad class, a bad piece, a bad link", () => {
    const bad = (b: unknown) => () => decodeMarkdownPage(page([b]));
    expect(bad({ kind: "listing", lang: null, pieces: [[99, "x"]] })).toThrow(
      DecodeError,
    );
    expect(bad({ kind: "listing", lang: null, pieces: [["x"]] })).toThrow(
      DecodeError,
    );
    expect(
      bad({
        kind: "paragraph",
        // eslint-disable-next-line no-script-url -- the decoder must refuse it
        inlines: [{ kind: "link", url: "javascript:x", label: null }],
      }),
    ).toThrow(DecodeError);
  });
});
