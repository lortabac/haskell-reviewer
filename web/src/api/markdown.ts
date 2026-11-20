// GET api/markdown/<side>/<path>: a Markdown file, rendered, CBOR.
// Shape: see Note [Markdown page] in
// HaskellReviewer.Server.Wire.Markdown.
import { field, nat, object, oneOf } from "./decode";
import { decodeDoc, type Doc } from "./richtext";

// not-text: the file isn't text (binary, too large, ...); its file
// view says why. too-slow: parsing it took too long.
export type MarkdownPage = { generation: number } & (
  { kind: "doc"; doc: Doc } | { kind: "not-text" } | { kind: "too-slow" }
);

const kinds = ["doc", "not-text", "too-slow"] as const;

export function decodeMarkdownPage(value: unknown): MarkdownPage {
  const o = object(value, "$");
  const generation = field(o, "generation", "$", nat);
  const kind = field(o, "kind", "$", oneOf(kinds));
  return kind === "doc"
    ? { generation, kind, doc: field(o, "doc", "$", decodeDoc) }
    : { generation, kind };
}
