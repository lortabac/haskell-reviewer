// GET api/markdown/<side>/<path>: a Markdown file, rendered, CBOR.
// Shape: see Note [Markdown page] in
// HaskellReviewer.Server.Wire.Markdown.
import { bool, field, nat, object, oneOf } from "./decode";
import { decodeDoc, type Doc } from "./richtext";

// not-text: the file isn't text (binary, too large, ...); its file
// view says why. not-local: the blob is not in the local repo (partial
// clone). too-slow: parsing it took too long.
// filtered: as on the file view.
export type MarkdownPage = { generation: number; filtered: boolean } & (
  | { kind: "doc"; doc: Doc }
  | { kind: "not-text" }
  | { kind: "not-local" }
  | { kind: "too-slow" }
);

const kinds = ["doc", "not-text", "not-local", "too-slow"] as const;

export function decodeMarkdownPage(value: unknown): MarkdownPage {
  const o = object(value, "$");
  const generation = field(o, "generation", "$", nat);
  const filtered = field(o, "filtered", "$", bool);
  const kind = field(o, "kind", "$", oneOf(kinds));
  return kind === "doc"
    ? { generation, filtered, kind, doc: field(o, "doc", "$", decodeDoc) }
    : { generation, filtered, kind };
}
