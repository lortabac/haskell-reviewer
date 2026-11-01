// GET api/search/<side>?q=: modules and names that match (D-207).
// Shape: see encodeSearch in HaskellReviewer.Server.Wire.
import {
  DecodeError,
  array,
  bool,
  field,
  nat,
  nullable,
  object,
  oneOf,
  str,
} from "./decode";

export const searchKinds = ["module", "decl"] as const;

export const searchNamespaces = [
  "type",
  "class",
  "con",
  "field",
  "val",
  "pat",
] as const;

export type SearchNamespace = (typeof searchNamespaces)[number];

export type SearchResult =
  | { kind: "module"; name: string; ref: string }
  | {
      kind: "decl";
      ns: SearchNamespace;
      name: string;
      // The type of a field; null for anything else.
      parent: string | null;
      // The declaration id, for the URL's #.
      anchor: string;
      // The module's name, and its ref for the URL.
      module: string;
      ref: string;
      exported: boolean;
    };

export type Search = {
  generation: number;
  // q as sent.
  query: string;
  // Still indexing: names may be missing.
  partial: boolean;
  // All hits; results has the best 50 at most.
  matches: number;
  results: SearchResult[];
};

function decodeResult(value: unknown, path: string): SearchResult {
  const o = object(value, path);
  const kind = field(o, "kind", path, oneOf(searchKinds));
  switch (kind) {
    case "module":
      return {
        kind,
        name: field(o, "name", path, str),
        ref: field(o, "ref", path, str),
      };
    case "decl": {
      const ns = field(o, "ns", path, oneOf(searchNamespaces));
      const parent = field(o, "parent", path, nullable(str));
      if ((ns === "field") !== (parent !== null)) {
        throw new DecodeError(`${path}.parent`, "a type for fields only");
      }
      return {
        kind,
        ns,
        name: field(o, "name", path, str),
        parent,
        anchor: field(o, "anchor", path, str),
        module: field(o, "module", path, str),
        ref: field(o, "ref", path, str),
        exported: field(o, "exported", path, bool),
      };
    }
  }
}

export function decodeSearch(value: unknown, path = "$"): Search {
  const o = object(value, path);
  const search = {
    generation: field(o, "generation", path, nat),
    query: field(o, "query", path, str),
    partial: field(o, "partial", path, bool),
    matches: field(o, "matches", path, nat),
    results: field(o, "results", path, array(decodeResult)),
  };
  if (search.results.length > search.matches) {
    throw new DecodeError(path, "results.length <= matches");
  }
  return search;
}

// The route for a query, relative to the token, for getJson.
export function searchUrl(q: string): string {
  return `/api/search/tree?q=${encodeURIComponent(q)}`;
}
