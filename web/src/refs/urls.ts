// Refs page URLs: /<token>/refs/tree/<module ref…>?decl=<id> (SPEC §4.1).
import { encodePath, segmentsAfter } from "../files/paths";

// Relative to the token. The id goes in the query, not the hash: the
// server never sees a hash, and the page is not a place in a document.
export function refsHref(moduleRef: string, decl: string): string {
  return `/refs/tree/${encodePath(moduleRef)}?decl=${encodeURIComponent(decl)}`;
}

// The first value of a query parameter, percent-decoded by hand: a raw
// "+" stays "+", as the server reads it (parseQueryReplacePlus False).
// Undefined if there is none or its own encoding is broken.
function param(search: string, name: string): string | undefined {
  const query = search.startsWith("?") ? search.slice(1) : search;
  for (const part of query.split("&")) {
    const eq = part.indexOf("=");
    const key = eq === -1 ? part : part.slice(0, eq);
    let k: string;
    try {
      k = decodeURIComponent(key);
    } catch {
      // Another parameter's bad encoding doesn't matter.
      continue;
    }
    if (k !== name) continue;
    try {
      return eq === -1 ? "" : decodeURIComponent(part.slice(eq + 1));
    } catch {
      return undefined;
    }
  }
  return undefined;
}

// What a location asks for; undefined if the path is bad or there is no
// decl.
export function refsFromLocation(
  pathname: string,
  search: string,
): { module: string; decl: string } | undefined {
  const segs = segmentsAfter(pathname, ["refs", "tree"]);
  if (segs === undefined) return undefined;
  const decl = param(search, "decl");
  if (decl === undefined || decl === "") return undefined;
  return { module: segs.join("/"), decl };
}
