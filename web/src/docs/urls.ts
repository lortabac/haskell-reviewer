// Docs view URLs: /<token>/docs/tree/<ref…>[#<decl>] (SPEC §4.1).
import { encodePath, segmentsAfter } from "../files/paths";

// The module ref in a full pathname, as typed. Loose modules' refs are
// paths, so the segments are joined with "/". Undefined if it is not a
// docs URL or a segment is bad (segmentsAfter).
export function refFromPathname(pathname: string): string | undefined {
  return segmentsAfter(pathname, ["docs", "tree"])?.join("/");
}

// The module page API URL, relative to the token.
export function apiModuleUrl(ref: string): string {
  return "/api/module/tree/" + encodePath(ref);
}

// The anchor in a hash: null for none, "bad" for broken percent
// encoding (shown as not found).
export function anchorFromHash(hash: string): string | null | { bad: true } {
  const raw = hash.startsWith("#") ? hash.slice(1) : hash;
  if (raw === "") return null;
  try {
    return decodeURIComponent(raw);
  } catch {
    return { bad: true };
  }
}

// What to put in the search box of a "not found" page: the module ref
// of a docs URL, the path of a file URL, else nothing. Lenient: bad
// encoding stays as typed.
export function queryFromUrl(pathname: string): string {
  const [, , view, side, ...rest] = pathname.split("/");
  if ((view !== "docs" && view !== "src") || side !== "tree") return "";
  return rest
    .filter((s) => s !== "")
    .map((s) => {
      try {
        return decodeURIComponent(s);
      } catch {
        return s;
      }
    })
    .join("/");
}

// The name in a declaration anchor, for the search box: "type:Foo" ->
// "Foo", "field:T.f" -> "f", "v:x" -> "x", "inst:Show:Maybe a" ->
// "Show". Anything else as is.
export function anchorName(anchor: string): string {
  const m = /^([a-z]+):(.*)$/s.exec(anchor);
  if (m === null) return anchor;
  const [, ns = "", rest = ""] = m;
  switch (ns) {
    case "field": {
      const dot = rest.lastIndexOf(".");
      return dot === -1 ? rest : rest.slice(dot + 1);
    }
    case "inst": {
      const colon = rest.indexOf(":");
      return colon === -1 ? rest : rest.slice(0, colon);
    }
    default:
      return rest;
  }
}

// Haddock escapes odd characters in anchors as "-<code point>-"
// (makeAnchorId): foldl' is "foldl-39-", <+> is "-60--43--62-". The
// name with those decoded, or undefined if nothing to decode.
export function unescapeHaddock(name: string): string | undefined {
  if (!/-\d+-/.test(name)) return undefined;
  let bad = false;
  const out = name.replace(/-(\d+)-/g, (_, n: string) => {
    const c = Number(n);
    if (c > 0x10ffff) {
      bad = true;
      return "";
    }
    return String.fromCodePoint(c);
  });
  return bad ? undefined : out;
}
