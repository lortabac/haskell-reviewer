// Text search page URLs: /<token>/search/<side>?q=<text> (D-310).
import { param } from "../refs/urls";
import type { Side } from "../sides";

// Relative to the token. q: the text between the quotes.
export function textSearchHref(side: Side, q: string): string {
  return `/search/${side}?q=${encodeURIComponent(q)}`;
}

// The text a location asks for: undefined if there is none, it is
// empty, or its encoding is broken. A raw "+" stays "+", as the server
// reads it.
export function textQueryFromLocation(search: string): string | undefined {
  const q = param(search, "q");
  return q === undefined || q === "" ? undefined : q;
}
