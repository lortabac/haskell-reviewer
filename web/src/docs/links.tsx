// How code and docs on a module page make their links.
import { createContext, useContext } from "solid-js";
import type { Handle } from "../api/module";
import { type LinkInfo, shortName } from "../source/links";

export interface Links {
  // The URL of a declaration of this page, if the page has it.
  decl: (n: Handle) => string | undefined;
  // The URL of a module page, with an anchor as written.
  module: (ref: string, anchor: string | null) => string;
  // The link of a page target (1 + its index): to docs, or Hoogle.
  outer: (k: number) => LinkInfo | undefined;
}

// The attributes of a link out of a page's code or docs: marks for a
// guess and for a new tab, and a short name (T10b).
export function outerAttrs(
  text: string,
  info: LinkInfo,
): {
  href: string;
  class: string | undefined;
  "aria-label": string | undefined;
  target: string | undefined;
  rel: string | undefined;
} {
  const name = shortName(text, info);
  return {
    href: info.href,
    class: info.guess ? "link-guess" : undefined,
    "aria-label": name === text ? undefined : name,
    target: info.external ? "_blank" : undefined,
    rel: info.external ? "noopener noreferrer" : undefined,
  };
}

export const LinksContext = createContext<Links>();

export function useLinks(): Links {
  const l = useContext(LinksContext);
  if (l === undefined) throw new Error("useLinks: no LinksContext");
  return l;
}
