// How code and docs on a module page make their links.
import { createContext, useContext } from "solid-js";
import type { Handle } from "../api/module";

export interface Links {
  // The URL of a declaration of this page, if the page has it.
  decl: (n: Handle) => string | undefined;
  // The URL of a module page, with an anchor as written.
  module: (ref: string, anchor: string | null) => string;
}

export const LinksContext = createContext<Links>();

export function useLinks(): Links {
  const l = useContext(LinksContext);
  if (l === undefined) throw new Error("useLinks: no LinksContext");
  return l;
}
