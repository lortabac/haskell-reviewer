// The session and the page's side, as contexts (D-332). App provides
// the session; Root provides the side, from the URL.
import { type Accessor, createContext, useContext } from "solid-js";
import { browseSession, type Session } from "./api/session";
import type { Side } from "./sides";

// Browse mode unless App says otherwise, so tests that mount a part
// of the app need no provider.
export const SessionContext = createContext<Session>(browseSession);

export function useSession(): Session {
  return useContext(SessionContext);
}

const treeSide: Accessor<Side> = () => "tree";

export const SideContext = createContext<Accessor<Side>>(treeSide);

// The side of the page: the URL's, or the session's default on pages
// with none (Home, not found). Links and fetches use it.
export function usePageSide(): Accessor<Side> {
  return useContext(SideContext);
}

// The views whose URLs have a side: /<view>/<side>/….
const sideViews = ["docs", "src", "md", "refs", "search"] as const;

// The side segment of a full pathname "/<token>/<view>/<side>/…", as
// typed, for the views that have one. Undefined if none.
export function urlSide(pathname: string): string | undefined {
  const [, , view, side] = pathname.split("/");
  if (!(sideViews as readonly (string | undefined)[]).includes(view)) {
    return undefined;
  }
  return side === "" ? undefined : side;
}

// The same URL on another side: the side segment swapped.
export function onSide(
  pathname: string,
  search: string,
  hash: string,
  side: Side,
): string {
  const [, , view, , ...rest] = pathname.split("/");
  return ["", view, side, ...rest].join("/") + search + hash;
}
