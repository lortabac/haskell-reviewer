// Diff view URLs:
// /<token>/diff/<path>[?ws=ignore][&layout=unified|split][#<side>-L<n>]. No
// side in the path: the page shows both (D-341). The one place that
// writes "/diff/" by hand (eslint.config.js).
import { encodePath, segmentsAfter } from "../files/paths";
import type { DiffSessionSide } from "../sides";
import type { DiffLayout } from "./split";

// A line on one side.
export interface DiffLine {
  side: DiffSessionSide;
  line: number;
}

// The diff view URL, relative to the token. ws: ignore whitespace.
// layout: only when the page's URL has one (D-344).
export function diffHref(
  path: string,
  opts: { ws?: boolean; layout?: DiffLayout | undefined; at?: DiffLine } = {},
): string {
  const q = new URLSearchParams();
  if (opts.ws === true) q.set("ws", "ignore");
  if (opts.layout !== undefined) q.set("layout", opts.layout);
  const s = q.toString();
  return (
    "/diff/" +
    encodePath(path) +
    (s === "" ? "" : "?" + s) +
    (opts.at === undefined ? "" : diffHash(opts.at))
  );
}

export function diffHash(at: DiffLine): string {
  return `#${at.side}-L${at.line}`;
}

// The path in a full pathname "/<token>/diff/<path>", as
// pathFromPathname.
export function diffPathFromPathname(pathname: string): string | undefined {
  return segmentsAfter(pathname, ["diff"])?.join("/");
}

// "#base-L3" or "#head-L3"; anything else, line 0 or a range:
// undefined.
export function parseDiffHash(hash: string): DiffLine | undefined {
  const m = /^#(base|head)-L([1-9]\d{0,8})$/.exec(hash);
  if (m === null) return undefined;
  return { side: m[1] as DiffSessionSide, line: Number(m[2]) };
}

// ?ws=ignore in a search string.
export function wsFromSearch(search: string): boolean {
  return new URLSearchParams(search).get("ws") === "ignore";
}

// A search string with ws=ignore on or off; other params kept.
export function searchWithWs(search: string, on: boolean): string {
  const q = new URLSearchParams(search);
  if (on) q.set("ws", "ignore");
  else q.delete("ws");
  const s = q.toString();
  return s === "" ? "" : "?" + s;
}

// ?layout=unified|split; anything else: none.
export function layoutFromSearch(search: string): DiffLayout | undefined {
  const l = new URLSearchParams(search).get("layout");
  return l === "unified" || l === "split" ? l : undefined;
}

// A search string with layout set, or dropped (undefined); other
// params kept.
export function searchWithLayout(
  search: string,
  layout: DiffLayout | undefined,
): string {
  const q = new URLSearchParams(search);
  if (layout === undefined) q.delete("layout");
  else q.set("layout", layout);
  const s = q.toString();
  return s === "" ? "" : "?" + s;
}
