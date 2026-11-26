// Applies a new generation (D-354): the page goes to its place (the
// view's here, else the URL as it is), with no new history entry, then
// Root mounts it again so it fetches again. Providers refetch by
// themselves (useReloads).
//
// The landing gets a mark in its history state, but the mark counts
// only for that one landing: we keep its n in memory, and drop it on
// the next navigation. So Back, Forward or a page load to that entry
// later is a plain landing.
import { useLocation, useNavigate } from "@solidjs/router";
import {
  createContext,
  createEffect,
  type JSX,
  on,
  type ParentProps,
  untrack,
  useContext,
} from "solid-js";
import { basePath } from "../base";
import { reloadTexts } from "../header/ReloadButton";
import { useKeys } from "../keys/KeysProvider";
import { focusInMain } from "../ui/focus";
import { type Apply, useGeneration } from "./generation";

// In the history state of a page landed on by a reload. n: the apply.
// focus: focus was in main, so the landing takes it.
export interface ReloadMark {
  n: number;
  focus: boolean;
}

function asObject(state: unknown): Record<string, unknown> | undefined {
  return typeof state === "object" && state !== null
    ? (state as Record<string, unknown>)
    : undefined;
}

function reloadOf(state: unknown): ReloadMark | undefined {
  const r = asObject(asObject(state)?.reload);
  if (r === undefined) return undefined;
  return typeof r.n === "number" && typeof r.focus === "boolean"
    ? { n: r.n, focus: r.focus }
    : undefined;
}

// The router path of the page now (no base).
function currentPath(): string {
  const l = window.location;
  return (
    (l.pathname.slice(basePath(l.pathname).length) || "/") + l.search + l.hash
  );
}

// The mark of state if it is the landing of the last apply. None
// outside a ReloadApply (tests that mount a page alone).
const LandingContext = createContext<
  (state: unknown) => ReloadMark | undefined
>(() => undefined);

export function useReloadLanding(): (state: unknown) => ReloadMark | undefined {
  return useContext(LandingContext);
}

// A reload landing with focus outside main: move the cursor, not focus.
export function useQuietReload(): (state: unknown) => boolean {
  const landing = useReloadLanding();
  return (state) => landing(state)?.focus === false;
}

export function ReloadApply(
  props: ParentProps<{ remount: () => void }>,
): JSX.Element {
  const gen = useGeneration();
  const keys = useKeys();
  const navigate = useNavigate();
  const location = useLocation();
  // The apply waiting for its navigation.
  let waiting: Apply | undefined;
  // The n of the landing now on screen; undefined once we leave it.
  let fresh: number | undefined;
  createEffect(
    on(
      gen.applied,
      (a) => {
        if (a === undefined) return;
        const t = keys?.links()?.here?.() ?? {
          href: currentPath(),
          state: location.state,
        };
        waiting = a;
        navigate(t.href, {
          replace: true,
          scroll: false,
          state: {
            ...asObject(t.state),
            reload: { n: a.n, focus: focusInMain() },
          },
        });
      },
      { defer: true },
    ),
  );
  createEffect(
    on(
      () => [location.pathname, location.search, location.hash, location.state],
      () => {
        const r = reloadOf(location.state);
        const w = waiting;
        // Mount again once the router has the new URL and state, so the
        // new page reads them.
        if (w !== undefined && r?.n === w.n) {
          waiting = undefined;
          fresh = w.n;
          props.remount();
          // After the status is cleared for the new URL.
          if (w.notice) queueMicrotask(() => keys?.say(reloadTexts.done));
          return;
        }
        // Another place (a refs page passing the landing on to docs
        // keeps it).
        if (r?.n !== fresh) fresh = undefined;
      },
      { defer: true },
    ),
  );
  const landing = (state: unknown): ReloadMark | undefined => {
    const r = reloadOf(state);
    return r !== undefined && r.n === fresh ? r : undefined;
  };
  return (
    <LandingContext.Provider value={landing}>
      {props.children}
    </LandingContext.Provider>
  );
}

// A page landed on by a reload whose thing is gone (D-354): Home, with
// a notice, no new history entry, and the notice said. gone: the answer
// says not found. what: the module or path.
export function useGoneHome(gone: () => boolean, what: () => string): void {
  const location = useLocation();
  const navigate = useNavigate();
  const keys = useKeys();
  const mark = useReloadLanding()(untrack(() => location.state));
  if (mark === undefined) return;
  createEffect(() => {
    if (!gone()) return;
    const w = what();
    keys?.sayAfterNav(goneText(w));
    // The landing goes on to Home, with what went (state.gone, of this
    // landing only).
    navigate("/", {
      replace: true,
      state: { gone: { what: w, n: mark.n }, reload: mark },
    });
  });
}

// On Home: what the reload landing now on screen took away. Not on a
// later reload, Back, Forward or page load to that entry.
export function useGone(): string | undefined {
  const location = useLocation();
  const state = untrack(() => location.state);
  const mark = useReloadLanding()(state);
  const g = asObject(asObject(state)?.gone);
  return mark !== undefined && g?.n === mark.n && typeof g.what === "string"
    ? g.what
    : undefined;
}

export const goneText = (what: string): string =>
  `${what} is not in the files any more.`;
