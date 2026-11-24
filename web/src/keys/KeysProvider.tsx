// Single-key shortcuts (SPEC §5.3, D-297): one dispatcher on document,
// the ? overlay, the view links of the header. Views add their own
// actions with useKeyActions and their Docs / Source links with
// useViewLinks; the latest one added wins.
import { useLocation, useNavigate, useResolvedPath } from "@solidjs/router";
import {
  type Accessor,
  createContext,
  createEffect,
  createSignal,
  type JSX,
  on,
  onCleanup,
  onMount,
  type ParentProps,
  useContext,
} from "solid-js";
import { fileStepTexts, notDiffMode } from "../diff/words";
import { usePrefs } from "../prefs/store";
import { useSession } from "../session";
import { copyText } from "../copy/clipboard";
import { withBase } from "../files/paths";
import type { DiffSessionSide } from "../sides";
import { createDispatcher } from "./dispatch";
import type { FileStep } from "./fileSteps";
import type { ActionId } from "./keymap";
import { KeysHelp } from "./KeysHelp";

// True: done. A string: done, and say it (no link here, …). afterNav:
// done, and say it once the navigation it started has changed the URL
// (the status is cleared by then); dropped after 1 s, or by any key.
// False: nothing to do here, the key
// goes on.
export type Done = true | string | { afterNav: string };

export type KeyHandler = () => false | Done;

export type KeyActions = Partial<Record<ActionId, KeyHandler>>;

export type ViewName = "docs" | "source" | "refs" | "diff";

export type ToView = Exclude<ViewName, "refs">;

export interface ViewTarget {
  // A router path (no base).
  href: string;
  state?: unknown;
}

export interface ViewLinks {
  current: ViewName;
  // Router paths. Undefined: this page has none.
  docs: () => string | undefined;
  source: () => string | undefined;
  // The diff of this page's file (diff mode only). Undefined: none.
  diff?: () => string | undefined;
  // Where 1 / 2 / 3 and the header links go: the same place in the
  // other view. Undefined: docs() / source() / diff(). null: none here
  // (said).
  at?: (to: ToView) => ViewTarget | null | undefined;
  // What to say when a view has no target here, if not the default.
  missing?: (to: ToView) => string | undefined;
  // Called right before goView leaves this page.
  leave?: () => void;
}

// What Copy location copies: a text, or words to say instead.
export type LocationText = string | { say: string };

// Where b goes, with the line or declaration. Words: said there.
export interface SideGo {
  href: string;
  state?: unknown;
  words?: string;
}

// View on base / head and b (D-349). go: top is the other page's top
// (the header link); at, only on use: the exact place (else top).
// off: why not. here: the page does it (the diff view).
export type SideTarget =
  | {
      kind: "go";
      side: DiffSessionSide;
      top: string;
      at?: () => SideGo;
      leave?: () => void;
    }
  | { kind: "off"; side: DiffSessionSide; why: string }
  | { kind: "here"; run: (fromKey: boolean) => Done };

export const noOtherSide = "No other side for this page.";

export interface KeysApi {
  links: Accessor<ViewLinks | undefined>;
  // Go to the other view, keeping the place.
  goView: (to: ToView) => void;
  // opener: the button that opened it, to get focus back if nothing
  // else had it (WebKit doesn't focus a clicked button).
  openHelp: (opener?: HTMLElement) => void;
  // For the polite status in the header.
  status: Accessor<string>;
  // Say something on the status line.
  say: (text: string) => void;
  // The view's Copy location, if it has one (the header button).
  location: Accessor<(() => LocationText | undefined) | undefined>;
  copyLocation: () => void;
  // The page's side switch, if it has one (the header item).
  sideTarget: Accessor<(() => SideTarget | undefined) | undefined>;
  goSide: (fromKey?: boolean) => void;
  // ] and [, and the Previous / Next file buttons (fromButton).
  stepFile: (dir: 1 | -1, fromButton?: boolean) => void;
  // The button of dir on the page a button press of dir went to: true
  // once, if it should take focus back (D-350).
  takeRefocus: (dir: 1 | -1) => boolean;
  register: (actions: KeyActions) => () => void;
  addLinks: (links: ViewLinks) => () => void;
  addLocation: (f: () => LocationText | undefined) => () => void;
  addSideTarget: (f: () => SideTarget | undefined) => () => void;
  addFileSteps: (f: (dir: 1 | -1) => FileStep) => () => void;
}

export const KeysContext = createContext<KeysApi>();

export function useKeys(): KeysApi | undefined {
  return useContext(KeysContext);
}

// Actions of a view, while it lives. No-op outside a KeysProvider
// (tests that mount a view alone).
export function useKeyActions(actions: KeyActions): void {
  const keys = useKeys();
  if (keys === undefined) return;
  onCleanup(keys.register(actions));
}

export function useViewLinks(links: ViewLinks): void {
  const keys = useKeys();
  if (keys === undefined) return;
  onCleanup(keys.addLinks(links));
}

// Copy location of a view, while it lives.
export function useCopyLocation(f: () => LocationText | undefined): void {
  const keys = useKeys();
  if (keys === undefined) return;
  onCleanup(keys.addLocation(f));
}

// The page's side switch, while it lives.
export function useSideTarget(f: () => SideTarget | undefined): void {
  const keys = useKeys();
  if (keys === undefined) return;
  onCleanup(keys.addSideTarget(f));
}

// ] and [ of a view, while it lives.
export function useFileSteps(f: (dir: 1 | -1) => FileStep): void {
  const keys = useKeys();
  if (keys === undefined) return;
  onCleanup(keys.addFileSteps(f));
}

// A stack of handlers: the top one wins; add gives the remover.
function stack<T>(): {
  all: Accessor<readonly T[]>;
  add: (x: T) => () => void;
} {
  const [all, setAll] = createSignal<readonly T[]>([]);
  return {
    all,
    add: (x) => {
      setAll((xs) => [...xs, x]);
      return () => setAll((xs) => xs.filter((y) => y !== x));
    },
  };
}

const missingWords = {
  docs: "No docs for this page.",
  source: "No source for this page.",
  diff: "No diff for this page.",
};

export function KeysProvider(props: ParentProps): JSX.Element {
  const prefs = usePrefs();
  const session = useSession();
  const navigate = useNavigate();
  const [helpOpen, setHelpOpen] = createSignal(false);
  let opener: HTMLElement | undefined;
  const actions = stack<KeyActions>();
  const linkStack = stack<ViewLinks>();
  const links = () => linkStack.all().at(-1);
  const locations = stack<() => LocationText | undefined>();
  const locationFn = () => locations.all().at(-1);
  const sides = stack<() => SideTarget | undefined>();
  const sideTarget = () => sides.all().at(-1);
  const steps = stack<(dir: 1 | -1) => FileStep>();

  // The same text again isn't read: it gets a no-break space at the
  // end, or loses it (as in FindBar). Cleared after a while.
  const [status, setStatus] = createSignal("");
  let timer: ReturnType<typeof setTimeout> | undefined;
  const say = (text: string) => {
    const old = status();
    const nbsp = "\u00a0";
    if (text !== "" && old.replace(/\u00a0$/, "") === text) {
      setStatus(old.endsWith(nbsp) ? text : text + nbsp);
    } else setStatus(text);
    clearTimeout(timer);
    timer = setTimeout(() => setStatus(""), 5000);
  };
  onCleanup(() => clearTimeout(timer));
  // Words for after the next navigation (KeyHandler's afterNav).
  // Dropped after 1 s, or by any key.
  let afterNav: string | undefined;
  let afterNavTimer: ReturnType<typeof setTimeout> | undefined;
  const dropAfterNav = () => {
    clearTimeout(afterNavTimer);
    afterNav = undefined;
  };
  onCleanup(() => clearTimeout(afterNavTimer));
  // A new page: the old words are about the old one.
  const location = useLocation();
  createEffect(
    on(
      () => [location.pathname, location.search, location.hash],
      () => {
        clearTimeout(timer);
        const words = afterNav;
        dropAfterNav();
        if (words !== undefined) say(words);
        else setStatus("");
      },
      { defer: true },
    ),
  );

  const goView = (to: ToView): void => {
    const l = links();
    if (l === undefined) return;
    if (l.current === to) return;
    if (to === "diff" && session.mode !== "diff") {
      say(notDiffMode);
      return;
    }
    const a = l.at?.(to);
    const t = a === null ? undefined : (a ?? hrefOnly(l[to]?.()));
    if (t === undefined) {
      say(l.missing?.(to) ?? missingWords[to]);
      return;
    }
    l.leave?.();
    navigate(t.href, { state: t.state });
  };

  const done = (r: false | Done): boolean => {
    if (typeof r === "string") {
      say(r);
      return true;
    }
    if (typeof r === "object") {
      afterNav = r.afterNav;
      afterNavTimer = setTimeout(dropAfterNav, 1000);
      return true;
    }
    return r;
  };

  const goSide = (fromKey = false): void => {
    if (session.mode !== "diff") {
      say(notDiffMode);
      return;
    }
    const t = sideTarget()?.();
    if (t === undefined) {
      say(noOtherSide);
      return;
    }
    switch (t.kind) {
      case "off":
        say(t.why);
        return;
      case "here":
        done(t.run(fromKey));
        return;
      case "go": {
        const g = t.at?.() ?? { href: t.top };
        t.leave?.();
        if (g.words !== undefined) done({ afterNav: g.words });
        navigate(g.href, { state: g.state });
      }
    }
  };

  // A Previous / Next file press that navigated: the same button on
  // the page it went to takes focus back, if nothing else did, so it can
  // be pressed again (D-350). Dropped on any other page, click or key.
  let carry: { dir: 1 | -1; at: string } | undefined;
  const base = useResolvedPath(() => "/");
  const here = () => location.pathname + location.search;
  createEffect(
    on(
      here,
      (h) => {
        if (carry !== undefined && carry.at !== h) carry = undefined;
      },
      { defer: true },
    ),
  );
  const dropCarry = () => (carry = undefined);
  onMount(() => {
    document.addEventListener("pointerdown", dropCarry, true);
    document.addEventListener("keydown", dropCarry, true);
    onCleanup(() => {
      document.removeEventListener("pointerdown", dropCarry, true);
      document.removeEventListener("keydown", dropCarry, true);
    });
  });
  const takeRefocus = (dir: 1 | -1): boolean => {
    const c = carry;
    if (c === undefined || c.dir !== dir || c.at !== here()) return false;
    carry = undefined;
    return true;
  };

  // The view's leave first, so Back comes to this place.
  const stepFile = (dir: 1 | -1, fromButton = false): void => {
    carry = undefined;
    if (session.mode !== "diff") {
      say(notDiffMode);
      return;
    }
    const f = steps.all().at(-1);
    const r = f === undefined ? fileStepTexts.elsewhere : f(dir);
    if (typeof r === "string") {
      say(r);
      return;
    }
    links()?.leave?.();
    done({ afterNav: r.words });
    if (fromButton) carry = { dir, at: withBase(base(), r.href) };
    navigate(r.href);
  };

  const run = (id: ActionId): boolean => {
    dropAfterNav();
    switch (id) {
      case "help":
        opener = undefined;
        setHelpOpen(true);
        return true;
      case "search": {
        const input = document.querySelector<HTMLInputElement>(
          ".search-box-header input",
        );
        if (input === null) return false;
        input.focus();
        input.select();
        return true;
      }
      case "docsView":
        goView("docs");
        return true;
      case "sourceView":
        goView("source");
        return true;
      case "diffView":
        goView("diff");
        return true;
      case "otherSide":
        goSide(true);
        return true;
      case "nextFile":
      case "prevFile":
        stepFile(id === "nextFile" ? 1 : -1);
        return true;
      case "copy": {
        // A tree row, a refs row: the text is in the element.
        const own = document.activeElement?.closest<HTMLElement>("[data-copy]");
        if (own !== null && own !== undefined) {
          void copyText(own.dataset.copy ?? "", say);
          return true;
        }
        // In the sidebar, the view's idea of "current" is not ours.
        if (document.activeElement?.closest(".frame-sidebar") != null) {
          return sayNothing();
        }
        return runView(id) || sayNothing();
      }
      default:
        return runView(id) || (id === "escape" && clearSelection());
    }
  };

  const sayNothing = (): boolean => {
    say("Nothing to copy here.");
    return true;
  };

  // The latest view with a handler for id.
  const runView = (id: ActionId): boolean => {
    const list = actions.all();
    for (let i = list.length - 1; i >= 0; i--) {
      const h = list[i]?.[id];
      if (h === undefined) continue;
      return done(h());
    }
    return false;
  };

  const copyLocation = (): void => {
    const t = locationFn()?.();
    if (t === undefined) say("No location here.");
    else if (typeof t === "string") void copyText(t, say);
    else say(t.say);
  };

  const api: KeysApi = {
    links,
    goView,
    openHelp: (from) => {
      opener = from;
      setHelpOpen(true);
    },
    status,
    say,
    location: locationFn,
    copyLocation,
    sideTarget,
    goSide,
    stepFile,
    takeRefocus,
    register: actions.add,
    addLinks: linkStack.add,
    addLocation: locations.add,
    addSideTarget: sides.add,
    addFileSteps: steps.add,
  };

  onMount(() => {
    const handler = createDispatcher({
      enabled: prefs.singleKeyShortcuts,
      dialogOpen: helpOpen,
      run,
    });
    // See dispatch.ts on the order with component keys.
    document.addEventListener("keydown", handler);
    onCleanup(() => document.removeEventListener("keydown", handler));
  });

  return (
    <KeysContext.Provider value={api}>
      {props.children}
      <KeysHelp
        open={helpOpen()}
        opener={() => opener}
        onClose={() => setHelpOpen(false)}
      />
    </KeysContext.Provider>
  );
}

function hrefOnly(href: string | undefined): ViewTarget | undefined {
  return href === undefined ? undefined : { href };
}

// Esc with nothing else to close.
function clearSelection(): boolean {
  const sel = getSelection();
  if (sel === null || sel.isCollapsed) return false;
  sel.removeAllRanges();
  return true;
}

// A save failure or "no link here": polite, in the header. Mounted at
// all times, like PrefsStatus.
export function KeysStatus(): JSX.Element {
  const keys = useKeys();
  return (
    <p class="keys-status" aria-live="polite">
      {keys?.status() ?? ""}
    </p>
  );
}
