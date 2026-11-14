// Single-key shortcuts (SPEC §5.3, D-297): one dispatcher on document,
// the ? overlay, the view links of the header. Views add their own
// actions with useKeyActions and their Docs / Source links with
// useViewLinks; the latest one added wins.
import { useLocation, useNavigate } from "@solidjs/router";
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
import { usePrefs } from "../prefs/store";
import { createDispatcher } from "./dispatch";
import type { ActionId } from "./keymap";
import { KeysHelp } from "./KeysHelp";

// True: done. A string: done, and say it (no link here, …). False:
// nothing to do here, the key goes on.
export type KeyHandler = () => boolean | string;

export type KeyActions = Partial<Record<ActionId, KeyHandler>>;

export type ViewName = "docs" | "source" | "refs";

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
  // Where 1 / 2 and the header links go: the same place in the other
  // view. Undefined: docs() / source().
  at?: (to: "docs" | "source") => ViewTarget | undefined;
}

export interface KeysApi {
  links: Accessor<ViewLinks | undefined>;
  // Go to the other view, keeping the place.
  goView: (to: "docs" | "source") => void;
  // opener: the button that opened it, to get focus back if nothing
  // else had it (WebKit doesn't focus a clicked button).
  openHelp: (opener?: HTMLElement) => void;
  // For the polite status in the header.
  status: Accessor<string>;
  register: (actions: KeyActions) => () => void;
  addLinks: (links: ViewLinks) => () => void;
}

const KeysContext = createContext<KeysApi>();

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

const missingWords = {
  docs: "No docs for this page.",
  source: "No source for this page.",
};

export function KeysProvider(props: ParentProps): JSX.Element {
  const prefs = usePrefs();
  const navigate = useNavigate();
  const [helpOpen, setHelpOpen] = createSignal(false);
  let opener: HTMLElement | undefined;
  const [actions, setActions] = createSignal<readonly KeyActions[]>([]);
  const [linkList, setLinkList] = createSignal<readonly ViewLinks[]>([]);
  const links = () => linkList().at(-1);

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
  // A new page: the old words are about the old one.
  const location = useLocation();
  createEffect(
    on(
      () => [location.pathname, location.search, location.hash],
      () => {
        clearTimeout(timer);
        setStatus("");
      },
      { defer: true },
    ),
  );

  const goView = (to: "docs" | "source"): void => {
    const l = links();
    if (l === undefined) return;
    if (l.current === to) return;
    const t = l.at?.(to) ?? hrefOnly(l[to]());
    if (t === undefined) {
      say(missingWords[to]);
      return;
    }
    navigate(t.href, { state: t.state });
  };

  const done = (r: boolean | string): boolean => {
    if (typeof r === "string") {
      say(r);
      return true;
    }
    return r;
  };

  const run = (id: ActionId): boolean => {
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
      default: {
        const list = actions();
        for (let i = list.length - 1; i >= 0; i--) {
          const h = list[i]?.[id];
          if (h === undefined) continue;
          if (done(h())) return true;
          break;
        }
        return id === "escape" ? clearSelection() : false;
      }
    }
  };

  const api: KeysApi = {
    links,
    goView,
    openHelp: (from) => {
      opener = from;
      setHelpOpen(true);
    },
    status,
    register: (a) => {
      setActions((l) => [...l, a]);
      return () => setActions((l) => l.filter((x) => x !== a));
    },
    addLinks: (l) => {
      setLinkList((xs) => [...xs, l]);
      return () => setLinkList((xs) => xs.filter((x) => x !== l));
    },
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
