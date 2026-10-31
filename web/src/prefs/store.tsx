// The UI preferences (theme, exported only, sidebar view): kept in a
// signal each, changed optimistically, saved with api/prefs (D-197,
// D-203). The first GET happens in main.tsx, before render (so the
// theme is right from the first paint); this provider only takes that
// value and keeps it in sync with the server from then on.
import {
  type Accessor,
  createContext,
  createEffect,
  createSignal,
  type JSX,
  onCleanup,
  onMount,
  type ParentProps,
  useContext,
} from "solid-js";
import { postJson } from "../api/client";
import {
  decodePrefs,
  type Prefs,
  type SidebarView,
  type Theme,
} from "../api/prefs";
import { applyTheme } from "./theme";

export interface PrefsApi {
  theme: Accessor<Theme>;
  setTheme: (t: Theme) => void;
  exportedOnly: Accessor<boolean>;
  setExportedOnly: (v: boolean) => void;
  sidebar: Accessor<SidebarView>;
  setSidebar: (v: SidebarView) => void;
  // A save failed: a polite status for the header's live region. Clears
  // once every key's latest save has succeeded.
  status: Accessor<string | undefined>;
}

type Key = "theme" | "exportedOnly" | "sidebar";
const keys: readonly Key[] = ["theme", "exportedOnly", "sidebar"];

const PrefsContext = createContext<PrefsApi>();

export function PrefsProvider(
  props: ParentProps<{
    initial: Prefs;
    // main.tsx's own GET, not raced against the 500 ms fallback: when
    // it resolves, late (or the race's loser), it fills in whatever
    // keys the user hasn't touched since (C3).
    pending?: Promise<Prefs>;
  }>,
): JSX.Element {
  const [theme, setThemeSignal] = createSignal(props.initial.theme);
  const [exportedOnly, setExportedOnlySignal] = createSignal(
    props.initial.exportedOnly,
  );
  const [sidebar, setSidebarSignal] = createSignal(props.initial.sidebar);
  const [status, setStatusSignal] = createSignal<string>();

  // main.tsx already set it once, before this mounts; this keeps it in
  // sync with every later change (and tests that mount the app alone).
  createEffect(() => applyTheme(document.documentElement, theme()));

  // A repeated identical message needs a real gap before it's set
  // again, or Chromium's accessibility tree sees no diff within the
  // same task and never re-announces it (round 2 review: a same-task
  // clear-then-set wasn't enough). Clear now; set (if there's still
  // something to say) a bit later. A newer call cancels a still-
  // pending one, so only the latest status ever lands.
  let statusTimer: ReturnType<typeof setTimeout> | undefined;
  const setStatus = (msg: string | undefined): void => {
    if (statusTimer !== undefined) clearTimeout(statusTimer);
    statusTimer = undefined;
    setStatusSignal(undefined);
    if (msg !== undefined) {
      statusTimer = setTimeout(() => {
        statusTimer = undefined;
        setStatusSignal(msg);
      }, 100);
    }
  };
  onCleanup(() => {
    if (statusTimer !== undefined) clearTimeout(statusTimer);
  });

  // A key touched by the user since mount: a late GET (pending, below)
  // must not override it.
  const touched: Record<Key, boolean> = {
    theme: false,
    exportedOnly: false,
    sidebar: false,
  };
  // A plain promise, read once (not a reactive value): no effect
  // needed, but onMount gives eslint-plugin-solid the "one-time" scope
  // it expects for a prop read outside JSX.
  onMount(() => {
    void props.pending?.then((p) => {
      if (!touched.theme) setThemeSignal(p.theme);
      if (!touched.exportedOnly) setExportedOnlySignal(p.exportedOnly);
      if (!touched.sidebar) setSidebarSignal(p.sidebar);
    });
  });

  // Only the latest outstanding save *of that key* may change that
  // key's failed flag: an earlier one's answer, arriving after a later
  // one for the same key already settled, says nothing new (C2). Each
  // key has its own counter, so a failed theme save can't be hidden by
  // an unrelated, later exportedOnly save succeeding (round 2 review).
  // The status shows while any key's latest save has failed, and
  // clears only once every key's latest save has succeeded.
  const latest: Record<Key, number> = { theme: 0, exportedOnly: 0, sidebar: 0 };
  const failed: Record<Key, boolean> = {
    theme: false,
    exportedOnly: false,
    sidebar: false,
  };
  const updateStatus = (): void => {
    setStatus(
      keys.some((k) => failed[k])
        ? "Couldn't save this preference."
        : undefined,
    );
  };
  // Same-key POSTs can still reach the server out of order (two clicks
  // close together: Dark, then Light): accepted, not corrected for.
  // It's one process, one lock per store (Session.Prefs), on
  // localhost; the window is a few ms (D-200's Note [Saving
  // preferences]), and the client's own value is never touched by
  // either answer (C2) either way.
  const save = (key: Key, patch: Partial<Prefs>): void => {
    const mine = ++latest[key];
    postJson("/api/prefs", patch, decodePrefs).then(
      (r) => {
        if (mine !== latest[key]) return;
        failed[key] = r.kind !== "ok";
        updateStatus();
      },
      () => {
        if (mine !== latest[key]) return;
        failed[key] = true;
        updateStatus();
      },
    );
  };

  // The value we chose is never replaced by a save's answer (C2): it
  // only reports whether the save worked, via status. A click that
  // doesn't change anything (the already-pressed theme button) is a
  // no-op: no touch, no save.
  const api: PrefsApi = {
    theme,
    setTheme: (t) => {
      if (t === theme()) return;
      touched.theme = true;
      setThemeSignal(t);
      save("theme", { theme: t });
    },
    exportedOnly,
    setExportedOnly: (v) => {
      if (v === exportedOnly()) return;
      touched.exportedOnly = true;
      setExportedOnlySignal(v);
      save("exportedOnly", { exportedOnly: v });
    },
    sidebar,
    setSidebar: (v) => {
      if (v === sidebar()) return;
      touched.sidebar = true;
      setSidebarSignal(v);
      save("sidebar", { sidebar: v });
    },
    status,
  };
  return (
    <PrefsContext.Provider value={api}>{props.children}</PrefsContext.Provider>
  );
}

export function usePrefs(): PrefsApi {
  const p = useContext(PrefsContext);
  if (p === undefined) throw new Error("usePrefs: no PrefsProvider");
  return p;
}

// A save failure, announced politely in the header. Mounted (and kept
// in the DOM) at all times: only its text changes (C4), so the region
// is there before there is anything to say and a screen reader never
// misses an announcement. docs.css keeps it out of the header's gap
// while it's empty, without taking it out of the accessibility tree.
export function PrefsStatus(): JSX.Element {
  const prefs = usePrefs();
  return (
    <p class="prefs-status" aria-live="polite">
      {prefs.status() ?? ""}
    </p>
  );
}
