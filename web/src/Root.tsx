// The frame around every page, and the side gate (D-332).
import { type RouteSectionProps, useLocation } from "@solidjs/router";
import {
  createMemo,
  createSignal,
  type JSX,
  type ParentProps,
  Show,
} from "solid-js";
import { defaultPrefs, type Prefs } from "./api/prefs";
import { defaultSide, sessionSides } from "./api/session";
import { CopyLocation } from "./copy/CopyLocation";
import { DiffFilesProvider } from "./diff/store";
import { ReloadApply } from "./events/ReloadApply";
import { Frame } from "./Frame";
import { ReloadButton } from "./header/ReloadButton";
import { RevisionInfo } from "./header/RevisionInfo";
import { KeysProvider, KeysStatus } from "./keys/KeysProvider";
import { ShortcutsButton } from "./keys/ShortcutsButton";
import { SideSwitch } from "./keys/SideSwitch";
import { ViewSwitch } from "./keys/ViewSwitch";
import { ProjectNotices } from "./modules/ProjectNotices";
import { ModulesProvider } from "./modules/store";
import { NotFound } from "./NotFound";
import { PrefsProvider, PrefsStatus } from "./prefs/store";
import { ThemeSwitch } from "./prefs/ThemeSwitch";
import { SearchBox } from "./search/SearchBox";
import { notDiffMode } from "./diff/words";
import { onSide, SideContext, urlSide, useSession } from "./session";
import { Sidebar } from "./Sidebar";
import { isSide, type Side } from "./sides";

// The URL's side: ok (the URL's), none (a page with no side: the
// default), or one the session doesn't have, as typed.
type PageSide =
  | { kind: "ok"; side: Side }
  | { kind: "none"; side: Side }
  | { kind: "wrong"; typed: string };

export function Root(
  props: RouteSectionProps & {
    initialPrefs?: Prefs;
    pendingPrefs?: Promise<Prefs>;
  },
): JSX.Element {
  const session = useSession();
  const location = useLocation();
  // A side in the URL that the session doesn't have (G3): no page, no
  // page fetch, a note instead.
  const page = createMemo((): PageSide => {
    const s = urlSide(location.pathname);
    if (s === undefined) return { kind: "none", side: defaultSide(session) };
    return isSide(s) && sessionSides(session).includes(s)
      ? { kind: "ok", side: s }
      : { kind: "wrong", typed: s };
  });
  const wrongSide = (): string | undefined => {
    const p = page();
    return p.kind === "wrong" ? p.typed : undefined;
  };
  // The sidebar and search use the default side on a wrong-side page.
  const side = createMemo((): Side => {
    const p = page();
    return p.kind === "wrong" ? defaultSide(session) : p.side;
  });
  // The side shown in the header: side pages only, not Home or not
  // found.
  const current = (): Side | undefined => {
    const p = page();
    return p.kind === "ok" ? p.side : undefined;
  };
  // Bumped by a reload (D-354): the page mounts again.
  const [epoch, setEpoch] = createSignal(0);
  const pageKey = () => `${side()} ${epoch()}`;
  return (
    <SideContext.Provider value={side}>
      <PrefsProvider
        initial={props.initialPrefs ?? defaultPrefs}
        pending={props.pendingPrefs}
      >
        <DiffFilesProvider>
          <ModulesProvider>
            <KeysProvider>
              <ReloadApply remount={() => setEpoch((e) => e + 1)}>
                <Frame
                  sidebar={<Sidebar />}
                  revisions={<RevisionInfo current={current()} />}
                  notices={<ProjectNotices />}
                  views={
                    <>
                      <ViewSwitch />
                      <SideSwitch />
                    </>
                  }
                  search={<SearchBox placement="header" />}
                  controls={
                    <>
                      <ReloadButton />
                      <CopyLocation />
                      <ThemeSwitch />
                      <ShortcutsButton />
                      <PrefsStatus />
                      <KeysStatus />
                    </>
                  }
                >
                  <Show
                    when={wrongSide()}
                    fallback={
                      // Keyed: a side change (an edited URL) or a reload
                      // mounts the page again. The router keeps route
                      // components when only params change, so their
                      // resources would keep the old side. Reading
                      // props.children here builds new routes.
                      <Show when={pageKey()} keyed>
                        {(s) => <Keyed on={s}>{props.children}</Keyed>}
                      </Show>
                    }
                  >
                    {(typed) => <WrongSide side={typed()} />}
                  </Show>
                </Frame>
              </ReloadApply>
            </KeysProvider>
          </ModulesProvider>
        </DiffFilesProvider>
      </PrefsProvider>
    </SideContext.Provider>
  );
}

// Just its children. on: the value the Show around it is keyed on (so
// the Show's callback has an argument to use).
function Keyed(props: ParentProps<{ on: string }>): JSX.Element {
  return <>{props.children}</>;
}

// A URL with a side the session doesn't have, and one explicit link to
// the same page on the default side (§5.1: never a silent switch).
function WrongSide(props: { side: string }): JSX.Element {
  const session = useSession();
  const location = useLocation();
  const to = defaultSide(session);
  const note = (): string =>
    session.mode === "diff"
      ? `This session compares base and head; there is no “${props.side}” side.`
      : props.side === "base" || props.side === "head"
        ? notDiffMode
        : `This session has only the tree side; there is no “${props.side}” side.`;
  return (
    <NotFound
      note={note()}
      link={{
        href: onSide(location.pathname, location.search, location.hash, to),
        text: `Open on ${to}`,
      }}
    />
  );
}
