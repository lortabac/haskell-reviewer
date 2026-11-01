// The docs view: /<token>/docs/tree/<ref…>[#<decl>] (SPEC §4.1, §4.2).
import { useLocation, useNavigate, useResolvedPath } from "@solidjs/router";
import {
  createEffect,
  createMemo,
  createResource,
  createSignal,
  For,
  type JSX,
  Match,
  on,
  onCleanup,
  Show,
  Switch,
} from "solid-js";
import { getCbor } from "../api/client";
import {
  type AnyDecl,
  decodeModulePage,
  docsHref,
  type ModulePage as Page,
  type ParsedModule,
  type TextOnlyModule,
} from "../api/module";
import { hrefFor, lineHref, withBase } from "../files/paths";
import { NotFound } from "../NotFound";
import { PageTitle } from "../PageTitle";
import { usePrefs } from "../prefs/store";
import { onReselectAnchor } from "../reselectAnchor";
import { SearchBox } from "../search/SearchBox";
import { createDelay, focusInMain, focusLost } from "../ui/focus";
import { PageBody } from "./Body";
import { body, hiddenCount } from "./sections";
import {
  anchorKey,
  Anchors,
  openStates,
  type PageCtx,
  PageContext,
} from "./context";
import { readable, WarningNotice } from "./Decl";
import { type Links, LinksContext } from "./links";
import { pageIndex, type PageIndex } from "./pageIndex";
import { reasonBanner, reasonLine } from "./reasons";
import { DocView } from "./RichText";
import {
  anchorFromHash,
  anchorName,
  apiModuleUrl,
  refFromPathname,
  unescapeHaddock,
} from "./urls";

export function DocsView(): JSX.Element {
  const location = useLocation();
  const ref = () => refFromPathname(location.pathname);
  return (
    <Show when={ref()} keyed fallback={<NotFound />}>
      {(r) => <DocsPage moduleRef={r} />}
    </Show>
  );
}

// One module, by its ref as typed in the URL (kept, D-182): a hash
// change doesn't refetch.
function DocsPage(props: { moduleRef: string }): JSX.Element {
  // Read before the old page goes away.
  const fromMain = focusInMain();
  const [res] = createResource(
    () => props.moduleRef,
    (r) => getCbor(apiModuleUrl(r), decodeModulePage),
  );
  const slow = createDelay(() => res.loading, 300);
  // Only the result for this ref, never a stale one.
  const result = () => (res.state === "ready" ? res() : undefined);
  const page = (): Page | undefined => {
    const r = result();
    return r?.kind === "ok" ? r.value : undefined;
  };
  // A link in main took us here: put focus on the new heading, as the
  // old one is gone. Anchors may move it on (ModulePage).
  if (fromMain) {
    createEffect(
      on(result, () => {
        if (focusLost()) {
          document.querySelector<HTMLElement>("#main h1")?.focus();
        }
      }),
    );
  }
  return (
    <Switch
      fallback={
        <>
          <PageTitle text={props.moduleRef} />
          <h1 class="docs-title" tabindex="-1">
            {props.moduleRef}
          </h1>
          <Show when={result()?.kind === "error"}>
            <p class="docs-error">Couldn't load this module.</p>
          </Show>
          <Show when={result() === undefined && slow()}>
            <p>Loading…</p>
          </Show>
        </>
      }
    >
      <Match when={result()?.kind === "not-found"}>
        <NotFound query={props.moduleRef} />
      </Match>
      <Match when={page()} keyed>
        {(p) =>
          p.parse.kind === "parsed" ? (
            <ModulePage page={p} parsed={p.parse} typedRef={props.moduleRef} />
          ) : (
            <TextOnlyPage page={p} parse={p.parse} />
          )
        }
      </Match>
    </Switch>
  );
}

// The file view URL of the module's source at a line, if it has one.
function useSourceHref(
  page: () => Page,
): () => ((line: number) => string) | undefined {
  const base = useResolvedPath(() => "/");
  return () => {
    const path = page().path;
    return path === null
      ? undefined
      : (line: number) => withBase(base(), lineHref(path, line));
  };
}

// Package, component and source file.
function Meta(props: { page: Page }): JSX.Element {
  const base = useResolvedPath(() => "/");
  const pkg = () => {
    const p = props.page.package;
    if (p === null) return undefined;
    return p.version === null ? p.name : `${p.name} ${p.version}`;
  };
  const parts = createMemo((): JSX.Element[] => {
    const out: JSX.Element[] = [];
    const p = pkg();
    if (p !== undefined) out.push(p);
    if (props.page.component !== null) out.push(props.page.component);
    const path = props.page.path;
    if (path !== null) {
      out.push(<a href={withBase(base(), hrefFor(path))}>{path}</a>);
    }
    return out.flatMap((x, i) => (i === 0 ? [x] : [" · ", x]));
  });
  return (
    <Show when={parts().length > 0}>
      <p class="docs-meta">{parts()}</p>
    </Show>
  );
}

// Preprocessor warnings (#warning), as notices.
function CppWarnings(props: { warnings: string[] }): JSX.Element {
  return (
    <For each={props.warnings}>
      {(w) => (
        <div class="docs-notice">
          <p>Preprocessor: {w}</p>
        </div>
      )}
    </For>
  );
}

// A name for the "hidden by Exported only" notice (D-204): an
// instance or family instance reads like "instance Eq (Box a)"
// (readable, Decl.tsx); anything else just has a name.
function anchorLabel(
  d: AnyDecl | undefined,
  fallback: string,
  ix: PageIndex,
): string {
  if (d === undefined) return fallback;
  if ("kind" in d && (d.kind === "instance" || d.kind === "faminst")) {
    return readable(d, ix);
  }
  return d.name ?? fallback;
}

// "Exported only" (D-204): a checkbox, and how many top-level
// declarations it hides, while it's on (instances the rule also hides
// aren't counted here, D-204). The status is mounted (and in the
// accessibility tree) at all times; only its text changes (C4), so a
// screen reader never misses it.
function Toolbar(props: { hidden: number }): JSX.Element {
  const prefs = usePrefs();
  const text = () =>
    prefs.exportedOnly() && props.hidden > 0
      ? `${props.hidden} declaration${props.hidden === 1 ? "" : "s"} hidden`
      : "";
  return (
    <div class="docs-toolbar">
      <label class="docs-toolbar-checkbox">
        <input
          type="checkbox"
          checked={prefs.exportedOnly()}
          onChange={(e) => prefs.setExportedOnly(e.currentTarget.checked)}
        />{" "}
        Exported only
      </label>
      <span class="docs-toolbar-status" aria-live="polite">
        {text()}
      </span>
    </div>
  );
}

function TextOnlyPage(props: {
  page: Page;
  parse: TextOnlyModule;
}): JSX.Element {
  const source = useSourceHref(() => props.page);
  const line = () => reasonLine(props.parse.reason) ?? 1;
  return (
    <>
      <PageTitle text={props.page.ref} />
      <h1 class="docs-title" tabindex="-1">
        {props.page.ref}
      </h1>
      <Meta page={props.page} />
      <div class="docs-notice docs-banner">
        <p>{reasonBanner(props.parse.reason)}</p>
        <Show when={source()}>
          {(href) => (
            <p>
              <a href={href()(line())}>View source</a>
            </p>
          )}
        </Show>
      </div>
      <CppWarnings warnings={props.parse.warnings} />
    </>
  );
}

function ModulePage(props: {
  page: Page;
  parsed: ParsedModule;
  typedRef: string;
}): JSX.Element {
  const location = useLocation();
  const navigate = useNavigate();
  const prefs = usePrefs();
  const base = useResolvedPath(() => "/");
  const ix = createMemo(() => pageIndex(props.parsed));
  const links: Links = {
    decl: (n) => {
      const id = ix().idOfHandle(n);
      return id === undefined
        ? undefined
        : withBase(base(), docsHref(props.typedRef, id));
    },
    module: (ref, anchor) => withBase(base(), docsHref(ref, anchor)),
  };
  const header = () => props.parsed.header;
  const name = () => header().name?.value ?? props.page.ref;
  const ext = () => props.parsed.extensions;
  // Elements for anchors, by scope and handle (no DOM ids from
  // declaration ids).
  const anchors = new Anchors();
  const instancesOpen = openStates();
  const sourceHref = useSourceHref(() => props.page);
  const b = createMemo(() => body(props.parsed, ix(), prefs.exportedOnly()));
  const hidden = createMemo(() => hiddenCount(props.parsed));
  const ctx: PageCtx = {
    get ix() {
      return ix();
    },
    get sourceHref() {
      return sourceHref();
    },
    anchors,
    instancesOpen,
    get exportedOnly() {
      return prefs.exportedOnly();
    },
    get marks() {
      return b().mode !== "B";
    },
  };
  // The element of a declaration. In an instance list if it or a
  // parent is an instance with a home (opened first); else in the main
  // flow. A nested one not drawn: the nearest parent that is.
  const elementOf = (d: AnyDecl): HTMLElement | undefined => {
    const i = ix();
    let home: AnyDecl | undefined;
    for (let x: AnyDecl | undefined = d; x !== undefined; x = i.parentOf(x)) {
      home = b().homes.get(x.n);
      if (home !== undefined) break;
    }
    if (home !== undefined) instancesOpen(home.n)[1](true);
    const near = (scope: string, from: AnyDecl) => {
      for (let x: AnyDecl | undefined = from; x; x = ix().parentOf(x)) {
        const el = anchors.get(anchorKey(scope, x.n));
        if (el !== undefined) return el;
      }
      return undefined;
    };
    return home === undefined
      ? near("", d)
      : (near(`${home.n}/`, d) ?? near("", home));
  };
  // An anchor this page has no declaration for, and one it has but
  // "exported only" hides (a different notice, D-204): kept apart so
  // toggling can clear one without the other.
  const [missing, setMissing] = createSignal<string>();
  const [hiddenAnchor, setHiddenAnchor] = createSignal<string>();
  // moveFocus: false on a toggle (recompute only, D-204): scrolling or
  // moving focus away from what the user was doing would be rude.
  const go = (hash: string, moveFocus: boolean) => {
    const a = anchorFromHash(hash);
    if (a === null) {
      setMissing(undefined);
      setHiddenAnchor(undefined);
      return;
    }
    if (typeof a !== "string") {
      setHiddenAnchor(undefined);
      setMissing(hash.slice(1));
      return;
    }
    const d = ix().byId.get(a);
    if (d !== undefined) {
      const el = elementOf(d);
      if (el === undefined) {
        // Known, but not drawn. Only call it "hidden by Exported
        // only" when that's on (C9): off, it's not why, so the
        // generic notice (D-183) is the honest one.
        if (prefs.exportedOnly()) {
          setMissing(undefined);
          setHiddenAnchor(a);
        } else {
          setHiddenAnchor(undefined);
          setMissing(a);
        }
        return;
      }
      setMissing(undefined);
      setHiddenAnchor(undefined);
      if (moveFocus) {
        el.scrollIntoView({ block: "start" });
        el.focus({ preventScroll: true });
      }
      return;
    }
    // Haddock's anchors (v:x, t:T), as in module links: our id
    // instead, in place (D-182).
    const m = /^([vt]):(.+)$/s.exec(a);
    if (m !== null) {
      const names = m[1] === "v" ? ix().byName.value : ix().byName.type;
      const raw = m[2] ?? "";
      // As written, else with Haddock's escapes decoded.
      const unescaped = unescapeHaddock(raw);
      const found =
        names.get(raw) ??
        (unescaped === undefined ? undefined : names.get(unescaped));
      if (found !== undefined) {
        navigate(docsHref(props.typedRef, ix().idOf(found)), {
          replace: true,
          scroll: false,
        });
        return;
      }
    }
    setHiddenAnchor(undefined);
    setMissing(a);
  };
  // After the router: it scrolls to the top on a hash it has no
  // element for.
  createEffect(
    on(
      () => location.hash,
      (hash) => {
        const frame = requestAnimationFrame(() => go(hash, true));
        onCleanup(() => cancelAnimationFrame(frame));
      },
    ),
  );
  // A search result (any SearchBox, header included) can be the
  // declaration already at the top of the URL: the router ignores a
  // link to its own URL, so nothing above would re-run `go`. Same
  // fix as `inPage` below, for a picker outside this page (C7).
  // eslint-disable-next-line solid/reactivity -- a window event, go() is always current
  onCleanup(onReselectAnchor((hash) => go(hash, true)));
  // Toggling "exported only" can show or hide the current anchor's
  // element: re-resolve, but never move focus or scroll (D-204).
  createEffect(
    on(
      () => prefs.exportedOnly(),
      () => {
        const frame = requestAnimationFrame(() => go(location.hash, false));
        onCleanup(() => cancelAnimationFrame(frame));
      },
      { defer: true },
    ),
  );
  // "Show all": turn the toggle off, then go to the anchor properly.
  // Not a reactive computation (it runs from a click), so no cleanup
  // to register; the frame outlives at most one render.
  const showAll = (hash: string) => {
    prefs.setExportedOnly(false);
    requestAnimationFrame(() => go(hash, true));
  };
  // Links to this page. Two router quirks: it scrolls to the top on a
  // hash it has no element for (one frame, before go), and it ignores
  // a link to the URL it is at, so a second click on a link to the
  // current anchor would do nothing. A native listener runs before the
  // router's one on document: mark the link noscroll, and resolve the
  // anchor again ourselves when the URL won't change.
  const inPage = (event: MouseEvent): void => {
    if (event.button !== 0 || event.metaKey || event.ctrlKey) return;
    if (event.shiftKey || event.altKey) return;
    const a =
      event.target instanceof Element
        ? event.target.closest<HTMLAnchorElement>("a[href]")
        : null;
    if (a === null || a.target !== "") return;
    const url = new URL(a.href, window.location.href);
    if (
      url.origin !== window.location.origin ||
      url.pathname !== window.location.pathname
    ) {
      return;
    }
    a.setAttribute("noscroll", "");
    if (url.hash !== "" && url.hash === window.location.hash) {
      go(url.hash, true);
    }
  };
  return (
    <div class="docs-page" on:click={inPage}>
      <LinksContext.Provider value={links}>
        <PageContext.Provider value={ctx}>
          <PageTitle
            text={
              name() === props.page.ref
                ? name()
                : `${name()} (${props.page.ref})`
            }
          />
          <h1 class="docs-title" tabindex="-1">
            {name()}
          </h1>
          <Show when={missing()} keyed>
            {(a) => (
              <div class="docs-notice decl-missing">
                <p>
                  No declaration <code>{a}</code> in this module, in this
                  snapshot.
                </p>
                <SearchBox initialQuery={anchorName(a)} placement="page" />
              </div>
            )}
          </Show>
          <Show when={hiddenAnchor()} keyed>
            {(a) => {
              const label = anchorLabel(ix().byId.get(a), a, ix());
              return (
                <div class="docs-notice decl-missing">
                  <p>
                    <code>{label}</code> is hidden by Exported only.
                  </p>
                  <button
                    type="button"
                    class="decl-source-toggle"
                    onClick={() => showAll(location.hash)}
                  >
                    Show all
                  </button>
                </div>
              );
            }}
          </Show>
          <Meta page={props.page} />
          <Show when={header().warning}>
            {(w) => <WarningNotice warning={w()} />}
          </Show>
          <CppWarnings warnings={props.parsed.warnings} />
          <Show when={header().haddock}>
            {(h) => <DocView doc={h().doc} level={1} />}
          </Show>
          <p class="docs-meta">
            Language: {ext().language}
            {ext().fromPragmas.length === 0
              ? null
              : `, with ${ext().fromPragmas.join(", ")}`}
          </p>
          <details class="docs-extensions">
            <summary>All extensions in effect ({ext().enabled.length})</summary>
            <ul>
              <For each={ext().enabled}>{(e) => <li>{e}</li>}</For>
            </ul>
          </details>
          <Toolbar hidden={hidden()} />
          <PageBody
            parsed={props.parsed}
            body={b()}
            exportedOnly={prefs.exportedOnly()}
          />
        </PageContext.Provider>
      </LinksContext.Provider>
    </div>
  );
}
