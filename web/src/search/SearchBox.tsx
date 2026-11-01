// The search box: in the header (every page) and reused by the "not
// found" and missing-declaration notices (amends D-183). Server search
// (D-207), a keyboard-usable combobox (D-209). Not a <form>: the CSP
// has form-action 'none'.
import { useLocation, useNavigate, useResolvedPath } from "@solidjs/router";
import {
  createEffect,
  createMemo,
  createSignal,
  createUniqueId,
  For,
  type JSX,
  on,
  Show,
  useContext,
} from "solid-js";
import { docsHref } from "../api/module";
import type { SearchNamespace, SearchResult } from "../api/search";
import { IndexingContext } from "../events/indexing";
import { withBase } from "../files/paths";
import { dispatchReselectAnchor } from "../reselectAnchor";
import { createSearch } from "./createSearch";

// The server's own cap (D-207): a prefilled query (from a URL) skips
// the input's maxlength, so it's capped here too, or a long enough
// "not found" URL would 400 instead of showing "No results."
const maxQueryLength = 128;

function capQuery(q: string): string {
  return [...q].slice(0, maxQueryLength).join("");
}

const kindLabels: Record<SearchNamespace, string> = {
  type: "type",
  class: "class",
  con: "constructor",
  field: "field",
  val: "value",
  pat: "pattern synonym",
};

// "type", "value", "field of Config", "module", … (D-209: from the
// DeclId namespace plus the field's parent).
function kindLabel(r: SearchResult): string {
  if (r.kind === "module") return "module";
  return r.ns === "field" ? `field of ${r.parent ?? ""}` : kindLabels[r.ns];
}

// The module to show next to a result: the decl's own module always;
// for a module result, only when its ref differs from its name (a
// loose module whose path isn't its name).
function moduleLabel(r: SearchResult): string | undefined {
  if (r.kind === "module") return r.ref === r.name ? undefined : r.ref;
  return r.module;
}

function resultHref(r: SearchResult): string {
  return r.kind === "module" ? docsHref(r.ref) : docsHref(r.ref, r.anchor);
}

// A stable key for a result, kept across a partial refresh (D-209):
// not the array index, which can shift as better matches arrive.
function resultKey(r: SearchResult): string {
  return r.kind === "module" ? `m:${r.ref}` : `d:${r.ns}:${r.ref}:${r.anchor}`;
}

// The live count line (C3: always current, never throttled — unlike
// the polite announcement below, which it also feeds the words of).
function statusText(
  query: string,
  matches: number,
  shown: number,
  partial: boolean,
): string {
  if (query === "") return "";
  const head =
    matches === 0
      ? "No results."
      : shown < matches
        ? `Showing ${shown} of ${matches}.`
        : matches === 1
          ? "1 result."
          : `${matches} results.`;
  return partial ? `${head} Partial: still indexing.` : head;
}

export function SearchBox(props: {
  // Prefilled (not-found, missing-declaration notices); else empty.
  initialQuery?: string;
  // "header": a popup, closed until there's text; opens the result
  // and keeps the text. "page": the list is always shown.
  placement: "header" | "page";
}): JSX.Element {
  const base = useResolvedPath(() => "/");
  const navigate = useNavigate();
  const location = useLocation();
  const indexingCtx = useContext(IndexingContext);
  const isHeader = (): boolean => props.placement === "header";
  const id = createUniqueId();
  const listboxId = `${id}-listbox`;

  const [query, setQuery] = createSignal(capQuery(props.initialQuery ?? ""));

  const [popupOpen, setPopupOpen] = createSignal(false);
  // Shown: the header's popup while open, else (page) whenever there's
  // something to search for. The header never opens for an empty
  // query (C5): nothing to show, and no empty strip to show it in.
  const showList = (): boolean => (isHeader() ? popupOpen() : query() !== "");

  // Passed to createSearch as "visible": no point refreshing a partial
  // answer nobody can see.
  const search = createSearch(query, showList);

  let container: HTMLDivElement | undefined;

  // A query whose answer hasn't landed yet (C1): see the Enter case
  // below. Cleared here too, not just by typing/Escape/an error, so a
  // late answer never navigates after the user has already moved on —
  // closePopup runs on a focus-out, a route change and choosing a
  // result, which between them cover leaving by Tab, a click
  // elsewhere (a sidebar link, a stale result), and Enter/choosing
  // itself.
  let pendingEnter: string | undefined;

  const closePopup = (): void => {
    setPopupOpen(false);
    setActiveKey(undefined);
    pendingEnter = undefined;
  };

  const [activeKey, setActiveKey] = createSignal<string>();
  // A new query starts with nothing active, even if the old one had a
  // same-named hit (the plan's "active option kept by key" is about a
  // partial refresh of the *same* query, not a new one).
  createEffect(on(query, () => setActiveKey(undefined)));

  const activeIndex = createMemo<number | undefined>(() => {
    const key = activeKey();
    if (key === undefined) return undefined;
    const i = search.results().findIndex((r) => resultKey(r) === key);
    return i === -1 ? undefined : i;
  });
  const activeId = createMemo<string | undefined>(() => {
    const i = activeIndex();
    return i === undefined ? undefined : `${id}-opt-${i}`;
  });

  // The live count/error line: always the current state, so it never
  // lags behind the list it describes (C3), including under an
  // error, which createSearch tracks but never showed before (C4).
  const liveStatusText = (): string =>
    search.error() !== undefined
      ? "Search failed."
      : statusText(
          query(),
          search.matches(),
          search.results().length,
          search.partial(),
        );

  // The polite live region: announced once a query's own fetch
  // settles (answer or error) and once more on partial -> full, never
  // on every partial refresh in between (that would spam it once a
  // second).
  const [announced, setAnnounced] = createSignal("");
  let announcedQuery: string | undefined;
  let wasPartial = false;
  createEffect(() => {
    const settled = search.settledQuery();
    const p = search.partial();
    if (settled === undefined) return;
    if (settled !== announcedQuery || (wasPartial && !p)) {
      announcedQuery = settled;
      setAnnounced(liveStatusText());
    }
    wasPartial = p;
  });

  // Closes the popup on navigation, by hand (Enter) or otherwise (a
  // link elsewhere, back/forward): never on the initial render.
  createEffect(
    on(
      () => location.pathname + location.search + location.hash,
      () => closePopup(),
      { defer: true },
    ),
  );

  // Enter on a query whose answer hasn't landed yet (still debouncing
  // or in flight): don't act on whatever's on screen, which answers
  // an *older* query (C1). Flush the debounce and wait; cancelled by
  // typing, Escape, leaving the box (closePopup, above), or an error
  // — but only an error that actually belongs to the pending query: a
  // stale one left over from an *earlier* query must not cancel it
  // (settledQuery, not query(), which an error never clears).
  const resolvePending = (): void => {
    if (pendingEnter === undefined) return;
    const q = query();
    if (pendingEnter !== q) {
      pendingEnter = undefined;
      return;
    }
    if (
      search.error() !== undefined &&
      search.settledQuery() === pendingEnter
    ) {
      pendingEnter = undefined;
      return;
    }
    if (search.answeredQuery() !== pendingEnter) return; // still waiting
    const r = search.results()[0];
    pendingEnter = undefined;
    if (r !== undefined) choose(r);
  };
  createEffect(() => {
    query();
    search.answeredQuery();
    search.error();
    resolvePending();
  });

  // The full href (with the token base), for comparing against the
  // current URL.
  const fullHref = (r: SearchResult): string => withBase(base(), resultHref(r));

  // The result resolves to the page already showing: the router
  // ignores a link to its own URL (same as any in-page link), so
  // nothing would happen without this. Re-run the anchor instead, the
  // way a same-page click does via DocsView's `inPage` (C7). A plain
  // window event: this box has no idea which view, if any, can
  // resolve a declaration anchor.
  const reselectIfHere = (full: string): boolean => {
    const here = location.pathname + location.hash;
    if (full !== here) return false;
    const i = full.indexOf("#");
    if (i !== -1) dispatchReselectAnchor(full.slice(i));
    return true;
  };

  // Enter: no click event fires, so navigate by hand. Keeps the text
  // (choice 11): only the popup closes.
  const choose = (r: SearchResult): void => {
    closePopup();
    const full = fullHref(r);
    if (!reselectIfHere(full)) navigate(resultHref(r));
  };

  // Mouse: the native click (and the router's interception of it)
  // already navigates for a different URL; only "already here" needs
  // our help.
  const onOptionClick = (r: SearchResult): void => {
    closePopup();
    reselectIfHere(fullHref(r));
  };

  const onInput: JSX.EventHandler<HTMLInputElement, InputEvent> = (e) => {
    const v = e.currentTarget.value;
    setQuery(v);
    if (isHeader()) setPopupOpen(v !== "");
  };

  const onFocus = (): void => {
    if (isHeader() && query() !== "") setPopupOpen(true);
  };

  const onFocusOut: JSX.EventHandler<HTMLDivElement, FocusEvent> = (e) => {
    const next = e.relatedTarget;
    if (
      container !== undefined &&
      next instanceof Node &&
      container.contains(next)
    ) {
      return;
    }
    closePopup();
  };

  // A mousedown anywhere in the popup — a result link included, not
  // just the padding or the footer — is prevented: focus (and the
  // popup) stays in the input until a real click lands, which
  // preventDefault on mousedown doesn't stop (C8). Kept deliberately:
  // nothing in the popup can be text-selected by dragging either.
  const onPopupMouseDown: JSX.EventHandler<HTMLDivElement, MouseEvent> = (
    e,
  ) => {
    e.preventDefault();
  };

  const onKeyDown: JSX.EventHandler<HTMLInputElement, KeyboardEvent> = (e) => {
    const results = search.results();
    switch (e.key) {
      case "ArrowDown": {
        if (isHeader() && !popupOpen()) {
          if (query() === "") return; // C5: nothing to open onto
          e.preventDefault();
          setPopupOpen(true);
          const r0 = results[0];
          if (r0 !== undefined) setActiveKey(resultKey(r0));
          return;
        }
        e.preventDefault();
        const i = activeIndex();
        const next = i === undefined ? 0 : Math.min(i + 1, results.length - 1);
        const r = results[next];
        if (r !== undefined) setActiveKey(resultKey(r));
        return;
      }
      case "ArrowUp": {
        if (isHeader() && !popupOpen()) return;
        e.preventDefault();
        const i = activeIndex();
        if (i === undefined || i === 0) {
          setActiveKey(undefined);
          return;
        }
        const r = results[i - 1];
        if (r !== undefined) setActiveKey(resultKey(r));
        return;
      }
      case "Enter": {
        // C2: a closed header popup has nothing current to open.
        if (isHeader() && !popupOpen()) return;
        const q = query();
        if (q === "") return;
        e.preventDefault();
        if (search.answeredQuery() === q) {
          const r = results[activeIndex() ?? 0];
          if (r !== undefined) choose(r);
          return;
        }
        // Not yet answered for this exact query (C1): wait for it.
        pendingEnter = q;
        search.flush();
        resolvePending();
        return;
      }
      case "Escape": {
        pendingEnter = undefined;
        if (isHeader() && popupOpen()) {
          e.preventDefault();
          closePopup();
        } else if (query() !== "") {
          e.preventDefault();
          setQuery("");
        }
        return;
      }
      default:
        return;
    }
  };

  const indexingSuffix = (): string => {
    const ix = indexingCtx.indexing();
    if (ix === undefined) return "";
    // Done, but the search's own refetch hasn't landed yet: "(n of n)"
    // next to "still indexing" would be self-contradictory (C10).
    if (ix.ready + ix.failed >= ix.total) return "";
    return ` (${ix.ready + ix.failed} of ${ix.total} modules)`;
  };

  return (
    <div
      class={isHeader() ? "search-box search-box-header" : "search-box"}
      role="search"
      aria-label={isHeader() ? "Search" : "Search modules and names"}
      ref={(el) => (container = el)}
      onFocusOut={onFocusOut}
    >
      <label for={id} class={isHeader() ? "sr-only" : undefined}>
        Search modules and names
      </label>
      <input
        id={id}
        type="search"
        role="combobox"
        placeholder={isHeader() ? "Search" : undefined}
        autocomplete="off"
        spellcheck={false}
        maxlength={maxQueryLength}
        aria-autocomplete="list"
        aria-expanded={showList()}
        aria-controls={showList() ? listboxId : undefined}
        aria-activedescendant={activeId()}
        value={query()}
        onInput={onInput}
        onFocus={onFocus}
        onKeyDown={onKeyDown}
      />
      {/* Throttled: a screen reader hears this, not the live count
          inside the popup (C3), which would spam it once a second
          while partial. */}
      <p class="search-live sr-only" role="status">
        {announced()}
      </p>
      {/* The live count, the list and the partial footer share one
          visibility: nothing to say when there's nothing shown
          (C5). For "page" this isn't really a floating popup
          (search.css scopes the absolute positioning to the header),
          just the one wrapper. */}
      <Show when={showList()}>
        <div class="search-popup" onMouseDown={onPopupMouseDown}>
          <p class="search-status">{liveStatusText()}</p>
          <ul id={listboxId} role="listbox" class="search-results">
            <For each={search.results()}>
              {(r, i) => (
                <li role="presentation">
                  <a
                    id={`${id}-opt-${i()}`}
                    role="option"
                    tabindex="-1"
                    aria-selected={activeIndex() === i()}
                    classList={{
                      "search-result-active": activeIndex() === i(),
                    }}
                    href={withBase(base(), resultHref(r))}
                    // A decl anchor isn't a real DOM id the router can
                    // scroll to; without this it falls back to
                    // scrolling to the top (Firefox: even for the
                    // anchor already shown). Our own reselectIfHere /
                    // DocsView's `go` do the real scrolling. Not on a
                    // module result: that's a plain page, where the
                    // router's own scroll-to-top is the right thing.
                    // (Imperative, like DocsView's inPage: "noscroll"
                    // isn't typed JSX, solid-router just reads the
                    // DOM attribute.)
                    ref={(el) => {
                      if (r.kind === "decl") el.setAttribute("noscroll", "");
                    }}
                    onClick={() => onOptionClick(r)}
                  >
                    <span class="search-result-name">{r.name}</span>
                    <span class="search-result-kind">{kindLabel(r)}</span>
                    <Show when={moduleLabel(r)}>
                      {(m) => <span class="search-result-module">{m()}</span>}
                    </Show>
                    <Show when={r.kind === "decl" && !r.exported}>
                      <span class="search-result-private">private</span>
                    </Show>
                  </a>
                </li>
              )}
            </For>
          </ul>
          <Show when={search.partial()}>
            <p class="search-partial">
              Partial results, still indexing{indexingSuffix()}.
            </p>
          </Show>
        </div>
      </Show>
    </div>
  );
}
