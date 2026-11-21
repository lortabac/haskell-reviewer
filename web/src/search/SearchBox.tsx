// The search box: in the header (every page) and reused by the "not
// found" and missing-declaration notices (amends D-183). Server search
// (D-207), a keyboard-usable combobox (D-209). Text in double quotes
// searches file contents instead (D-307, D-309). Not a <form>: the CSP
// has form-action 'none'.
import { useLocation, useNavigate, useResolvedPath } from "@solidjs/router";
import {
  createEffect,
  createMemo,
  createSignal,
  createUniqueId,
  For,
  type JSX,
  Match,
  on,
  Show,
  Switch,
  useContext,
} from "solid-js";
import { getJson } from "../api/client";
import { docsHref } from "../api/module";
import type { SearchNamespace, SearchResult } from "../api/search";
import {
  decodeTextSearch,
  type TextHit,
  type TextSearch,
  textSearchUrl,
} from "../api/textSearch";
import { IndexingContext } from "../events/indexing";
import { lineHref, withBase } from "../files/paths";
import { dispatchReselectAnchor } from "../reselectAnchor";
import { usePageSide } from "../session";
import type { Side } from "../sides";
import { hitKey, hitState } from "../textsearch/snippet";
import { SnippetText } from "../textsearch/SnippetText";
import { textSearchHref } from "../textsearch/urls";
import { createFetch } from "./createFetch";
import { createSearch } from "./createSearch";
import { parseBoxQuery, quoted, textTooLong, textTooLongText } from "./query";

// The server's own cap (D-207): a prefilled query (from a URL) skips
// the input's maxlength, so it's capped here too, or a long enough
// "not found" URL would 400 instead of showing "No results."
const maxQueryLength = 128;

// Text hits in the popup; the rest is on the results page.
const textRows = 8;

// Code points shown before the first hit in a text row.
const textLead = 20;

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

function resultHref(side: Side, r: SearchResult): string {
  return r.kind === "module"
    ? docsHref(side, r.ref)
    : docsHref(side, r.ref, r.anchor);
}

// A stable key for a result, kept across a partial refresh (D-209):
// not the array index, which can shift as better matches arrive.
function resultKey(r: SearchResult): string {
  return r.kind === "module" ? `m:${r.ref}` : `d:${r.ns}:${r.ref}:${r.anchor}`;
}

// One row of the list (D-309). Keys: "t:" the "Search text for" row,
// "h:<path>:<line>" a text hit, "all:" the link to the results page.
type Option =
  | { kind: "name"; key: string; r: SearchResult }
  | { kind: "text"; key: string; text: string }
  | { kind: "hit"; key: string; h: TextHit }
  | { kind: "all"; key: string; text: string; total: number };

function optionHref(side: Side, o: Option): string {
  switch (o.kind) {
    case "name":
      return resultHref(side, o.r);
    case "text":
    case "all":
      return textSearchHref(side, o.text);
    case "hit":
      return lineHref(side, o.h.path, o.h.line);
  }
}

const count = (n: number): string => n.toLocaleString("en-US");

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

// The same for a text search answer.
function textStatus(t: TextSearch): string {
  const shown = Math.min(t.results.length, textRows);
  const head =
    t.total === 0
      ? "No matches."
      : shown < t.total
        ? `Showing ${shown} of ${count(t.total)} matching lines.`
        : t.total === 1
          ? "1 matching line."
          : `${count(t.total)} matching lines.`;
  return t.stopped ? `${head} Search stopped early: incomplete.` : head;
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
  // The page's side, or the default one on pages with none (G15).
  const side = usePageSide();
  const isHeader = (): boolean => props.placement === "header";
  const id = createUniqueId();
  const listboxId = `${id}-listbox`;
  const hintId = `${id}-hint`;

  const [query, setQuery] = createSignal(capQuery(props.initialQuery ?? ""));
  const mode = createMemo(() => parseBoxQuery(query()), undefined, {
    equals: (a, b) => a.kind === b.kind && a.text === b.text,
  });

  const [popupOpen, setPopupOpen] = createSignal(false);
  // Shown: the header's popup while open, else (page) whenever there's
  // something to search for. The header never opens for an empty
  // query (C5): nothing to show, and no empty strip to show it in.
  const showList = (): boolean => (isHeader() ? popupOpen() : query() !== "");

  // Names: only in name mode. Passed showList as "visible": no point
  // refreshing a partial answer nobody can see.
  const nameQuery = (): string => (mode().kind === "name" ? query() : "");
  const search = createSearch(nameQuery, showList);
  // Text: only once the quote is closed, and not past the server's
  // limit (it would 400).
  const tooLong = (): boolean => {
    const m = mode();
    return m.kind === "text" && textTooLong(m.text);
  };
  const textQuery = (): string => {
    const m = mode();
    return m.kind === "text" && !tooLong() ? m.text : "";
  };
  const text = createFetch(
    textQuery,
    side,
    (q, s, signal) =>
      getJson(textSearchUrl(s, q, textRows), decodeTextSearch, { signal }),
    (v) => v.query,
  );

  const options = createMemo((): Option[] => {
    const m = mode();
    const textRow = (t: string): Option[] =>
      t === "" ? [] : [{ kind: "text", key: "t:", text: t }];
    if (m.kind === "name") {
      const names: Option[] = search
        .results()
        .map((r) => ({ kind: "name", key: resultKey(r), r }));
      // The text row joins once this query's names have landed (or
      // failed): until then the list is the old answer, and a quick
      // ArrowDown + Enter must not pick the text row by surprise.
      if (search.settledQuery() !== query()) return names;
      return [...names, ...textRow(m.text)];
    }
    if (m.kind === "open") return textRow(m.text);
    const v = text.value();
    if (m.text === "" || tooLong() || v === undefined) return [];
    const hits: Option[] = v.results
      .slice(0, textRows)
      .map((h) => ({ kind: "hit", key: `h:${hitKey(h)}`, h }));
    if (v.total === 0) return hits;
    return [
      ...hits,
      { kind: "all", key: "all:", text: v.query, total: v.total },
    ];
  });

  let container: HTMLDivElement | undefined;

  // A query whose answer hasn't landed yet (C1): see the Enter case
  // below. Cleared here too, not just by typing/Escape/an error, so a
  // late answer never navigates after the user has already moved on —
  // closePopup runs on a focus-out, a route change and choosing a
  // result, which between them cover leaving by Tab, a click
  // elsewhere (a sidebar link, a stale result), and Enter/choosing
  // itself. Name mode only: text mode never waits.
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
    const i = options().findIndex((o) => o.key === key);
    return i === -1 ? undefined : i;
  });
  const activeId = createMemo<string | undefined>(() => {
    const i = activeIndex();
    return i === undefined ? undefined : `${id}-opt-${i}`;
  });

  // The live count/error line: always the current state, so it never
  // lags behind the list it describes (C3), including under an
  // error, which createSearch tracks but never showed before (C4).
  const liveStatusText = (): string => {
    const m = mode();
    if (m.kind === "name") {
      // The list is no name answer at all (a text mode was just left):
      // nothing to count yet. Else the count of what is shown (D-209).
      const q = query();
      const settled = search.settledQuery();
      if (q !== "" && (settled === undefined || settled === "")) {
        return "Searching…";
      }
      return search.error() !== undefined
        ? "Search failed."
        : statusText(
            q,
            search.matches(),
            search.results().length,
            search.partial(),
          );
    }
    if (m.text === "") return "Type text between the quotes.";
    if (m.kind === "open") return "Close the quote to search text.";
    if (tooLong()) return textTooLongText;
    // An error counts only for the text it was for.
    if (text.error() !== undefined && text.settledQuery() === m.text) {
      return "Search failed.";
    }
    const v = text.value();
    if (v === undefined || text.answeredQuery() !== m.text) {
      return "Searching…";
    }
    return textStatus(v);
  };

  // The polite live region: announced once a query's own fetch
  // settles (answer or error) and once more on partial -> full, never
  // on every partial refresh in between (that would spam it once a
  // second). In text modes: once per hint, and once per text query
  // when its own answer lands. Text keys start with a NUL, so they
  // never equal a name query.
  const [announced, setAnnounced] = createSignal("");
  let announcedQuery: string | undefined;
  let wasPartial = false;
  createEffect(() => {
    const m = mode();
    if (m.kind === "name") {
      const settled = search.settledQuery();
      const p = search.partial();
      // Only this query's own answer (C1-like: after a text mode the
      // name search has settled for "", not for this).
      if (settled === undefined || settled !== query()) return;
      if (settled !== announcedQuery || (wasPartial && !p)) {
        announcedQuery = settled;
        setAnnounced(liveStatusText());
      }
      wasPartial = p;
      return;
    }
    const k =
      m.text === ""
        ? "\0hint"
        : m.kind === "open"
          ? "\0open"
          : tooLong()
            ? "\0long"
            : text.settledQuery() === m.text
              ? `\0t:${m.text}`
              : undefined;
    if (k === undefined || k === announcedQuery) return;
    announcedQuery = k;
    setAnnounced(liveStatusText());
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
    // The first name, or the "Search text for" row if none.
    const o = options()[0];
    pendingEnter = undefined;
    if (o !== undefined) choose(o);
  };
  createEffect(() => {
    query();
    search.answeredQuery();
    search.error();
    resolvePending();
  });

  // The full href (with the token base), for comparing against the
  // current URL.
  const fullHref = (o: Option): string =>
    withBase(base(), optionHref(side(), o));

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

  // "Search text for 'foo'": the box becomes "foo" in quotes and shows
  // text hits (D-309). The popup stays open.
  const pickText = (t: string): void => {
    pendingEnter = undefined;
    // No keystroke to wait for: the next text query goes at once (from
    // a handler or from an effect alike).
    text.atOnce();
    setQuery(quoted(t));
    if (isHeader()) setPopupOpen(true);
  };

  // Enter: no click event fires, so navigate by hand. Keeps the text
  // (choice 11): only the popup closes.
  const choose = (o: Option): void => {
    if (o.kind === "text") {
      pickText(o.text);
      return;
    }
    closePopup();
    if (o.kind === "hit") {
      navigate(optionHref(side(), o), { state: hitState(o.h) });
      return;
    }
    if (!reselectIfHere(fullHref(o))) navigate(optionHref(side(), o));
  };

  // Mouse: the native click (and the router's interception of it)
  // already navigates for a different URL; only "already here" needs
  // our help. The text row is a link to the results page only for a
  // new tab; a plain click fills the box.
  const onOptionClick = (o: Option, e: MouseEvent): void => {
    if (o.kind === "text") {
      if (e.button !== 0 || e.ctrlKey || e.metaKey || e.shiftKey) return;
      e.preventDefault();
      pickText(o.text);
      return;
    }
    closePopup();
    if (o.kind === "name") reselectIfHere(fullHref(o));
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
    const opts = options();
    switch (e.key) {
      case "ArrowDown": {
        if (isHeader() && !popupOpen()) {
          if (query() === "") return; // C5: nothing to open onto
          e.preventDefault();
          setPopupOpen(true);
          const o0 = opts[0];
          if (o0 !== undefined) setActiveKey(o0.key);
          return;
        }
        e.preventDefault();
        const i = activeIndex();
        const next = i === undefined ? 0 : Math.min(i + 1, opts.length - 1);
        const o = opts[next];
        if (o !== undefined) setActiveKey(o.key);
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
        const o = opts[i - 1];
        if (o !== undefined) setActiveKey(o.key);
        return;
      }
      case "Enter": {
        // C2: a closed header popup has nothing current to open.
        if (isHeader() && !popupOpen()) return;
        const q = query();
        if (q === "") return;
        e.preventDefault();
        const m = mode();
        const i = activeIndex();
        const active = i === undefined ? undefined : opts[i];
        // The text row always fits the current text: no waiting.
        if (active?.kind === "text") {
          choose(active);
          return;
        }
        if (m.kind === "open") {
          if (m.text !== "") pickText(m.text);
          return;
        }
        if (m.kind === "text") {
          if (m.text === "") return;
          // An option of this query's own answer, else the results
          // page, which needs no answer (D-309).
          if (active !== undefined && text.answeredQuery() === m.text) {
            choose(active);
          } else {
            closePopup();
            navigate(textSearchHref(side(), m.text));
          }
          return;
        }
        if (search.answeredQuery() === q) {
          const o = opts[i ?? 0];
          if (o !== undefined) choose(o);
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
    const ix = indexingCtx.of(side());
    if (ix === undefined) return "";
    // Done, but the search's own refetch hasn't landed yet: "(n of n)"
    // next to "still indexing" would be self-contradictory (C10).
    if (ix.ready + ix.failed >= ix.total) return "";
    return ` (${ix.ready + ix.failed} of ${ix.total} modules)`;
  };

  const label = "Search modules and names, or text in double quotes";

  return (
    <div
      class={isHeader() ? "search-box search-box-header" : "search-box"}
      role="search"
      aria-label={isHeader() ? "Search" : label}
      ref={(el) => (container = el)}
      onFocusOut={onFocusOut}
    >
      <label for={id} class={isHeader() ? "sr-only" : undefined}>
        {label}
      </label>
      <input
        id={id}
        type="search"
        role="combobox"
        placeholder={isHeader() ? 'Search, or "text"' : undefined}
        autocomplete="off"
        spellcheck={false}
        maxlength={maxQueryLength}
        aria-autocomplete="list"
        aria-expanded={showList()}
        aria-controls={showList() ? listboxId : undefined}
        aria-activedescendant={activeId()}
        aria-describedby={hintId}
        value={query()}
        onInput={onInput}
        onFocus={onFocus}
        onKeyDown={onKeyDown}
      />
      <p id={hintId} class={isHeader() ? "sr-only" : "search-hint"}>
        Put text in double quotes, like "where", to search file contents for
        that exact text.
      </p>
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
            <For each={options()}>
              {(o, i) => (
                <li role="presentation">
                  <a
                    id={`${id}-opt-${i()}`}
                    role="option"
                    tabindex="-1"
                    data-kind={o.kind}
                    aria-selected={activeIndex() === i()}
                    classList={{
                      "search-result-active": activeIndex() === i(),
                      "search-hit": o.kind === "hit",
                    }}
                    href={fullHref(o)}
                    // A decl anchor isn't a real DOM id the router can
                    // scroll to; without this it falls back to
                    // scrolling to the top (Firefox: even for the
                    // anchor already shown). Our own reselectIfHere /
                    // DocsView's `go` do the real scrolling. Not on a
                    // module result: that's a plain page, where the
                    // router's own scroll-to-top is the right thing.
                    // (Imperative, like DocsView's inPage: "noscroll"
                    // isn't typed JSX, solid-router just reads the
                    // DOM attribute.) A text hit carries where to land
                    // in its "state" attribute, which the router reads
                    // the same way.
                    ref={(el) => {
                      if (o.kind === "name" && o.r.kind === "decl") {
                        el.setAttribute("noscroll", "");
                      }
                      if (o.kind === "hit") {
                        el.setAttribute("state", JSON.stringify(hitState(o.h)));
                      }
                    }}
                    onClick={(e) => onOptionClick(o, e)}
                  >
                    <OptionBody o={o} />
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

function OptionBody(props: { o: Option }): JSX.Element {
  return (
    <Switch>
      <Match when={props.o.kind === "name" && props.o.r} keyed>
        {(r) => (
          <>
            <span class="search-result-name">{r.name}</span>
            <span class="search-result-kind">{kindLabel(r)}</span>
            <Show when={moduleLabel(r)}>
              {(m) => <span class="search-result-module">{m()}</span>}
            </Show>
            <Show when={r.kind === "decl" && !r.exported}>
              <span class="search-result-private">private</span>
            </Show>
          </>
        )}
      </Match>
      <Match when={props.o.kind === "text" && props.o.text} keyed>
        {(t) => (
          <span>
            Search text for '<span class="search-result-name">{t}</span>'
          </span>
        )}
      </Match>
      <Match when={props.o.kind === "hit" && props.o.h} keyed>
        {(h) => <HitBody h={h} />}
      </Match>
      <Match when={props.o.kind === "all" && props.o.total} keyed>
        {(n) => <span>All results ({count(n)})</span>}
      </Match>
    </Switch>
  );
}

function HitBody(props: { h: TextHit }): JSX.Element {
  return (
    <>
      <span class="search-hit-place">
        {props.h.path}:{props.h.line}
      </span>
      <span class="search-hit-line">
        <SnippetText h={props.h} lead={textLead} />
      </span>
    </>
  );
}
