// The text search page: /<token>/search/<side>?q=<text> (D-310). The
// lines that have the text, in groups by file, a link to each line.
import { A, useLocation } from "@solidjs/router";
import {
  createEffect,
  createMemo,
  createResource,
  createUniqueId,
  For,
  type JSX,
  Match,
  on,
  onCleanup,
  Show,
  Switch,
  untrack,
} from "solid-js";
import { getJson } from "../api/client";
import {
  decodeTextSearch,
  type TextHit,
  textSearchUrl,
} from "../api/textSearch";
import { CopyButton } from "../copy/CopyButton";
import { lastFocus } from "../copy/lastFocus";
import { location as locationText } from "../copy/names";
import { hrefFor, lineHref } from "../files/paths";
import { useCopyLocation, useSideTarget } from "../keys/KeysProvider";
import { NotFound } from "../NotFound";
import { PageTitle } from "../PageTitle";
import { textTooLong, textTooLongText } from "../search/query";
import { onSide, usePageSide } from "../session";
import { otherSide } from "../sides";
import { createDelay } from "../ui/focus";
import {
  groupHits,
  type HitGroup,
  hitKey,
  hitState,
  skippedText,
  stoppedText,
  textSummary,
} from "./snippet";
import { SnippetText } from "./SnippetText";
import { textQueryFromLocation } from "./urls";

// The history entry of the page says which row was followed from it
// (state.textFollowed), so Back focuses that row again. From the
// router's state, which is the new entry's on a push and on Back alike
// (window.history.state is still the old one while a push renders).
function followedKey(state: unknown): string | undefined {
  if (typeof state !== "object" || state === null) return undefined;
  const k = (state as { textFollowed?: unknown }).textFollowed;
  return typeof k === "string" ? k : undefined;
}

export function TextSearchView(): JSX.Element {
  const location = useLocation();
  const q = createMemo(() => textQueryFromLocation(location.search));
  return (
    <Show
      when={q()}
      keyed
      fallback={<NotFound note="No text to search for." query="" />}
    >
      {(text) => <TextSearchPage q={text} />}
    </Show>
  );
}

function TextSearchPage(props: { q: string }): JSX.Element {
  const side = usePageSide();
  const location = useLocation();
  // Set when we come back to this entry (Back, Forward, reload).
  const followed = followedKey(untrack(() => location.state));
  // The server would say 400: say why here, with no request.
  const long = textTooLong(untrack(() => props.q));
  const [answer] = createResource(
    () => (long ? undefined : props.q),
    (q) => getJson(textSearchUrl(side(), q), decodeTextSearch),
  );
  const result = () => (answer.state === "ready" ? answer() : undefined);
  const value = () => {
    const r = result();
    return r?.kind === "ok" ? r.value : undefined;
  };
  const failed = () => {
    const r = result();
    return r !== undefined && r.kind !== "ok";
  };
  const groups = createMemo(() => groupHits(value()?.results ?? []));
  const slow = createDelay(() => answer.loading, 300);
  // Once, when it has loaded.
  const said = () => {
    const v = value();
    if (long) return textTooLongText;
    if (failed()) return "Couldn't search.";
    if (v === undefined) return "";
    return v.stopped ? `${textSummary(v)} ${stoppedText}` : textSummary(v);
  };
  let list: HTMLDivElement | undefined;

  let h1: HTMLHeadingElement | undefined;
  // Back from a followed row: focus it again; if it is gone, the h1
  // (else the h1 right away, below).
  createEffect(
    on(
      () => value() !== undefined || failed(),
      (done) => {
        if (!done || followed === undefined) return;
        const frame = requestAnimationFrame(() => {
          const a = [
            ...document.querySelectorAll<HTMLElement>("[data-hit-key]"),
          ].find((e) => e.dataset.hitKey === followed);
          if (a === undefined) {
            h1?.focus();
            return;
          }
          a.focus();
          a.scrollIntoView({ block: "center" });
        });
        onCleanup(() => cancelAnimationFrame(frame));
      },
    ),
  );

  const focus = lastFocus(() => list, "[data-hit-key]");
  // b: the same search on the other side (D-349). Never off.
  useSideTarget(() => {
    const s = side();
    if (s === "tree") return undefined;
    const o = otherSide(s);
    const top = onSide(location.pathname, location.search, location.hash, o);
    return { kind: "go", side: o, top };
  });
  useCopyLocation(
    () => focus.get()?.dataset.copy ?? { say: "No location here." },
  );

  const title = () => `Text search: "${props.q}"`;
  return (
    <>
      <PageTitle text={title()} />
      <h1
        // Arrival: focus the h1, so the results come next.
        ref={(el) => {
          h1 = el;
          queueMicrotask(() => {
            if (followed === undefined && el.isConnected) el.focus();
          });
        }}
        class="refs-title text-search-title"
        tabindex="-1"
      >
        Text search: <code class="text-search-q">"{props.q}"</code>
      </h1>
      <Switch>
        <Match when={long}>
          <p>{textTooLongText}</p>
        </Match>
        <Match when={failed()}>
          <p>Couldn't search.</p>
        </Match>
        <Match when={value()} keyed>
          {(v) => (
            <>
              <p class="refs-summary">{textSummary(v)}</p>
              <Show when={v.stopped}>
                <p class="refs-summary">{stoppedText}</p>
              </Show>
              <Show when={skippedText(v.skipped)}>
                {(t) => <p class="refs-tier">{t()}</p>}
              </Show>
            </>
          )}
        </Match>
        <Match when={slow()}>
          <p>Loading…</p>
        </Match>
      </Switch>
      <p class="sr-only" role="status">
        {said()}
      </p>
      <div ref={(el) => (list = el)} class="refs-list">
        <For each={groups()}>{(g) => <GroupView g={g} />}</For>
      </div>
    </>
  );
}

function GroupView(props: { g: HitGroup }): JSX.Element {
  const side = usePageSide();
  const id = createUniqueId();
  const n = () => props.g.hits.length;
  return (
    <section class="refs-group" aria-labelledby={id}>
      <div class="copy-host">
        <CopyButton
          text={() => props.g.path}
          label={`Copy path ${props.g.path}`}
        />
        <h2 id={id} class="refs-group-head text-search-path">
          <A href={hrefFor(side(), props.g.path)} data-copy={props.g.path}>
            {props.g.path}
          </A>
        </h2>
      </div>
      <p class="refs-group-meta">
        <span class="refs-count">
          {n()} {n() === 1 ? "matching line" : "matching lines"}
        </span>
      </p>
      <ol class="refs-items">
        <For each={props.g.hits}>{(h) => <HitView h={h} />}</For>
      </ol>
    </section>
  );
}

function HitView(props: { h: TextHit }): JSX.Element {
  const side = usePageSide();
  return (
    <li class="refs-item">
      <A
        href={lineHref(side(), props.h.path, props.h.line)}
        state={hitState(props.h)}
        data-hit-key={hitKey(props.h)}
        data-copy={locationText(props.h.path, props.h.line)}
        on:click={(e: MouseEvent) => {
          if (e.button !== 0 || e.ctrlKey || e.metaKey) return;
          if (e.shiftKey || e.altKey) return;
          // Native: before the router's click handler, which pushes
          // the next entry.
          window.history.replaceState(
            {
              ...(window.history.state as Record<string, unknown> | null),
              textFollowed: hitKey(props.h),
            },
            "",
          );
        }}
      >
        <span class="refs-line">
          <span class="sr-only">Line </span>
          {props.h.line}
        </span>{" "}
        <code class="refs-snippet">
          <SnippetText h={props.h} />
        </code>
      </A>
    </li>
  );
}
