import { useLocation, useResolvedPath } from "@solidjs/router";
import {
  batch,
  createEffect,
  createMemo,
  createResource,
  createSignal,
  For,
  type JSX,
  Match,
  on,
  onCleanup,
  onMount,
  Show,
  Switch,
  untrack,
  useContext,
} from "solid-js";
import { getCbor } from "../api/client";
import { decodeFileView, type FileView as View } from "../api/files";
import { docsHref } from "../api/module";
import {
  decodeSourceView,
  type LexedSource,
  type OutlineEntry,
  sameLexed,
} from "../api/source";
import { IndexingContext } from "../events/indexing";
import { refetchStep } from "../events/partialRefetch";
import { bannerText } from "../modules/notices";
import { useModules } from "../modules/store";
import { NotFound } from "../NotFound";
import { PageTitle } from "../PageTitle";
import { type CodeApi, CodeView, type LineMarks } from "../source/CodeView";
import { highlightDecor, highlightedLine } from "../source/decor";
import { type Highlight, highlightOf } from "../source/highlight";
import { jumpOf, linkInfos } from "../source/links";
import { lineIndex, lineText } from "../source/lines";
import { createDelay, focusInMain, focusLost } from "../ui/focus";
import { bytes, limit } from "../ui/format";
import { bringOnPage } from "../ui/scroll";
import {
  apiFileUrl,
  apiSourceUrl,
  hrefFor,
  lineHref,
  parseLineHash,
  pathFromPathname,
  withBase,
} from "./paths";

// The file view: /<token>/src/tree/<path>.
export function FileView(): JSX.Element {
  const location = useLocation();
  const path = createMemo(() => pathFromPathname(location.pathname));
  return (
    <Show when={path()} keyed fallback={<NotFound />}>
      {(p) => <FilePage path={p} />}
    </Show>
  );
}

function FilePage(props: { path: string }): JSX.Element {
  // Read before the old page goes away.
  const fromMain = focusInMain();
  const [file] = createResource(
    () => props.path,
    (p) => getCbor(apiFileUrl(p), decodeFileView),
  );
  const slow = createDelay(() => file.loading, 300);
  // Generated or preprocessed: say why there's only text. Needs the
  // module tree, but never waits for it.
  const modules = useModules();
  const banner = () => {
    const info = modules.bySource().get(props.path);
    return info === undefined ? undefined : bannerText(info.source);
  };
  // Only the result for this path, never a stale one.
  const result = () => (file.state === "ready" ? file() : undefined);
  const notFound = () => result()?.kind === "not-found";
  const view = (): View | undefined => {
    const r = result();
    return r?.kind === "ok" ? r.value : undefined;
  };
  const lexed = createSource(() => props.path, view);
  // A link in main (like a symlink target) took us here: its page is
  // gone, so put focus on the new heading instead of letting it drop to
  // body. Again if a 404 swaps the heading.
  if (fromMain) {
    createEffect(
      on(notFound, () => {
        if (focusLost()) {
          document.querySelector<HTMLElement>("#main h1")?.focus();
        }
      }),
    );
  }
  return (
    <Show when={!notFound()} fallback={<NotFound />}>
      <PageTitle text={props.path} />
      <h1 class="file-path" tabindex="-1">
        {props.path}
      </h1>
      <Show when={banner()}>{(b) => <p class="file-banner">{b()}</p>}</Show>
      <Switch
        fallback={
          <Show when={slow()}>
            <p>Loading…</p>
          </Show>
        }
      >
        <Match when={view()} keyed>
          {(v) => body(v, props.path, lexed)}
        </Match>
        <Match when={result()?.kind === "error"}>
          <p>Couldn't load this file.</p>
        </Match>
      </Switch>
    </Show>
  );
}

// Highlighting, at the same time as the text. Never waited for: the
// text shows plain first (the server can take a while when indexing).
// Any failure: plain, no message. While the answer is partial, asked
// again on indexing events (D-286): at most once a second, plus once
// when indexing is done. A failed or plain answer keeps the last good
// one; an equal one changes nothing (no redraw). An answer or event
// for another snapshot is dropped (stale until a reload).
function createSource(
  path: () => string,
  view: () => View | undefined,
): () => LexedSource | undefined {
  const indexing = useContext(IndexingContext);
  const [latest, setLatest] = createSignal<LexedSource>();
  const [partial, setPartial] = createSignal(false);
  // Bumped on each fetch: a late answer of an earlier one is dropped.
  let seq = 0;
  let ac: AbortController | undefined;
  let lastAt = 0;
  let answerGen: number | undefined;
  // The refetch at done is done once, until partial changes.
  let doneAsked = false;
  const fetchSource = () => {
    ac?.abort();
    const mine = ++seq;
    const signal = (ac = new AbortController()).signal;
    lastAt = Date.now();
    void getCbor(apiSourceUrl(untrack(path)), decodeSourceView, {
      signal,
    }).then((r) => {
      if (mine !== seq || r.kind !== "ok") return;
      const s = r.value;
      const v = untrack(view);
      if (v !== undefined && s.generation !== v.generation) return;
      answerGen = s.generation;
      if (s.partial !== untrack(partial)) doneAsked = false;
      batch(() => {
        setPartial(s.partial);
        if (s.highlight === "lexed") setLatest(s);
      });
    });
  };
  fetchSource();
  onCleanup(() => {
    seq++;
    ac?.abort();
  });
  createEffect(() => {
    const p = indexing.indexing();
    if (p === undefined || !partial()) return;
    const step = refetchStep(p, answerGen, lastAt, Date.now());
    if (step === "due") fetchSource();
    else if (step === "done" && !doneAsked) {
      doneAsked = true;
      fetchSource();
    }
  });
  // Only a lexed answer for this very text (same generation).
  return createMemo(
    (): LexedSource | undefined => {
      const v = view();
      const s = latest();
      return v !== undefined && s !== undefined && s.generation === v.generation
        ? s
        : undefined;
    },
    undefined,
    { equals: sameLexed },
  );
}

function body(
  v: View,
  path: string,
  source: () => LexedSource | undefined,
): JSX.Element {
  switch (v.kind) {
    case "text":
      return <FileText text={v.text} path={path} source={source()} />;
    case "binary":
      return (
        <p>This file is not UTF-8 text, so it isn't shown ({bytes(v.size)}).</p>
      );
    case "too-large":
      return (
        <p>
          This file is too large to show ({bytes(v.size)}; the limit is{" "}
          {limit(v.limit)}).
        </p>
      );
    case "symlink":
      return <Symlink target={v.target} resolved={v.resolved} />;
    case "submodule":
      return (
        <p>
          {v.commit === null
            ? "Nested git repository."
            : `Submodule at commit ${v.commit}.`}{" "}
          Its files aren't shown.
        </p>
      );
  }
}

function Symlink(props: {
  target: string;
  resolved: string | null;
}): JSX.Element {
  const base = useResolvedPath(() => "/");
  return (
    <p>
      Symbolic link to{" "}
      <Show when={props.resolved} fallback={<code>{props.target}</code>}>
        {(r) => (
          <a href={withBase(base(), hrefFor(r()))}>
            <code>{props.target}</code>
          </a>
        )}
      </Show>
    </p>
  );
}

// The text in the source view. #L<n>[-L<m>] marks those lines and
// scrolls the first one to the middle; again on a hash change, with no
// refetch. Bad or out of range: nothing. An end past the end is cut.
// source: highlighting, when it comes.
function FileText(props: {
  text: string;
  path: string;
  source: LexedSource | undefined;
}): JSX.Element {
  const location = useLocation();
  const base = useResolvedPath(() => "/");
  const index = createMemo(() => lineIndex(props.text));
  const hl = createMemo((): Highlight | undefined => {
    const s = props.source;
    if (s === undefined) return undefined;
    const links = linkInfos(
      { path: props.path, ref: s.ref },
      s.targets,
      base(),
    );
    return highlightOf(s, index().count, links);
  });
  const line = (n: number) => withBase(base(), lineHref(props.path, n));
  // The outline, if it has entries.
  const outline = () => {
    const o = hl()?.outline;
    return o === undefined || o.length === 0 ? undefined : o;
  };
  const docs = (id: string): string | undefined => {
    const ref = props.source?.ref ?? null;
    return ref === null ? undefined : withBase(base(), docsHref(ref, id));
  };
  // New functions when the highlight comes: the rows redraw.
  const renderLine = createMemo(() => {
    const h = hl();
    return h === undefined ? undefined : highlightedLine(h);
  });
  const lineDecor = createMemo(() => {
    const h = hl();
    return h === undefined ? undefined : highlightDecor(h, docs);
  });
  const marks = createMemo((): LineMarks | undefined => {
    const r = parseLineHash(location.hash);
    const n = index().count;
    if (r === undefined || r.from > n) return undefined;
    return { from: r.from, to: Math.min(r.to, n) };
  });
  // Rows off screen aren't in the DOM: say what is marked.
  const hint = () => {
    const m = marks();
    if (m === undefined) return "";
    return m.to === m.from
      ? `Line ${m.from} marked.`
      : `Lines ${m.from}–${m.to} marked.`;
  };
  let api: CodeApi | undefined;
  // A same-file link was followed: where the cursor goes, once the
  // hash changes (D-284).
  let jump: { line: number; col: number } | undefined;
  const onLinkClick = (e: MouseEvent) => {
    const a =
      e.target instanceof Element
        ? e.target.closest<HTMLAnchorElement>("a.code-link")
        : null;
    const s = props.source;
    const h = hl();
    if (a === null || s === undefined || h === undefined) return;
    if (e.defaultPrevented || e.button !== 0) return;
    if (e.ctrlKey || e.metaKey || e.altKey || e.shiftKey) return;
    const k = Number(a.dataset.link);
    if (h.links[k - 1]?.here !== true) return;
    const text = props.text;
    const idx = index();
    const j = jumpOf(k, s.targets, h.runs, (n) => lineText(text, idx, n));
    if (j === undefined) return;
    if (a.hash === location.hash) {
      // Same URL: no navigation, so move here.
      e.preventDefault();
      api?.focusAt(j.line, j.col);
      api?.scrollToLine(j.line, "center");
    } else {
      jump = j;
    }
  };
  // Made before the effect below: its effects (the virtualiser's set
  // up) must run first.
  const view = (
    <Show when={index().count > 0} fallback={<p>This file is empty.</p>}>
      <CodeView
        text={props.text}
        index={index()}
        marks={marks()}
        renderLine={renderLine()}
        lineDecor={lineDecor()}
        ref={(a) => (api = a)}
      />
    </Show>
  );
  createEffect(
    on(marks, (m) => {
      const a = api;
      if (m === undefined || a === undefined) return;
      // The cursor goes there too, so Tab lands on the line. Focus
      // stays in the view if it was there (its old row may go away).
      const j = jump;
      jump = undefined;
      // A link of another file took us here: focus the line.
      const state: unknown = untrack(() => location.state);
      const codeJump =
        typeof state === "object" &&
        state !== null &&
        "codeJump" in state &&
        state.codeJump === true;
      if (j !== undefined && j.line === m.from && a.hasFocus()) {
        a.focusAt(j.line, j.col);
      } else if (a.hasFocus() || codeJump) {
        a.focusLine(m.from);
      } else a.setCursor(m.from);
      a.scrollToLine(m.from, "center");
      // Then the page, if the view is off screen. After the router's
      // own scroll to the top.
      const frame = requestAnimationFrame(() => {
        const r = a.row(m.from);
        if (r !== undefined) bringOnPage(r);
      });
      onCleanup(() => cancelAnimationFrame(frame));
    }),
  );
  let wrap!: HTMLDivElement;
  onMount(() => {
    // Native: before the router's click handler on the document.
    wrap.addEventListener("click", onLinkClick);
    onCleanup(() => wrap.removeEventListener("click", onLinkClick));
  });
  // file-text: a test hook, no style.
  return (
    <div class="file-text" ref={(el) => (wrap = el)}>
      <p class="sr-only" role="status">
        {hint()}
      </p>
      {view}
      {/* After the view: it comes late, and must not push the view. */}
      <Show when={outline()} keyed>
        {(entries) => <Outline entries={entries} docs={docs} line={line} />}
      </Show>
    </div>
  );
}

// The outline as a list, for the keyboard: the gutter markers are not
// Tab stops (one stop in the view, G7). Each entry: its docs, its line.
function Outline(props: {
  entries: readonly OutlineEntry[];
  docs: (id: string) => string | undefined;
  // The href of line n.
  line: (n: number) => string;
}): JSX.Element {
  return (
    <details class="code-outline">
      <summary>Outline ({props.entries.length})</summary>
      <nav aria-label="Outline">
        <ul>
          <For each={props.entries}>
            {(e) => (
              <li>
                <Show when={props.docs(e[1])} fallback={e[2]}>
                  {(href) => <a href={href()}>{e[2]}</a>}
                </Show>{" "}
                <a href={props.line(e[3])}>line {e[3]}</a>
              </li>
            )}
          </For>
        </ul>
      </nav>
    </details>
  );
}
