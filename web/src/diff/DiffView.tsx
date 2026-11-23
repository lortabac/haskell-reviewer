// The diff view: /<token>/diff/<path>[?ws=ignore][#<side>-L<n>]
// (D-341). One file, base against head, unified. Data: api/diff, then
// each side's text (api/file) and highlighting (api/source).
import { useLocation, useResolvedPath } from "@solidjs/router";
import {
  createEffect,
  createMemo,
  createResource,
  createSignal,
  type JSX,
  Match,
  on,
  onCleanup,
  onMount,
  Show,
  Switch,
  untrack,
} from "solid-js";
import { getCbor } from "../api/client";
import {
  decodeDiffView,
  type DiffSide,
  type DiffView as View,
  diffUrl,
} from "../api/diff";
import { decodeFileView } from "../api/files";
import type { LexedSource } from "../api/source";
import { CopyButton } from "../copy/CopyButton";
import { apiFileUrl } from "../files/paths";
import { NotFound } from "../NotFound";
import { PageTitle } from "../PageTitle";
import { useSession } from "../session";
import type { DiffSessionSide } from "../sides";
import { createSource } from "../source/createSource";
import { highlightedLine } from "../source/decor";
import { type Highlight, highlightOf } from "../source/highlight";
import { isCodeJump } from "../source/landing";
import { jumpOf, linkInfos } from "../source/links";
import { lineIndex, lineText } from "../source/lines";
import { createDelay } from "../ui/focus";
import { bringOnPage } from "../ui/scroll";
import { type DiffApi, DiffGrid, type SideText } from "./DiffGrid";
import { countsMatch, deleteRow, type Rows, rowsOf } from "./rows";
import {
  diffHref,
  diffPathFromPathname,
  parseDiffHash,
  wsFromSearch,
} from "./urls";

export const texts = {
  notDiff: "Not in diff mode.",
  notFound: "No file at this path on either side.",
  failed: "Couldn't load this diff. See the terminal for details.",
  mismatch: "Couldn't show this diff.",
  noLines: "No line diff for this file.",
  empty: "Both sides are empty.",
  emptyAdded: "Empty file added.",
  emptyRemoved: "Empty file removed.",
  hint: "Browser find sees only the lines on screen.",
};

export function DiffView(): JSX.Element {
  const session = useSession();
  const location = useLocation();
  const path = createMemo(() => diffPathFromPathname(location.pathname));
  // No fetch in browse mode.
  return (
    <Show
      when={session.mode === "diff"}
      fallback={<NotFound note={texts.notDiff} />}
    >
      <Show when={path()} keyed fallback={<NotFound />}>
        {(p) => <DiffPage path={p} />}
      </Show>
    </Show>
  );
}

// What a side's text needs: a fetch, a fixed text, or none (no text).
type Need =
  | { kind: "fetch"; path: string }
  | { kind: "fixed"; text: string }
  | { kind: "none" };

function needOf(side: DiffSide, path: string): Need {
  switch (side.kind) {
    case "text":
      return { kind: "fetch", path };
    case "absent":
      return { kind: "fixed", text: "" };
    // Its target, as one line (no fetch).
    case "symlink":
      return { kind: "fixed", text: side.target };
    case "binary":
    case "too-large":
    case "submodule":
    case "not-local":
      return { kind: "none" };
  }
}

// A side's text once known; "error" when the fetch failed or the file
// isn't text any more. generation: api/file's, for a fetched text.
type Loaded =
  | { kind: "ok"; text: string; generation: number | undefined }
  | { kind: "error" }
  | undefined;

// One side's data: its text, highlighting and path.
interface SideData {
  loaded: () => Loaded;
  text: () => SideText | undefined;
  lexed: () => LexedSource | undefined;
  // The fetched path; undefined: no fetch (absent, symlink, no text).
  path: () => string | undefined;
}

// One side's text. Kept for the page's life: a ws change only fetches
// the diff again. wanted: the generation the text should have (head's:
// the diff's); an older one is fetched again, once per generation.
function createSideData(
  side: DiffSessionSide,
  need: () => Need | undefined,
  wanted: () => number | undefined = () => undefined,
): SideData {
  // Kept while a new diff loads (a ws change): no refetch.
  const fetchPath = createMemo((last: string | undefined) => {
    const n = need();
    if (n === undefined) return last;
    return n.kind === "fetch" ? n.path : undefined;
  }, undefined);
  const [file, { refetch }] = createResource(fetchPath, (p) =>
    getCbor(apiFileUrl(side, p), decodeFileView),
  );
  // The generation a refetch was made for.
  const [askedFor, setAskedFor] = createSignal<number>();
  // Older than wanted and not asked again yet: ask (once).
  const stale = createMemo(() => {
    const w = wanted();
    if (w === undefined || file.state !== "ready") return false;
    const r = file();
    return r.kind === "ok" && r.value.generation < w && askedFor() !== w;
  });
  createEffect(() => {
    if (!stale()) return;
    setAskedFor(untrack(wanted));
    void refetch();
  });
  const loaded = createMemo((): Loaded => {
    const n = need();
    if (n === undefined || n.kind === "none") return undefined;
    if (n.kind === "fixed") {
      return { kind: "ok", text: n.text, generation: undefined };
    }
    if (file.state !== "ready" || stale()) return undefined;
    const r = file();
    if (r.kind !== "ok" || r.value.kind !== "text") return { kind: "error" };
    return { kind: "ok", text: r.value.text, generation: r.value.generation };
  });
  const text = createMemo(
    (): SideText | undefined => {
      const l = loaded();
      return l?.kind === "ok"
        ? { text: l.text, index: lineIndex(l.text) }
        : undefined;
    },
    undefined,
    { equals: (a, b) => a?.text === b?.text },
  );
  // Highlighting, when api/source has it.
  const lexed = createSource(
    () => fetchPath() ?? "",
    () => {
      const l = loaded();
      return l?.kind === "ok" ? l.generation : undefined;
    },
    {
      enabled: () => fetchPath() !== undefined && loaded()?.kind === "ok",
      side: () => side,
    },
  );
  return { loaded, text, lexed, path: fetchPath };
}

// The page's state: the rows once the diff and both texts are in.
type Model =
  | { kind: "loading" }
  | { kind: "not-found" }
  | { kind: "error" }
  | { kind: "mismatch" }
  | { kind: "no-lines" }
  | { kind: "lines"; view: View; rows: Rows; base: SideText; head: SideText };

// What a page says for a diff with no rows.
function emptyNote(v: View): string {
  if (v.base.kind === "absent") return texts.emptyAdded;
  if (v.head.kind === "absent") return texts.emptyRemoved;
  return texts.empty;
}

function DiffPage(props: { path: string }): JSX.Element {
  const location = useLocation();
  const routerBase = useResolvedPath(() => "/");
  const ws = createMemo(() => wsFromSearch(location.search));
  const [diff] = createResource(
    () => ({ path: props.path, ws: ws() }),
    (k) => getCbor(diffUrl(k.path, k.ws), decodeDiffView),
  );
  const result = () => (diff.state === "ready" ? diff() : undefined);
  const view = () => {
    const r = result();
    return r?.kind === "ok" ? r.value : undefined;
  };
  const baseData = createSideData("base", () => {
    const v = view();
    return v === undefined ? undefined : needOf(v.base, v.oldPath ?? v.path);
  });
  const headData = createSideData(
    "head",
    () => {
      const v = view();
      return v === undefined ? undefined : needOf(v.head, v.path);
    },
    // Head's text must be from the diff's snapshot.
    () => view()?.generation,
  );

  const model = createMemo((): Model => {
    const r = result();
    if (r === undefined) return { kind: "loading" };
    if (r.kind === "not-found") return { kind: "not-found" };
    if (r.kind === "error") return { kind: "error" };
    const v = r.value;
    if (v.diff.kind !== "lines") return { kind: "no-lines" };
    const bl = baseData.loaded();
    const hl = headData.loaded();
    if (bl?.kind === "error" || hl?.kind === "error") return { kind: "error" };
    const bt = baseData.text();
    const ht = headData.text();
    if (bt === undefined || ht === undefined) return { kind: "loading" };
    // Head's text from another snapshot than the diff (a file changed
    // on disk between the two fetches): same counts can still lie.
    const g = hl?.kind === "ok" ? hl.generation : undefined;
    if (g !== undefined && g !== v.generation) {
      console.warn(
        `diff of ${v.path}: head text is generation ${g}, the diff ${v.generation}`,
      );
      return { kind: "mismatch" };
    }
    const rows = rowsOf(v.diff.runs);
    if (!countsMatch(rows, bt.index.count, ht.index.count)) {
      console.warn(
        `diff of ${v.path}: runs have ${rows.baseCount}/${rows.headCount} lines, texts ${bt.index.count}/${ht.index.count}`,
      );
      return { kind: "mismatch" };
    }
    return { kind: "lines", view: v, rows, base: bt, head: ht };
  });
  const lines = createMemo(
    () => {
      const m = model();
      return m.kind === "lines" ? m : undefined;
    },
    undefined,
    // A new grid only for new rows or texts.
    {
      equals: (a, b) =>
        a === b ||
        (a !== undefined &&
          b !== undefined &&
          a.rows === b.rows &&
          a.base === b.base &&
          a.head === b.head),
    },
  );
  const slow = createDelay(() => model().kind === "loading", 300);

  // Each side's highlight. Same-file links go to this page's hash on
  // that side (D-341).
  const highlight = (side: DiffSessionSide, s: SideData) =>
    createMemo((): Highlight | undefined => {
      const lexed = s.lexed();
      const t = s.text();
      const p = s.path();
      if (lexed === undefined || t === undefined || p === undefined) {
        return undefined;
      }
      const here = (line: number | null) =>
        diffHref(props.path, {
          ws: ws(),
          at: line === null ? undefined : { side, line },
        });
      const links = linkInfos(
        side,
        { path: p, ref: lexed.ref },
        lexed.targets,
        routerBase(),
        here,
      );
      return highlightOf(lexed, t.index.count, links);
    });
  const baseHl = highlight("base", baseData);
  const headHl = highlight("head", headData);

  return (
    <Switch>
      <Match when={model().kind === "not-found"}>
        <NotFound note={texts.notFound} />
      </Match>
      <Match when={true}>
        <PageTitle text={props.path} />
        <div class="copy-host">
          <CopyButton text={() => props.path} label="Copy path" />
          <h1 class="file-path" tabindex="-1">
            {props.path}
          </h1>
        </div>
        <Switch
          fallback={
            <Show when={slow()}>
              <p>Loading…</p>
            </Show>
          }
        >
          <Match when={lines()} keyed>
            {(m) => (
              <Show
                when={m.rows.count > 0}
                fallback={<p>{emptyNote(m.view)}</p>}
              >
                <DiffBody
                  path={props.path}
                  rows={m.rows}
                  base={{
                    text: m.base,
                    hl: baseHl,
                    lexed: baseData.lexed,
                    lineRow: m.rows.baseRow,
                  }}
                  head={{
                    text: m.head,
                    hl: headHl,
                    lexed: headData.lexed,
                    lineRow: m.rows.headRow,
                  }}
                />
              </Show>
            )}
          </Match>
          <Match when={model().kind === "error"}>
            <p>{texts.failed}</p>
          </Match>
          <Match when={model().kind === "mismatch"}>
            <p>{texts.mismatch}</p>
          </Match>
          <Match when={model().kind === "no-lines"}>
            <p>{texts.noLines}</p>
          </Match>
        </Switch>
      </Match>
    </Switch>
  );
}

// One side as the grid uses it. lineRow: the row of each line.
interface Pane {
  text: SideText;
  hl: () => Highlight | undefined;
  lexed: () => LexedSource | undefined;
  lineRow: Uint32Array;
}

function renderOf(p: Pane) {
  return createMemo(() => {
    const h = p.hl();
    return h === undefined ? undefined : highlightedLine(h);
  });
}

// The toolbar and the grid of a diff with rows.
function DiffBody(props: {
  path: string;
  rows: Rows;
  base: Pane;
  head: Pane;
}): JSX.Element {
  const location = useLocation();
  const [whole, setWhole] = createSignal(false);
  // Read once: DiffPage makes a new body for new rows.
  const { rows, base, head } = untrack(() => ({
    rows: props.rows,
    base: props.base,
    head: props.head,
  }));
  const renderBase = renderOf(base);
  const renderHead = renderOf(head);
  const hash = createMemo(() => parseDiffHash(location.hash));
  // The row of the hash. A line past the end: none.
  const marked = createMemo((): number | undefined => {
    const h = hash();
    if (h === undefined) return undefined;
    const map = (h.side === "base" ? base : head).lineRow;
    return h.line < map.length ? map[h.line] : undefined;
  });
  const hint = () => {
    const h = hash();
    return h === undefined || marked() === undefined
      ? ""
      : `${h.side === "base" ? "Base" : "Head"} line ${h.line} marked.`;
  };
  let api: DiffApi | undefined;
  // A same-file link was followed: where the cursor goes, once the
  // hash changes.
  let jump: { row: number; col: number } | undefined;

  const grid = (
    <DiffGrid
      path={props.path}
      rows={rows}
      base={base.text}
      head={head.text}
      renderBase={renderBase()}
      renderHead={renderHead()}
      whole={whole()}
      marked={marked()}
      ref={(a) => (api = a)}
    />
  );

  createEffect(
    on([marked, () => location.state], ([r]) => {
      const a = api;
      if (r === undefined || a === undefined) return;
      a.reveal(r);
      const j = jump;
      jump = undefined;
      // A link of another file took us here: focus the line.
      const codeJump = isCodeJump(untrack(() => location.state));
      if (j !== undefined && j.row === r && a.hasFocus()) a.focusRow(r, j.col);
      else if (a.hasFocus() || codeJump) a.focusRow(r, 0);
      else a.setCursorRow(r);
      a.scrollToRow(r, "center");
      const frame = requestAnimationFrame(() => {
        const el = a.rowEl(r);
        if (el !== undefined) bringOnPage(el);
      });
      onCleanup(() => cancelAnimationFrame(frame));
    }),
  );

  // A same-file code link: the cursor goes to its target's column.
  const onClick = (e: MouseEvent) => {
    const a =
      e.target instanceof Element
        ? e.target.closest<HTMLAnchorElement>("a.code-link")
        : null;
    if (a === null || e.defaultPrevented || e.button !== 0) return;
    if (e.ctrlKey || e.metaKey || e.altKey || e.shiftKey) return;
    const rowEl = a.closest<HTMLElement>(".code-row");
    if (rowEl === null) return;
    const pane =
      rows.kind[Number(rowEl.dataset.key)] === deleteRow ? base : head;
    const h = pane.hl();
    const s = pane.lexed();
    const k = Number(a.dataset.link);
    if (h === undefined || s === undefined || h.links[k - 1]?.here !== true) {
      return;
    }
    const t = pane.text;
    const j = jumpOf(k, s.targets, h.runs, (n) => lineText(t.text, t.index, n));
    if (j === undefined) return;
    const row = pane.lineRow[j.line];
    if (row === undefined) return;
    if (a.hash === location.hash) {
      // Same URL: no navigation, so move here.
      e.preventDefault();
      api?.focusRow(row, j.col);
      api?.scrollToRow(row, "center");
    } else jump = { row, col: j.col };
  };
  let wrap!: HTMLDivElement;
  onMount(() => {
    // Native: before the router's click handler on the document.
    wrap.addEventListener("click", onClick);
    onCleanup(() => wrap.removeEventListener("click", onClick));
  });

  // file-text and diff-text: test hooks, no style.
  return (
    <div class="file-text diff-text" ref={(el) => (wrap = el)}>
      <p class="sr-only" role="status">
        {hint()}
      </p>
      <div class="code-toolbar">
        <button
          type="button"
          aria-pressed={whole()}
          onClick={() => setWhole(!whole())}
        >
          Show whole file
        </button>
        <span class="code-hint">{texts.hint}</span>
      </div>
      {grid}
    </div>
  );
}
