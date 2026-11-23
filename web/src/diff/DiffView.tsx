// The diff view: /<token>/diff/<path>[?ws=ignore][#<side>-L<n>]
// (D-341, D-342). One file, base against head, unified. Data:
// api/diff, then each side's text (api/file) and highlighting
// (api/source).
import { A, useLocation, useNavigate, useResolvedPath } from "@solidjs/router";
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
import { docsHref } from "../api/module";
import type { LexedSource } from "../api/source";
import { CopyButton } from "../copy/CopyButton";
import {
  apiFileUrl,
  hrefFor,
  isMarkdownPath,
  lineHref,
  mdHref,
} from "../files/paths";
import {
  type KeyHandler,
  useKeyActions,
  useKeys,
  useViewLinks,
  type ViewLinks,
} from "../keys/KeysProvider";
import { declAtLine } from "../keys/outline";
import { NotFound } from "../NotFound";
import { PageTitle } from "../PageTitle";
import { useSession } from "../session";
import type { DiffSessionSide } from "../sides";
import { createSource } from "../source/createSource";
import { highlightedLine } from "../source/decor";
import { type Highlight, highlightOf } from "../source/highlight";
import { isCodeJump, shouldReland } from "../source/landing";
import { jumpOf, linkInfos } from "../source/links";
import {
  type Origin,
  onLeaveClick,
  originOf,
  saveOrigin,
} from "../source/origin";
import { lineIndex, lineText } from "../source/lines";
import { createDelay } from "../ui/focus";
import { bringOnPage } from "../ui/scroll";
import { type DiffApi, DiffGrid, type SideText } from "./DiffGrid";
import { diffNotes } from "./notes";
import {
  blockStarts,
  countsMatch,
  deleteRow,
  hunkStarts,
  type Rows,
  rowLabel,
  rowsOf,
  stepTo,
} from "./rows";
import {
  diffHash,
  diffHref,
  diffPathFromPathname,
  parseDiffHash,
  searchWithWs,
  wsFromSearch,
} from "./urls";

export const texts = {
  notDiff: "Not in diff mode.",
  notFound: "No file at this path on either side.",
  failed: "Couldn't load this diff. See the terminal for details.",
  mismatch: "Couldn't show this diff.",
  empty: "Both sides are empty.",
  emptyAdded: "Empty file added.",
  emptyRemoved: "Empty file removed.",
  hint: "Browser find sees only the lines on screen.",
  ws: "Ignore whitespace",
  wholeFile: "Show whole file",
  prevChange: "Previous change",
  nextChange: "Next change",
  tryAgain: "Try again",
  stillSlow: "Still too slow.",
  baseSource: "Base source",
  headSource: "Head source",
  noMore: "No more changes.",
  noDocs: "No docs for this page.",
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

// Base's path: the old one of a rename.
function basePathOf(v: View): string {
  return v.oldPath ?? v.path;
}

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
  const navigate = useNavigate();
  const [diff, { refetch }] = createResource(
    () => ({ path: props.path, ws: ws() }),
    (k) => getCbor(diffUrl(k.path, k.ws), decodeDiffView),
  );
  const result = () => (diff.state === "ready" ? diff() : undefined);
  const view = () => {
    const r = result();
    return r?.kind === "ok" ? r.value : undefined;
  };
  // Texts only for a diff with lines (not for too slow).
  const linesView = () => {
    const v = view();
    return v?.diff.kind === "lines" ? v : undefined;
  };
  const baseData = createSideData("base", () => {
    const v = linesView();
    return v === undefined ? undefined : needOf(v.base, basePathOf(v));
  });
  const headData = createSideData(
    "head",
    () => {
      const v = linesView();
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

  // The notes of the last diff in: kept while a new one loads (ws).
  const notesView = createMemo(
    (last: View | undefined) => (result() === undefined ? last : view()),
    undefined,
  );
  const notes = createMemo(() => {
    const v = notesView();
    return v === undefined ? [] : diffNotes(v);
  });
  const tooSlow = () => notesView()?.diff.kind === "too-slow";
  // The grid's buttons: kept while a new diff loads, so focus stays.
  const rowsShown = createMemo((last: boolean) => {
    const m = model();
    if (m.kind === "loading") return last;
    return m.kind === "lines" && m.rows.count > 0;
  }, false);
  // Ignore whitespace: also when -w is on or too slow, to turn it off.
  const toolbarShown = () => rowsShown() || tooSlow() || ws();
  const [whole, setWhole] = createSignal(false);
  // Same URL, ws on or off: a new history entry, same hash, other
  // params kept (D-342).
  const toggleWs = () =>
    navigate(
      diffHref(props.path) +
        searchWithWs(location.search, !ws()) +
        location.hash,
      { scroll: false },
    );
  // Each side's path, for the links to it.
  const basePath = () => {
    const v = notesView();
    return v === undefined ? props.path : basePathOf(v);
  };
  const headPath = () => notesView()?.path ?? props.path;

  // Set by DiffBody while the grid lives.
  let hooks: BodyHooks | undefined;
  const keys = useKeys();
  const stepChange = (dir: 1 | -1) => {
    const r = hooks?.stepChange(dir);
    if (r !== undefined) keys?.say(r);
  };
  // Docs / Source in the header, and 1 / 2 (D-342). The cursor's side
  // and line when the grid has one; else head's (base's if removed).
  const topSide = (): DiffSessionSide =>
    notesView()?.head.kind === "absent" ? "base" : "head";
  const topPath = () => (topSide() === "base" ? basePath() : headPath());
  useViewLinks({
    current: "diff",
    docs: () => {
      const s = topSide();
      if (isMarkdownPath(topPath())) return mdHref(s, topPath());
      const ref = (s === "base" ? baseData : headData).lexed()?.ref ?? null;
      return ref === null ? undefined : docsHref(s, ref);
    },
    source: () => hrefFor(topSide(), topPath()),
    at: (to) => hooks?.at(to),
    leave: () => hooks?.leave(),
  });

  // Try again: say if it is still too slow; else focus goes to the
  // grid once it is in, or to the heading.
  let heading!: HTMLHeadingElement;
  let focusGrid = false;
  const tryAgain = async () => {
    await refetch();
    const v = view();
    if (v?.diff.kind === "too-slow") keys?.say(texts.stillSlow);
    else if (v?.diff.kind === "lines" && v.diff.runs.length > 0) {
      focusGrid = true;
    } else heading.focus();
  };
  // No grid after all (texts failed, mismatch), or a new ws: no
  // focus later. A new path is a new page.
  createEffect(() => {
    const k = model().kind;
    if (k !== "loading" && k !== "lines") focusGrid = false;
  });
  createEffect(on(ws, () => (focusGrid = false), { defer: true }));
  const takeFocus = () => {
    const f = focusGrid;
    focusGrid = false;
    return f;
  };

  return (
    <Switch>
      <Match when={model().kind === "not-found"}>
        <NotFound note={texts.notFound} />
      </Match>
      <Match when={true}>
        <PageTitle text={props.path} />
        <div class="copy-host">
          <CopyButton text={() => props.path} label="Copy path" />
          <h1 class="file-path" tabindex="-1" ref={(el) => (heading = el)}>
            {props.path}
          </h1>
        </div>
        <Show when={toolbarShown()}>
          <div class="code-toolbar">
            <Show when={rowsShown()}>
              <button type="button" onClick={() => stepChange(-1)}>
                {texts.prevChange}
              </button>
              <button type="button" onClick={() => stepChange(1)}>
                {texts.nextChange}
              </button>
              <button
                type="button"
                aria-pressed={whole()}
                onClick={() => setWhole(!whole())}
              >
                {texts.wholeFile}
              </button>
            </Show>
            <button type="button" aria-pressed={ws()} onClick={toggleWs}>
              {texts.ws}
            </button>
            <Show when={rowsShown()}>
              <span class="code-hint">{texts.hint}</span>
            </Show>
          </div>
        </Show>
        <For each={notes()}>{(n) => <p class="file-note">{n}</p>}</For>
        <Show when={tooSlow()}>
          {/* diff-retry: a test hook, no style. */}
          <p class="diff-retry">
            <button type="button" onClick={() => void tryAgain()}>
              {texts.tryAgain}
            </button>{" "}
            <Show when={notesView()?.base.kind !== "absent"}>
              <A href={hrefFor("base", basePath())}>{texts.baseSource}</A>{" "}
            </Show>
            <Show when={notesView()?.head.kind !== "absent"}>
              <A href={hrefFor("head", headPath())}>{texts.headSource}</A>
            </Show>
          </p>
        </Show>
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
                  whole={whole()}
                  base={{
                    side: "base",
                    path: basePathOf(m.view),
                    text: m.base,
                    hl: baseHl,
                    lexed: baseData.lexed,
                    lineRow: m.rows.baseRow,
                  }}
                  head={{
                    side: "head",
                    path: m.view.path,
                    text: m.head,
                    hl: headHl,
                    lexed: headData.lexed,
                    lineRow: m.rows.headRow,
                  }}
                  setHooks={(h) => (hooks = h)}
                  takeFocus={takeFocus}
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
        </Switch>
      </Match>
    </Switch>
  );
}

// What the grid's body gives the page while it lives.
interface BodyHooks {
  // 1 / 2: the docs or source of the cursor's side and line.
  at: NonNullable<ViewLinks["at"]>;
  // Back comes to the cursor (D-339).
  leave: () => void;
  // The change buttons: the cursor goes there, focus stays. What to
  // say: the row, or "No more changes.".
  stepChange: (dir: 1 | -1) => string | undefined;
}

// One side as the grid uses it. lineRow: the row of each line.
interface Pane {
  side: DiffSessionSide;
  // The side's path (base: the old one of a rename).
  path: string;
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

// The grid of a diff with rows, its keys and its Back origin.
function DiffBody(props: {
  path: string;
  rows: Rows;
  base: Pane;
  head: Pane;
  whole: boolean;
  setHooks: (h: BodyHooks | undefined) => void;
  // True once: focus the grid when it is in (after Try again).
  takeFocus: () => boolean;
}): JSX.Element {
  const location = useLocation();
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
  // A row's side, and its line there. Removed rows: base; the rest
  // (gaps too: equal rows) head.
  const paneOf = (r: number) => (rows.kind[r] === deleteRow ? base : head);
  const lineOf = (r: number) =>
    (paneOf(r) === base ? rows.base[r] : rows.head[r]) ?? 0;
  let api: DiffApi | undefined;
  // A same-file link was followed: where the cursor goes, once the
  // hash changes.
  let jump: { row: number; col: number } | undefined;
  // Back came here before the highlight: once it comes, land again.
  let land: { row: number; col: number } | undefined;

  const grid = (
    <DiffGrid
      path={props.path}
      rows={rows}
      base={base.text}
      head={head.text}
      renderBase={renderBase()}
      renderHead={renderHead()}
      whole={props.whole}
      marked={marked()}
      ref={(a) => (api = a)}
    />
  );

  createEffect(
    on([marked, () => location.state], ([m]) => {
      const a = api;
      const state: unknown = untrack(() => location.state);
      const origin = originOf(state);
      // Back to an origin with no hash: the first row (D-342).
      const r = m ?? (origin === undefined ? undefined : 0);
      if (r === undefined || a === undefined) return;
      // Back to a gap: the gap, kept closed.
      if (origin?.gap !== true) a.reveal(r);
      const j = jump;
      jump = undefined;
      land = undefined;
      // A link of another file took us here: focus the line.
      const codeJump = isCodeJump(state);
      if (origin !== undefined) {
        a.focusRow(r, origin.col);
        if (origin.gap !== true && untrack(paneOf(r).hl) === undefined) {
          land = { row: r, col: origin.col };
        }
      } else if (j !== undefined && j.row === r && a.hasFocus()) {
        a.focusRow(r, j.col);
      } else if (a.hasFocus() || codeJump) a.focusRow(r, 0);
      else a.setCursorRow(r);
      a.scrollToRow(r, "center");
      const frame = requestAnimationFrame(() => {
        const el = a.rowEl(r);
        if (el !== undefined) bringOnPage(el);
      });
      onCleanup(() => cancelAnimationFrame(frame));
    }),
  );
  // The highlight came after Back landed: land again, on its link.
  createEffect(
    on([base.hl, head.hl], () => {
      const l = land;
      if (l === undefined || paneOf(l.row).hl() === undefined) return;
      land = undefined;
      queueMicrotask(() => {
        const a = api;
        if (a === undefined) return;
        const c = a.cursor();
        const at = { line: c.row, col: c.col };
        if (shouldReland({ line: l.row, col: l.col }, at, a.hasFocus())) {
          a.focusRow(l.row, l.col);
        }
      });
    }),
  );

  // Before we leave for a link or by 1 / 2: Back comes to the cursor
  // (D-339, D-342). to: where the link goes.
  const saveCursor = (to?: string) => {
    const a = api;
    if (a === undefined) return;
    const c = a.cursor();
    const origin: Origin = c.gap ? { col: 0, gap: true } : { col: c.col };
    saveOrigin(
      diffHash({ side: paneOf(c.row).side, line: lineOf(c.row) }),
      origin,
      untrack(marked) === c.row,
      c.first,
      to,
    );
  };
  // 1 / 2: the docs or source of the cursor's side and line. null:
  // none on that side (not lexed yet, or no module), never the other
  // side's.
  const at = (to: "docs" | "source") => {
    const a = api;
    if (a === undefined) return undefined;
    const r = a.cursor().row;
    const p = paneOf(r);
    const line = lineOf(r);
    if (to === "source") return { href: lineHref(p.side, p.path, line) };
    if (isMarkdownPath(p.path)) return { href: mdHref(p.side, p.path) };
    const ref = p.lexed()?.ref ?? null;
    if (ref === null) return null;
    const e = declAtLine(p.hl()?.outline ?? [], line);
    return { href: docsHref(p.side, ref, e?.[1] ?? null) };
  };

  // j / k: hunks; n / p and the buttons: change blocks (D-342). The
  // first step forward on a fresh page may land on the cursor's own
  // row (a change on row 0).
  const hunks = hunkStarts(rows);
  const blocks = blockStarts(rows);
  let stepped = false;
  // focus: into the grid (keys); else the cursor moves and focus
  // stays (buttons). The words to say, or true.
  const go = (
    starts: readonly number[],
    dir: 1 | -1,
    focus: boolean,
  ): string | true | undefined => {
    const a = api;
    if (a === undefined) return undefined;
    const c = a.cursor();
    const fresh = dir === 1 && !stepped && !a.hasFocus() && c.first;
    stepped = true;
    const to = stepTo(starts, fresh ? c.row - 1 : c.row, dir);
    if (to === undefined) return texts.noMore;
    a.reveal(to);
    if (focus) a.focusRow(to, 0);
    else a.setCursorRow(to);
    a.scrollToRow(to, "center");
    return focus ? true : rowLabel(rows, to);
  };
  const key =
    (starts: readonly number[], dir: 1 | -1): KeyHandler =>
    () =>
      go(starts, dir, true) ?? false;
  useKeyActions({
    next: key(hunks, 1),
    prev: key(hunks, -1),
    nextChange: key(blocks, 1),
    prevChange: key(blocks, -1),
  });
  // eslint-disable-next-line solid/reactivity -- given once
  props.setHooks({
    at,
    leave: () => saveCursor(),
    stepChange: (dir) => {
      const r = go(blocks, dir, false);
      return r === true ? undefined : r;
    },
  });
  onCleanup(() => props.setHooks(undefined));

  // A same-file code link: the cursor goes to its target's column.
  const sameFileJump = (e: MouseEvent, a: HTMLAnchorElement) => {
    if (!a.classList.contains("code-link")) return;
    const rowEl = a.closest<HTMLElement>(".code-row");
    if (rowEl === null) return;
    const pane = paneOf(Number(rowEl.dataset.key));
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
  // Any same-site link in the grid: as FileView.
  const onClick = (e: MouseEvent) => onLeaveClick(e, sameFileJump, saveCursor);
  let wrap!: HTMLDivElement;
  onMount(() => {
    // After Try again: the marked row, else the first one.
    if (props.takeFocus()) api?.focusRow(untrack(marked) ?? 0, 0);
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
      {grid}
    </div>
  );
}
