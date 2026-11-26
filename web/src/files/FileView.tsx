import { useLocation, useResolvedPath } from "@solidjs/router";
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
import { decodeFileView, type FileView as View } from "../api/files";
import { docsHref } from "../api/module";
import { type LexedSource, type OutlineEntry } from "../api/source";
import { useDiffLink } from "../diff/link";
import { FileStepButtons } from "../diff/FileStepButtons";
import { fileStep, sourceOrder } from "../diff/fileOrder";
import { filesWhy, useDiffFiles } from "../diff/store";
import { stepTo } from "../diff/rows";
import { fileStepTexts, stepTexts } from "../diff/words";
import { useAnswerGeneration } from "../events/generation";
import { useGoneHome, useQuietReload } from "../events/ReloadApply";
import { bannerText } from "../modules/notices";
import { useModules } from "../modules/store";
import { declAtLine, outlineStep } from "../keys/outline";
import {
  type KeyHandler,
  useCopyLocation,
  useKeyActions,
  useKeys,
  useFileSteps,
  useSideTarget,
  useViewLinks,
  type ViewTarget,
} from "../keys/KeysProvider";
import { copyText } from "../copy/clipboard";
import { CopyButton } from "../copy/CopyButton";
import { location as locationText, targetCopyText } from "../copy/names";
import { usePrefs } from "../prefs/store";
import { NotFound } from "../NotFound";
import { PageTitle } from "../PageTitle";
import { usePageSide } from "../session";
import {
  type CodeApi,
  CodeView,
  type FindHits,
  type LineMarks,
} from "../source/CodeView";
import { createSource } from "../source/createSource";
import { changeStatus } from "../source/diffMarks";
import { createFinder } from "../source/find";
import { FindBar, type FindHandle } from "../source/FindBar";
import { isCodeJump, shouldReland } from "../source/landing";
import { onLeaveClick, originOf, saveOrigin } from "../source/origin";
import { RefsBar, type RefsHandle } from "../source/RefsBar";
import { utf16Cols } from "../source/columns";
import { highlightDecor, highlightedLine } from "../source/decor";
import { type Highlight, highlightOf } from "../source/highlight";
import { jumpOf, linkInfos } from "../source/links";
import { lineIndex, lineText } from "../source/lines";
import { tokenAt } from "../source/refTarget";
import { createDelay, focusInMain, focusLost } from "../ui/focus";
import { bytes, limit } from "../ui/format";
import { noteTexts } from "../ui/notes";
import { bringOnPage } from "../ui/scroll";
import {
  changesNote,
  changesTexts,
  changesWhy,
  createSourceChanges,
} from "./sourceChanges";
import { listState, otherFile, pathTarget, sourceTarget } from "./otherSide";
import {
  apiFileUrl,
  hrefFor,
  isMarkdownPath,
  lineHref,
  mdHref,
  parseLineHash,
  pathFromPathname,
  withBase,
} from "./paths";

// The file view: /<token>/src/<side>/<path>.
export function FileView(): JSX.Element {
  const side = usePageSide();
  const location = useLocation();
  const path = createMemo(() => pathFromPathname(location.pathname, side()));
  return (
    <Show when={path()} keyed fallback={<NotFound />}>
      {(p) => <FilePage path={p} />}
    </Show>
  );
}

function FilePage(props: { path: string }): JSX.Element {
  const side = usePageSide();
  // Read before the old page goes away.
  const fromMain = focusInMain();
  const [file] = createResource(
    () => props.path,
    (p) => getCbor(apiFileUrl(side(), p), decodeFileView),
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
  useGoneHome(notFound, () => props.path);
  const view = (): View | undefined => {
    const r = result();
    return r?.kind === "ok" ? r.value : undefined;
  };
  useAnswerGeneration(() => view()?.generation);
  const lexed = createSource(
    () => props.path,
    () => view()?.generation,
  );
  // Docs / Source in the header. The module from the tree, else from
  // the highlight's answer.
  const moduleRef = () => {
    for (const info of modules.byRef().values()) {
      if (info.path === props.path) return info.ref;
    }
    return lexed()?.ref ?? undefined;
  };
  // Set by FileText: the docs of the declaration at the cursor.
  let docsAt: (() => ViewTarget | undefined) | undefined;
  // Set by FileText (diff mode): the diff at the cursor's line, and why
  // there is none.
  let diffHooks: DiffHooks | undefined;
  // Set by FileText: save the cursor's line in history (D-339).
  let leave: (() => void) | undefined;
  const diffLink = useDiffLink();
  useViewLinks({
    current: "source",
    docs: () => {
      // A Markdown file's docs: the file, rendered.
      if (isMarkdownPath(props.path)) return mdHref(side(), props.path);
      const r = moduleRef();
      return r === undefined ? undefined : docsHref(side(), r);
    },
    source: () => hrefFor(side(), props.path),
    diff: () => diffLink(side(), props.path),
    at: (to) =>
      to === "docs" ? docsAt?.() : to === "diff" ? diffHooks?.at() : undefined,
    missing: (to) =>
      to === "diff" ? (diffHooks?.why() ?? changesTexts.unchanged) : undefined,
    leave: () => leave?.(),
    // A reload: the cursor's line (D-354). T17b keeps the code line.
    here: () => {
      const c = diffHooks?.cursor();
      return c === undefined
        ? undefined
        : {
            href: lineHref(side(), props.path, c.line),
            state: { origin: { col: c.col } },
          };
    },
  });
  // b: the path only (FileText adds the line). Also binary files,
  // symlinks and not found (it may be on the other side).
  const diffFiles = useDiffFiles();
  useSideTarget(() => {
    const s = side();
    if (s === "tree") return undefined;
    return sourceTarget(pathTarget(diffFiles, s, props.path), s);
  });
  // ] and [: this side's changed files, at their top (D-350).
  useFileSteps((dir) => {
    const s = side();
    if (s === "tree") return fileStepTexts.elsewhere;
    const m = diffFiles.marks();
    if (m === undefined) return filesWhy(diffFiles);
    return fileStep(sourceOrder(m), props.path, dir, (p) => hrefFor(s, p));
  });
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
      <div class="copy-host">
        <CopyButton text={() => props.path} label="Copy path" />
        <h1 class="file-path" tabindex="-1">
          {props.path}
        </h1>
      </div>
      <Show when={banner()}>{(b) => <p class="file-banner">{b()}</p>}</Show>
      {/* Read from disk, and git would filter it (D-335). */}
      <Show when={view()?.filtered === true}>
        <p class="file-note">{noteTexts.filtered}</p>
      </Show>
      <Switch
        fallback={
          <Show when={slow()}>
            <p>Loading…</p>
          </Show>
        }
      >
        <Match when={view()} keyed>
          {(v) =>
            body(
              v,
              props.path,
              lexed,
              (f) => {
                docsAt = f;
              },
              (f) => {
                diffHooks = f;
              },
              (f) => {
                leave = f;
              },
            )
          }
        </Match>
        <Match when={result()?.kind === "error"}>
          <p>Couldn't load this file.</p>
        </Match>
      </Switch>
    </Show>
  );
}

function body(
  v: View,
  path: string,
  source: () => LexedSource | undefined,
  setDocsAt: (f: (() => ViewTarget | undefined) | undefined) => void,
  setDiffHooks: (f: DiffHooks | undefined) => void,
  setLeave: (f: (() => void) | undefined) => void,
): JSX.Element {
  switch (v.kind) {
    case "text":
      return (
        <FileText
          text={v.text}
          path={path}
          source={source()}
          generation={v.generation}
          setDocsAt={setDocsAt}
          setDiffHooks={setDiffHooks}
          setLeave={setLeave}
        />
      );
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
    case "not-local":
      return <p>{noteTexts.notLocal}</p>;
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
  const side = usePageSide();
  const base = useResolvedPath(() => "/");
  return (
    <p>
      Symbolic link to{" "}
      <Show when={props.resolved} fallback={<code>{props.target}</code>}>
        {(r) => (
          <a href={withBase(base(), hrefFor(side(), r()))}>
            <code>{props.target}</code>
          </a>
        )}
      </Show>
    </p>
  );
}

// What the text tells the page: for the header's Diff link and `3`,
// and the cursor (where a reload lands, D-354).
interface DiffHooks {
  // The diff at the cursor's line.
  at: () => ViewTarget | undefined;
  // Why there is no diff to show, if the state says.
  why: () => string | undefined;
  cursor: () => { line: number; col: number } | undefined;
}

// The text in the source view. #L<n>[-L<m>] marks those lines and
// scrolls the first one to the middle; again on a hash change, with no
// refetch. Bad or out of range: nothing. An end past the end is cut.
// source: highlighting, when it comes.
function FileText(props: {
  text: string;
  path: string;
  source: LexedSource | undefined;
  generation: number;
  setDocsAt?: (f: (() => ViewTarget | undefined) | undefined) => void;
  setDiffHooks?: (f: DiffHooks | undefined) => void;
  setLeave?: (f: (() => void) | undefined) => void;
}): JSX.Element {
  const side = usePageSide();
  const location = useLocation();
  const quietReload = useQuietReload();
  const base = useResolvedPath(() => "/");
  const index = createMemo(() => lineIndex(props.text));
  // Diff mode: the change signs, and n / p between them (D-348).
  const diffLink = useDiffLink();
  const changes = createSourceChanges({
    // eslint-disable-next-line solid/reactivity -- fixed for this view
    path: props.path,
    lines: () => index().count,
    // eslint-disable-next-line solid/reactivity -- fixed for this view
    generation: props.generation,
  });
  const codeChanges = () => {
    const side = changes.side();
    const s = changes.state();
    return side === undefined
      ? undefined
      : { side, marks: s.kind === "lines" ? s.changes : undefined };
  };
  const hl = createMemo((): Highlight | undefined => {
    const s = props.source;
    if (s === undefined) return undefined;
    const links = linkInfos(
      side(),
      { path: props.path, ref: s.ref },
      s.targets,
      base(),
    );
    return highlightOf(s, index().count, links);
  });
  const line = (n: number) => withBase(base(), lineHref(side(), props.path, n));
  // The outline, if it has entries.
  const outline = () => {
    const o = hl()?.outline;
    return o === undefined || o.length === 0 ? undefined : o;
  };
  const docs = (id: string): string | undefined => {
    const ref = props.source?.ref ?? null;
    return ref === null
      ? undefined
      : withBase(base(), docsHref(side(), ref, id));
  };
  // New functions when the highlight comes: the rows redraw.
  const renderLine = createMemo(() => {
    const h = hl();
    return h === undefined ? undefined : highlightedLine(h);
  });
  const card = (k: number) => hl()?.cards[k - 1];
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
  // The same, as a signal, for the label of the References button.
  const [apiSig, setApiSig] = createSignal<CodeApi>();
  let findBar: FindHandle | undefined;
  let refsBar: RefsHandle | undefined;
  const prefs = usePrefs();
  const [find, setFind] = createSignal<FindHits>();
  const finder = createMemo(() => createFinder(props.text, index()));
  // A same-file link was followed: where the cursor goes, once the
  // hash changes (D-284).
  let jump: { line: number; col: number } | undefined;
  // Where a refs link landed before the highlight came.
  let land: { line: number; col: number } | undefined;
  // Before we leave for a link: Back comes to the cursor (D-339).
  const saveCursor = (to?: string) => {
    const a = api;
    if (a === undefined) return;
    const c = a.cursor();
    saveOrigin(
      `#L${c.line}`,
      { col: c.col },
      untrack(marks)?.from === c.line,
      c.line === 1,
      to,
    );
  };
  // eslint-disable-next-line solid/reactivity -- given once
  props.setLeave?.(() => saveCursor());
  onCleanup(() => props.setLeave?.(undefined));
  const onLinkClick = (e: MouseEvent) =>
    onLeaveClick(e, sameFileJump, saveCursor);
  // A same-file code link (D-284).
  const sameFileJump = (e: MouseEvent, a: HTMLAnchorElement) => {
    const s = props.source;
    const h = hl();
    if (!a.classList.contains("code-link")) return;
    if (s === undefined || h === undefined) return;
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
  // The match a text search link landed on (D-310), shown as a find
  // hit. Find or references replace it; another landing or Esc clears
  // it. from: the history state it came with: each landing is marked
  // once, so a redo of the marks (same state) doesn't bring it back.
  let landed: FindHits | undefined;
  let landedFrom: unknown;
  const markLanded = (
    from: unknown,
    line: number,
    cp: unknown,
    len: unknown,
  ): void => {
    if (from === landedFrom) return;
    landedFrom = from;
    const had = landed !== undefined && untrack(find) === landed;
    landed = undefined;
    if (typeof cp === "number" && typeof len === "number" && cp >= 1) {
      const t = lineText(props.text, index(), line);
      // The file changed on disk since the search: no mark.
      if (len >= 1 && cp + len - 1 <= [...t].length) {
        const [a, b] = utf16Cols(t, [cp, cp + len]) as [number, number];
        // An open bar would keep its words over our paint.
        findBar?.closeQuietly();
        refsBar?.closeQuietly();
        landed = {
          matches: {
            count: 1,
            line: Uint32Array.of(line),
            from: Uint32Array.of(a),
            to: Uint32Array.of(b),
          },
          current: 0,
        };
        setFind(landed);
        return;
      }
    }
    if (had) setFind(undefined);
  };
  const clearLanded = (): boolean => {
    if (landed === undefined || untrack(find) !== landed) return false;
    landed = undefined;
    setFind(undefined);
    return true;
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
        card={card}
        find={find()}
        changes={codeChanges()}
        ref={(a) => {
          api = a;
          setApiSig(a);
        }}
      />
    </Show>
  );
  createEffect(
    // The state too: a link to the line already in the URL (same
    // hash, new state) is a new landing.
    on([marks, () => location.state], ([marked]) => {
      const a = api;
      // Back to an origin with no hash: line 1 (D-339).
      const m =
        marked ??
        (originOf(untrack(() => location.state)) === undefined
          ? undefined
          : { from: 1, to: 1 });
      if (m === undefined || a === undefined) return;
      // The cursor goes there too, so Tab lands on the line. Focus
      // stays in the view if it was there (its old row may go away).
      const j = jump;
      jump = undefined;
      // A link of another file took us here: focus the line.
      const state: unknown = untrack(() => location.state);
      const codeJump = isCodeJump(state);
      // A refs page link also says the token's column (code points).
      const cp =
        codeJump && typeof state === "object" && state !== null
          ? (state as { cp?: unknown }).cp
          : undefined;
      // A text search link also says the length: mark that match.
      const len =
        codeJump && typeof state === "object" && state !== null
          ? (state as { len?: unknown }).len
          : undefined;
      // Back to where a link was followed: the cursor, focused. No
      // search mark: it would move the cursor to its own column.
      const origin = originOf(state);
      markLanded(state, m.from, cp, origin === undefined ? len : undefined);
      // A reload with focus outside main: the cursor only (D-354).
      if (origin !== undefined && quietReload(state)) {
        a.setCursor(m.from, origin.col);
      } else if (origin !== undefined) {
        a.focusAt(m.from, origin.col);
        if (untrack(hl) === undefined) {
          land = { line: m.from, col: origin.col };
        }
      } else if (j !== undefined && j.line === m.from && a.hasFocus()) {
        a.focusAt(j.line, j.col);
      } else if (typeof cp === "number" && cp >= 1) {
        const col =
          utf16Cols(lineText(props.text, index(), m.from), [cp])[0] ?? 0;
        a.focusAt(m.from, col);
        // No highlight yet: the token is no link. When it comes, once
        // more, if nothing else moved the cursor meanwhile.
        if (untrack(hl) === undefined) land = { line: m.from, col };
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
  createEffect(
    on(hl, (h) => {
      const l = land;
      if (h === undefined || l === undefined) return;
      land = undefined;
      // After the rows are drawn again.
      queueMicrotask(() => {
        if (shouldReland(l, api?.cursor(), api?.hasFocus() === true)) {
          api?.focusAt(l.line, l.col);
        }
      });
    }),
  );
  // Keys (D-298). j / k: the outline's entries.
  const step = (dir: 1 | -1): boolean => {
    const entries = outline();
    const a = api;
    if (entries === undefined || a === undefined) return false;
    const to = outlineStep(entries, a.cursor().line, dir);
    if (to === undefined) return true;
    a.focusLine(to);
    a.scrollToLine(to, "center");
    return true;
  };
  useKeyActions({
    next: () => step(1),
    prev: () => step(-1),
    definition: () => {
      const link = api?.cursorLink();
      if (link === undefined) return "No link here.";
      link.click();
      return true;
    },
    references: () => {
      if (refsBar === undefined) return false;
      refsBar.run();
      return true;
    },
    find: () => {
      if (findBar === undefined) return false;
      findBar.open();
      return true;
    },
    // After the hover card and the bars' own Esc: a bar that is open
    // while focus is elsewhere.
    escape: () => {
      if (findBar?.isOpen() === true) {
        findBar.close();
        return true;
      }
      if (refsBar?.isOpen() === true) {
        refsBar.close();
        return true;
      }
      return (api?.clearSelection() ?? false) || clearLanded();
    },
  });
  const keys = useKeys();
  // n / p and the buttons: this side's change lines (D-348). The first
  // step forward on a fresh page may land on line 1. focus: into the
  // view (keys); else the cursor moves and focus stays (buttons). The
  // words to say, or true. Undefined: not in diff mode.
  let stepped = false;
  const stepChange = (
    dir: 1 | -1,
    focus: boolean,
  ): string | true | undefined => {
    const st = changes.state();
    const why = changesWhy(st);
    if (why !== undefined) return why;
    const a = api;
    const side = changes.side();
    if (st.kind !== "lines" || a === undefined || side === undefined) {
      return undefined;
    }
    const c = a.cursor().line;
    const fresh = dir === 1 && !stepped && !a.hasFocus() && c === 1;
    stepped = true;
    const to = stepTo(st.changes.starts, fresh ? 0 : c, dir);
    if (to === undefined) return stepTexts.noMore;
    if (focus) a.focusLine(to);
    else a.setCursor(to);
    a.scrollToLine(to, "center");
    return focus ? true : changeStatus(st.changes, to, side);
  };
  const changeKey =
    (dir: 1 | -1): KeyHandler =>
    () =>
      stepChange(dir, true) ?? false;
  useKeyActions({ nextChange: changeKey(1), prevChange: changeKey(-1) });
  // "Show diff here (3)": the hint only with shortcuts on.
  const diffButton = () =>
    `Show diff here${prefs.singleKeyShortcuts() ? " (3)" : ""}`;
  const clickChange = (dir: 1 | -1) => {
    const r = stepChange(dir, false);
    if (typeof r === "string") keys?.say(r);
  };
  // y: the name at the cursor, else its place. Copy location: the
  // selected lines, else the cursor line.
  const modules = useModules();
  const here = () => locationText(props.path, api?.cursor().line ?? 1);
  const nameAtCursor = (): string | undefined => {
    const a = api;
    const h = hl();
    const s = props.source;
    if (a === undefined || h === undefined || s === undefined) return undefined;
    const tok = tokenAt(
      h.runs,
      s.targets,
      // eslint-disable-next-line solid/reactivity -- run by a key
      (n) => lineText(props.text, index(), n),
      a.cursor(),
    );
    const t = tok?.target;
    if (tok === undefined || t === undefined) return undefined;
    return targetCopyText(
      tok.text,
      t,
      (ref) => modules.byRef().get(ref)?.name ?? undefined,
    );
  };
  useKeyActions({
    copy: () => {
      void copyText(nameAtCursor() ?? here(), keys?.say);
      return true;
    },
  });
  useCopyLocation(() => {
    const r = api?.selectedLines();
    return r === undefined ? here() : locationText(props.path, r.from, r.to);
  });
  // b: the same line on the other side (D-349). Back: the cursor.
  const diffFiles = useDiffFiles();
  useSideTarget(() => {
    const s = changes.side();
    if (s === undefined) return undefined;
    const o = {
      side: s,
      path: props.path,
      ...listState(diffFiles, props.path),
      state: changes.state(),
    };
    // The cursor only on use: no mapLine on every move.
    return sourceTarget(
      otherFile({ ...o, line: undefined }),
      s,
      () => otherFile({ ...o, line: api?.cursor().line }),
      () => saveCursor(),
    );
  });
  // 1: the docs of the declaration at the cursor.
  // eslint-disable-next-line solid/reactivity -- given once
  props.setDocsAt?.(() => {
    const ref = props.source?.ref ?? null;
    const a = api;
    if (ref === null || a === undefined) return undefined;
    const e = declAtLine(outline() ?? [], a.cursor().line);
    return { href: docsHref(side(), ref, e?.[1] ?? null) };
  });
  onCleanup(() => props.setDocsAt?.(undefined));
  // 3: the diff at the cursor's line, on this side.
  // eslint-disable-next-line solid/reactivity -- given once
  props.setDiffHooks?.({
    at: () => {
      const ds = changes.side();
      const a = api;
      if (ds === undefined || a === undefined) return undefined;
      const href = diffLink(side(), props.path, {
        side: ds,
        line: a.cursor().line,
      });
      return href === undefined
        ? undefined
        : { href, state: { codeJump: true } };
    },
    why: () => changesWhy(changes.state()),
    cursor: () => {
      const c = api?.cursor();
      return c === undefined ? undefined : { line: c.line, col: c.col };
    },
  });
  onCleanup(() => props.setDiffHooks?.(undefined));
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
      <Show when={changesNote(changes.state())}>
        {(n) => <p class="file-note">{n()}</p>}
      </Show>
      <Show when={index().count > 0}>
        <FindBar
          finder={finder}
          api={() => api}
          onFind={setFind}
          onOpen={() => refsBar?.closeQuietly()}
          handle={(h) => (findBar = h)}
          keyHint={prefs.singleKeyShortcuts() ? "(f)" : undefined}
        >
          <RefsBar
            hl={hl}
            targets={() => props.source?.targets ?? []}
            lineText={(n) => lineText(props.text, index(), n)}
            api={apiSig}
            onHits={setFind}
            onOpen={() => findBar?.closeQuietly()}
            handle={(h) => (refsBar = h)}
            onLeave={() => saveCursor()}
          />
          <Show when={changes.state().kind === "lines"}>
            <button type="button" onClick={() => clickChange(-1)}>
              {stepTexts.prevChange}
            </button>
            <button type="button" onClick={() => clickChange(1)}>
              {stepTexts.nextChange}
            </button>
          </Show>
          <Show when={changes.side() !== undefined}>
            <button type="button" onClick={() => keys?.goView("diff")}>
              {diffButton()}
            </button>
          </Show>
          <FileStepButtons />
        </FindBar>
      </Show>
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
