// The source view: a file's lines as an ARIA grid, virtualised (D-70,
// SPEC §11.7). Only rows near the screen are mounted, plus pinned ones
// (cursor, selection ends). Fixed row height, no wrap.
import {
  createVirtualizer,
  type Range,
  type Virtualizer,
} from "@tanstack/solid-virtual";
import {
  createEffect,
  createMemo,
  createSelector,
  createSignal,
  getOwner,
  type JSX,
  on,
  onCleanup,
  onMount,
} from "solid-js";
import { setVar } from "../ui/cssom";
import { bringOnPage } from "../ui/scroll";
import { createCopy } from "./copy";
import { pieces } from "./display";
import { focusKeepingSelection } from "./focus";
import { isSelectAll, keyTarget } from "./keys";
import { type LineIndex, lineText } from "./lines";
import { domPoint } from "./selection";
import { createRowHeight, mountRows, withPins } from "./virtual";

// Lines from 1, columns in UTF-16 units from 0.
export interface Cursor {
  line: number;
  col: number;
}

// Marked lines, both ends in.
export interface LineMarks {
  from: number;
  to: number;
}

// What other views can do with a source view.
export interface CodeApi {
  // Scroll the line into the view. No focus change.
  scrollToLine: (line: number, align?: "auto" | "center" | "start") => void;
  // Move the cursor to the line, focus it and scroll to it.
  focusLine: (line: number) => void;
  // Move the cursor, no focus change and no scroll.
  setCursor: (line: number) => void;
  cursor: () => Cursor;
  // Focus is somewhere in the view.
  hasFocus: () => boolean;
  // Lines to keep mounted (on top of the cursor's and the selection's).
  // For find (T09) and links (T08).
  pin: (lines: readonly number[]) => void;
  // The selection's ends in the view, as lines. For T08/T09 (copy
  // location, find from the selection).
  selectionLines: () => number[];
  // The row of a line, if it is mounted.
  row: (line: number) => HTMLElement | undefined;
}

// text and index are read once: a new file needs a new view.
export interface CodeViewProps {
  text: string;
  index: LineIndex;
  marks?: LineMarks | undefined;
  // The code of a line; plain text by default.
  renderLine?: (line: number, text: string) => JSX.Element;
  ref?: (api: CodeApi) => void;
}

// 0.9rem × 1.5 at 16px, about: only until the probe is measured (and
// in tests, which have no layout).
export const fallbackRowHeight = 21;
const overscan = 10;

export function CodeView(props: CodeViewProps): JSX.Element {
  let box!: HTMLDivElement;
  let rowsEl!: HTMLDivElement;
  let probe!: HTMLDivElement;
  const owner = getOwner();
  const count = () => props.index.count;
  const clamp = (n: number) => Math.min(Math.max(1, n), Math.max(1, count()));
  const rowHeight = createRowHeight(fallbackRowHeight);
  const [cursor, setCursorRaw] = createSignal<Cursor>({ line: 1, col: 0 });
  const [extraPins, setExtraPins] = createSignal<readonly number[]>([]);
  const [selLines, setSelLines] = createSignal<number[]>([]);
  const pins = createMemo(() =>
    [cursor().line, ...selLines(), ...extraPins()].map((n) => n - 1),
  );

  const setCursor = (c: Cursor) => {
    const cur = cursor();
    if (cur.line !== c.line || cur.col !== c.col) setCursorRaw(c);
  };

  // Scrolled with focus elsewhere and the cursor off screen: the cursor
  // follows, so Tab lands on a line on screen. Only on a real scroll,
  // not when the cursor itself just moved.
  const follow = (inst: Virtualizer<HTMLDivElement, HTMLDivElement>) => {
    const r = inst.range;
    if (!inst.isScrolling || r === null || box.contains(document.activeElement))
      return;
    const n = cursor().line - 1;
    if (n < r.startIndex || n > r.endIndex) {
      setCursor({ line: clamp(r.startIndex + 1), col: 0 });
    }
  };

  const v = createVirtualizer<HTMLDivElement, HTMLDivElement>({
    get count() {
      return count();
    },
    getScrollElement: () => box,
    estimateSize: () => rowHeight.height(),
    overscan,
    // A new function when the pins change: the virtualizer only
    // recomputes on a new one.
    get rangeExtractor() {
      // eslint-disable-next-line solid/reactivity -- read again on each change
      const p = pins();
      return (r: Range) => withPins(r, p);
    },
    onChange: follow,
  });
  // The lines to mount, in order.
  const lines = createMemo(() => v.getVirtualItems().map((i) => i.index + 1));

  const row = (n: number) =>
    rowsEl.querySelector<HTMLElement>(`.code-row[data-line="${n}"]`) ??
    undefined;
  const scrollToLine = (n: number, align: "auto" | "center" | "start") => {
    v.scrollToIndex(clamp(n) - 1, { align });
  };
  // The cursor's row is pinned, so it's mounted and its cell focusable
  // once the cursor moves: at once in an event handler, after the
  // effects inside an effect (then pendingFocus does it).
  let pendingFocus: number | undefined;
  const focusRow = (line: number, page = true): boolean => {
    const r = row(line);
    const c = r?.querySelector<HTMLElement>(".code-cell");
    if (r === undefined || !c?.hasAttribute("tabindex")) return false;
    focusKeepingSelection(c);
    if (page) bringOnPage(r);
    return true;
  };
  const focusLine = (n: number) => {
    const line = clamp(n);
    setCursor({ line, col: line === cursor().line ? cursor().col : 0 });
    scrollToLine(line, "auto");
    pendingFocus = focusRow(line) ? undefined : line;
  };

  const copy = createCopy({
    view: () => box,
    rows: () => rowsEl,
    // Read once (see CodeViewProps).
    // eslint-disable-next-line solid/reactivity
    text: props.text,
    // eslint-disable-next-line solid/reactivity
    index: props.index,
    onSelectionLines: (ls) => {
      const cur = selLines();
      if (ls.length !== cur.length || ls.some((n, i) => n !== cur[i])) {
        setSelLines(ls);
      }
    },
  });

  // eslint-disable-next-line solid/reactivity -- once, at setup
  props.ref?.({
    scrollToLine: (n, align = "center") => scrollToLine(n, align),
    focusLine,
    setCursor: (n) => setCursor({ line: clamp(n), col: 0 }),
    cursor,
    hasFocus: () => box.contains(document.activeElement),
    pin: (ls) => setExtraPins([...ls]),
    selectionLines: selLines,
    row,
  });

  // Keys on a focused line. Left and right stay native (scroll
  // sideways).
  const onKeyDown = (e: KeyboardEvent) => {
    // A key right after a click wins over the click's late check.
    clearTimeout(upTimer);
    if (
      !(e.target instanceof Element) ||
      e.target.closest(".code-cell") === null
    )
      return;
    const mods = {
      ctrl: e.ctrlKey,
      meta: e.metaKey,
      alt: e.altKey,
      shift: e.shiftKey,
    };
    if (isSelectAll(e.key, mods)) {
      e.preventDefault();
      copy.selectAll();
      return;
    }
    const page = Math.max(
      1,
      Math.floor(box.clientHeight / rowHeight.height()) - 1,
    );
    const to = keyTarget(e.key, mods, cursor().line, count(), page);
    if (to === undefined) return;
    e.preventDefault();
    focusLine(to);
  };

  // Clicks and drags. Only the cursor's cell is focusable, so
  // mousedown focuses the view, not a cell (WebKit drops the selection
  // otherwise). On mouseup: cursor to the selection's end (or the
  // clicked line), focus it. Again a task later: Firefox moves the
  // selection after mouseup when you click inside it.
  let downLine: number | undefined;
  let upTimer: ReturnType<typeof setTimeout> | undefined;
  const onMouseDown = (e: MouseEvent) => {
    const r =
      e.target instanceof Element ? e.target.closest(".code-row") : null;
    downLine = r === null ? undefined : Number(r.getAttribute("data-line"));
    // A left click on a line ends select-all at once: its selectionchange
    // can come after a quick copy. The scrollbar and a right click
    // (menu Copy) keep it.
    if (r !== null && e.button === 0) copy.endSelectAll();
  };
  const onMouseUp = () => {
    const down = downLine;
    downLine = undefined;
    clearTimeout(upTimer);
    if (down === undefined) return;
    afterClick(down);
    upTimer = setTimeout(() => afterClick(down), 0);
  };
  const afterClick = (down: number) => {
    // Focus went elsewhere meanwhile: leave it.
    if (!box.contains(document.activeElement)) return;
    const sel = getSelection();
    const end =
      sel?.focusNode == null
        ? undefined
        : domPoint(box, sel.focusNode, sel.focusOffset);
    let line = down;
    let col = 0;
    // The rowgroup itself is select-all's end, not a line the user is on.
    if (end?.kind === "at" && sel !== null && sel.focusNode !== rowsEl) {
      if (!sel.isCollapsed) {
        line = end.line;
        col = end.col;
        // A triple click ends at the next line's start: the line before.
        const start =
          sel.anchorNode === null
            ? undefined
            : domPoint(box, sel.anchorNode, sel.anchorOffset);
        const from =
          start?.kind === "at"
            ? start.line
            : start?.kind === "before"
              ? 0
              : line;
        if (col === 0 && line > from && line > 1) {
          line -= 1;
          col = Infinity;
        }
      } else if (end.line === line) {
        col = end.col;
      }
    }
    col = Math.min(col, lineText(props.text, props.index, line).length);
    setCursor({ line, col });
    pendingFocus = focusRow(line, false) ? undefined : line;
  };

  // Focus went elsewhere: a waiting focus must not take it back later.
  const onAnyFocus = (e: FocusEvent) => {
    if (!(e.target instanceof Node) || !box.contains(e.target)) {
      pendingFocus = undefined;
    }
  };
  onMount(() => {
    rowHeight.observe(probe);
    document.addEventListener("focusin", onAnyFocus);
    document.addEventListener("mouseup", onMouseUp);
    onCleanup(() => {
      document.removeEventListener("focusin", onAnyFocus);
      document.removeEventListener("mouseup", onMouseUp);
      clearTimeout(upTimer);
    });
  });

  // A new row height (the probe's first measure, zoom, fonts): keep the
  // same line at the top. The virtualiser only sees it on measure().
  let lastHeight = fallbackRowHeight;
  createEffect(() => {
    const h = rowHeight.height();
    if (h === lastHeight) return;
    const top = box.scrollTop / lastHeight;
    lastHeight = h;
    v.measure();
    // The new height first: the browser clamps scrollTop to the old one.
    setVar(rowsEl, "--code-h", `${v.getTotalSize()}px`);
    box.scrollTop = top * h;
  });
  createEffect(() => {
    setVar(box, "--code-digits", String(String(count()).length));
    setVar(box, "--code-cols", String(props.index.maxCols));
  });
  createEffect(() => setVar(rowsEl, "--code-h", `${v.getTotalSize()}px`));

  const isCursor = createSelector(() => cursor().line);
  const marked = (n: number) =>
    props.marks !== undefined && n >= props.marks.from && n <= props.marks.to;

  // One row. A row is for one line all its life.
  function lineRow(n: number): Node {
    let el!: HTMLDivElement;
    createEffect(() =>
      setVar(el, "--code-y", `${(n - 1) * rowHeight.height()}px`),
    );
    const text = lineText(props.text, props.index, n);
    return (
      <div
        ref={(e) => (el = e)}
        class="code-row"
        classList={{ "code-line-target": marked(n) }}
        role="row"
        aria-rowindex={n}
        data-line={n}
      >
        <div
          class="code-num"
          role="rowheader"
          aria-label={String(n)}
          data-n={n}
        />
        <div
          class="code-cell"
          role="gridcell"
          tabindex={isCursor(n) ? 0 : undefined}
        >
          {props.renderLine === undefined
            ? plainLine(text)
            : props.renderLine(n, text)}
        </div>
      </div>
    ) as Node;
  }

  // eslint-disable-next-line solid/reactivity -- mountRows tracks lines
  mountRows(() => rowsEl, lines, lineRow, owner);
  // A focus that waited for its row or the cell's tabindex (user
  // effects run after the DOM ones).
  const retryFocus = () => {
    if (pendingFocus !== undefined && focusRow(pendingFocus)) {
      pendingFocus = undefined;
    }
  };
  createEffect(on(cursor, retryFocus, { defer: true }));
  // After the rows changed: a waiting focus, and still all selected if
  // it was.
  createEffect(
    on(
      lines,
      () => {
        retryFocus();
        copy.afterMount();
      },
      { defer: true },
    ),
  );

  return (
    <div class="code-wrap">
      <p class="code-hint">
        Browser find (Ctrl+F) sees only the lines on screen.
      </p>
      <div
        ref={(el) => (box = el)}
        class="code-view"
        role="grid"
        aria-readonly="true"
        // Firefox makes a scroll box a Tab stop of its own: the line is
        // the one stop.
        tabindex="-1"
        aria-rowcount={count()}
        aria-label="File contents"
        onKeyDown={onKeyDown}
        onMouseDown={onMouseDown}
      >
        <div ref={(el) => (rowsEl = el)} class="code-rows" role="rowgroup" />
      </div>
      {/* Measures a row: same classes, height 1.5em. No text, and after
          the view, so it's never in a copy. */}
      <div
        ref={(el) => (probe = el)}
        class="code-row code-probe"
        aria-hidden="true"
      />
    </div>
  );
}

// Plain text, with control characters as glyphs (display.ts).
function plainLine(text: string): JSX.Element {
  return pieces(text).map((p) =>
    p.ctrl ? <span class="code-ctrl">{p.text}</span> : p.text,
  );
}
