// The source view: a file's lines as an ARIA grid, virtualised (D-70,
// SPEC §11.7). Only rows near the screen are mounted, plus pinned ones
// (cursor, selection ends). Fixed row height, no wrap.
import {
  createVirtualizer,
  type Range as VRange,
  type Virtualizer,
} from "@tanstack/solid-virtual";
import {
  createEffect,
  createMemo,
  createRenderEffect,
  createSelector,
  createSignal,
  createUniqueId,
  getOwner,
  type JSX,
  on,
  onCleanup,
  onMount,
  untrack,
} from "solid-js";
import { setVar } from "../ui/cssom";
import { bringOnPage } from "../ui/scroll";
import { createCopy } from "./copy";
import { createHoverCard } from "./createHoverCard";
import { plainPieces } from "./display";
import { focusKeepingSelection } from "./focus";
import { inLine, type Matches } from "./find";
import { createPaint, maxRangesPerRow } from "./findPaint";
import {
  isSelectAll,
  keyTarget,
  landOn,
  nextStop,
  type Spot,
  type Stop,
} from "./keys";
import type { LineKind } from "./highlight";
import type { CardText } from "./hover";
import { type LineIndex, lineText } from "./lines";
import { lineNodes } from "./paint";
import {
  colIn,
  domPoint,
  nodeAt,
  nodesAt,
  pointAt,
  replaceKeepingSelection,
} from "./selection";
import { createRowHeight, mountRows, withPins } from "./virtual";

// Lines from 1, columns in UTF-16 units from 0. link: the cursor is on
// the link starting at col (it has focus, not the cell). See D-284.
export interface Cursor {
  line: number;
  col: number;
  link: boolean;
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
  // Move the cursor to the line, at the goal column, focus it and
  // scroll to it.
  focusLine: (line: number) => void;
  // The same at a column: the link there, else the cell.
  focusAt: (line: number, col: number) => void;
  // Move the cursor, no focus change and no scroll.
  setCursor: (line: number, col?: number) => void;
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
  // The link at the cursor: the one it is on, else the one under its
  // column (g d).
  cursorLink: () => HTMLAnchorElement | undefined;
  // Esc: no selection, and select-all ends at once (a copy right after
  // gets nothing). False if there was no selection.
  clearSelection: () => boolean;
}

// More for a line: its CPP kind (a row class, and words in its
// number's label), something in the marker column.
export interface LineDecor {
  kind?: Exclude<LineKind, "code"> | undefined;
  mark?: JSX.Element;
}

const kindLabel: Record<Exclude<LineKind, "code">, string> = {
  excluded: "excluded by CPP",
  directive: "CPP directive",
};

// The hits of find (T09). current: the one to show (-1: none). A new
// object moves the view to the current hit, even if it is the same one.
export interface FindHits {
  matches: Matches;
  current: number;
}

// text and index are read once: a new file needs a new view.
export interface CodeViewProps {
  text: string;
  index: LineIndex;
  marks?: LineMarks | undefined;
  // The code of a line, as nodes (paint.ts); undefined: plain text.
  // Its text must be the line's, 1:1 in UTF-16 (copy reads columns
  // from the DOM). A new function redraws the mounted rows, keeping
  // the selection.
  renderLine?: ((line: number, text: string) => Node[]) | undefined;
  lineDecor?: ((line: number) => LineDecor | undefined) | undefined;
  // The hover card of a link word (D-285); undefined: none.
  card?: ((link: number) => CardText | undefined) | undefined;
  // Hits to draw; undefined: none.
  find?: FindHits | undefined;
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
  let cardEl!: HTMLDivElement;
  const cardId = createUniqueId();
  const owner = getOwner();
  const count = () => props.index.count;
  const clamp = (n: number) => Math.min(Math.max(1, n), Math.max(1, count()));
  const rowHeight = createRowHeight(fallbackRowHeight);
  const [cursor, setCursorRaw] = createSignal<Cursor>({
    line: 1,
    col: 0,
    link: false,
  });
  // Where ↑ and ↓ aim (UTF-16), kept over short lines.
  let goal = 0;
  const [extraPins, setExtraPins] = createSignal<readonly number[]>([]);
  const [selLines, setSelLines] = createSignal<number[]>([]);
  const pins = createMemo(() =>
    [cursor().line, ...selLines(), ...extraPins()].map((n) => n - 1),
  );

  const setCursor = (c: Cursor) => {
    const cur = cursor();
    if (cur.line !== c.line || cur.col !== c.col || cur.link !== c.link) {
      setCursorRaw(c);
    }
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
      setCursor({ line: clamp(r.startIndex + 1), col: 0, link: false });
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
      return (r: VRange) => withPins(r, p);
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
  const lineLength = (n: number) => lineText(props.text, props.index, n).length;

  // The links of a cell, read from the DOM: the stops of → and ←.
  const linksOf = (cell: Element): (Stop & { el: HTMLElement })[] =>
    [...cell.querySelectorAll<HTMLElement>("a.code-link")].map((el) => {
      const start = colIn(cell, el, 0);
      return { start, end: start + el.textContent.length, el };
    });

  // Roving tabindex: the cursor's link has tabindex 0 (set by hand: the
  // rows are drawn by hand), the cell -1. One Tab stop in the view.
  let stopEl: HTMLElement | undefined;
  const applyStop = (cell: Element | undefined, c: Cursor) => {
    let el: HTMLElement | undefined;
    if (c.link && cell !== undefined) {
      el = linksOf(cell).find((s) => s.start === c.col)?.el;
      // The link went (a redraw): the cell again.
      if (el === undefined) setCursor({ ...c, link: false });
    }
    if (stopEl !== undefined && stopEl !== el) stopEl.tabIndex = -1;
    stopEl = el;
    if (el !== undefined) el.tabIndex = 0;
  };
  const cellOf = (n: number) =>
    row(n)?.querySelector<HTMLElement>(".code-cell") ?? undefined;

  // Scroll sideways so el isn't under the sticky gutter or past the
  // right edge.
  const revealX = (el: Element, rect?: DOMRect) => {
    const b = box.getBoundingClientRect();
    const r = rect ?? el.getBoundingClientRect();
    const gutter =
      el
        .closest(".code-row")
        ?.querySelector(".code-mark")
        ?.getBoundingClientRect().right ?? b.left;
    const right = b.left + box.clientWidth;
    if (r.left < gutter) box.scrollLeft -= gutter - r.left;
    else if (r.right > right) {
      box.scrollLeft += Math.min(r.right - right, r.left - gutter);
    }
  };

  // Focus line n at want: a column (the link there, else the cell) or
  // an exact spot. False if the row isn't there yet.
  const tryFocus = (n: number, want: number | Spot, page: boolean) => {
    const r = row(n);
    const cell = r?.querySelector<HTMLElement>(".code-cell");
    if (r === undefined || cell == null) return false;
    const links = linksOf(cell);
    const spot = typeof want === "number" ? landOn(links, want) : want;
    const link = spot.link
      ? links.find((s) => s.start === spot.col)?.el
      : undefined;
    const c: Cursor = {
      line: n,
      col: Math.min(spot.col, lineLength(n)),
      link: link !== undefined,
    };
    setCursor(c);
    applyStop(cell, c);
    if (link !== undefined) {
      // In view first, then the card (placed there), then focus: the
      // description is there when focus comes.
      revealX(link);
      if (page) bringOnPage(r);
      hover.beforeFocus(link);
      focusKeepingSelection(link);
    } else {
      // Not the cursor's cell yet (inside an effect): wait.
      if (!cell.hasAttribute("tabindex")) return false;
      focusKeepingSelection(cell);
      if (page) bringOnPage(r);
    }
    return true;
  };
  // The cursor's row is pinned, so it's mounted and its cell focusable
  // once the cursor moves: at once in an event handler, after the
  // effects inside an effect (then pendingFocus does it).
  let pendingFocus:
    { line: number; want: number | Spot; page: boolean } | undefined;
  const focusAt = (n: number, want: number | Spot, page = true) => {
    pendingFocus = undefined;
    const line = clamp(n);
    // The cursor first: its row gets pinned (mounted).
    if (cursor().line !== line) {
      const col = typeof want === "number" ? want : want.col;
      setCursor({ line, col: Math.min(col, lineLength(line)), link: false });
    }
    if (page) scrollToLine(line, "auto");
    if (!tryFocus(line, want, page)) pendingFocus = { line, want, page };
  };
  const focusLine = (n: number) => focusAt(n, goal);

  const hover = createHoverCard({
    view: () => box,
    card: () => cardEl,
    row,
    text: (k) => props.card?.(k),
    dragging: () => linkDrag !== undefined,
  });

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
    focusLine: (n) => {
      clearTimeout(upTimer);
      focusLine(n);
    },
    focusAt: (n, col) => {
      // A move now wins over a click's late check.
      clearTimeout(upTimer);
      goal = col;
      focusAt(n, col);
    },
    setCursor: (n, col = 0) => {
      clearTimeout(upTimer);
      const line = clamp(n);
      goal = col;
      setCursor({ line, col: Math.min(col, lineLength(line)), link: false });
    },
    cursor,
    hasFocus: () => box.contains(document.activeElement),
    pin: (ls) => setExtraPins([...ls]),
    selectionLines: selLines,
    row,
    cursorLink: () => {
      const c = cursor();
      const cell = cellOf(c.line);
      if (cell === undefined) return undefined;
      const s = linksOf(cell).find((x) =>
        c.link ? x.start === c.col : x.start <= c.col && c.col < x.end,
      );
      return s?.el instanceof HTMLAnchorElement ? s.el : undefined;
    },
    clearSelection: () => {
      copy.endSelectAll();
      const sel = getSelection();
      if (sel === null || sel.isCollapsed) return false;
      sel.removeAllRanges();
      return true;
    },
  });

  // Keys on a focused line or link. See D-284.
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
    const cur = cursor();
    const plain = !mods.ctrl && !mods.meta && !mods.alt && !mods.shift;
    if (plain && (e.key === "ArrowRight" || e.key === "ArrowLeft")) {
      e.preventDefault();
      const dir = e.key === "ArrowRight" ? 1 : -1;
      const cell = cellOf(cur.line);
      const to =
        cell === undefined ? undefined : nextStop(linksOf(cell), cur, dir);
      if (to === undefined) {
        box.scrollLeft += 40 * dir;
        return;
      }
      goal = to.col;
      focusAt(cur.line, to, false);
      return;
    }
    const page = Math.max(
      1,
      Math.floor(box.clientHeight / rowHeight.height()) - 1,
    );
    const to = keyTarget(e.key, mods, cur.line, count(), page);
    if (to === undefined) return;
    e.preventDefault();
    if (e.key === "Home" || e.key === "End") goal = 0;
    focusAt(to, goal);
  };

  // Clicks and drags. Only the cursor's cell is focusable, so
  // mousedown focuses the view, not a cell (WebKit drops the selection
  // otherwise). On mouseup: cursor to the selection's end (or the
  // clicked line), focus it (the link there, else the cell). Again a
  // task later: Firefox moves the selection after mouseup when you
  // click inside it.
  let downLine: number | undefined;
  // The link the mousedown was on, if any.
  let downLink: HTMLElement | undefined;
  let upTimer: ReturnType<typeof setTimeout> | undefined;
  const onMouseDown = (e: MouseEvent) => {
    const r =
      e.target instanceof Element ? e.target.closest(".code-row") : null;
    downLine = r === null ? undefined : Number(r.getAttribute("data-line"));
    downLink =
      e.target instanceof Element
        ? (e.target.closest<HTMLElement>("a.code-link") ?? undefined)
        : undefined;
    // A left click on a line ends select-all at once: its selectionchange
    // can come after a quick copy. The scrollbar and a right click
    // (menu Copy) keep it.
    if (r !== null && e.button === 0) copy.endSelectAll();
    // On a link, Chromium and WebKit neither start a selection (a drag)
    // nor extend one (Shift+click): do both by hand. Firefox does it
    // natively, but no feature check tells, so one path for all.
    const sel = getSelection();
    const onLink =
      e.target instanceof Element && e.target.closest("a.code-link") !== null;
    if (!onLink || sel === null || e.button !== 0 || e.detail > 1) return;
    if (e.ctrlKey || e.metaKey || e.altKey) return;
    const p = pointAt(e.clientX, e.clientY);
    if (p === undefined || !box.contains(p[0])) return;
    e.preventDefault();
    // No native focus now: the view, as for a click on a line.
    if (!box.contains(document.activeElement)) focusKeepingSelection(box);
    // Shift with no selection: put the caret here (onClick still
    // stops a Shift+click from following the link).
    if (e.shiftKey && sel.rangeCount > 0) {
      sel.extend(p[0], p[1]);
      return;
    }
    sel.collapse(p[0], p[1]);
    linkDrag = { x: e.clientX, y: e.clientY };
    // Focus left the window mid-drag (Alt+Tab): no mouseup will come.
    window.addEventListener("blur", endDrag);
  };
  // A drag that started on a link: the selection's end follows the
  // pointer, and the view scrolls while it is above or below it.
  let linkDrag: { x: number; y: number } | undefined;
  let dragTimer: ReturnType<typeof setInterval> | undefined;
  const extendTo = (x: number, y: number) => {
    const b = box.getBoundingClientRect();
    // Clamped into the view, scrollbars out: the edge row when outside.
    const left = b.left + box.clientLeft;
    const top = b.top + box.clientTop;
    const cx = Math.min(Math.max(x, left + 1), left + box.clientWidth - 2);
    const cy = Math.min(Math.max(y, top + 1), top + box.clientHeight - 2);
    const p = pointAt(cx, cy);
    const sel = getSelection();
    if (p !== undefined && box.contains(p[0]) && sel !== null) {
      sel.extend(p[0], p[1]);
    }
  };
  const dragStep = () => {
    const d = linkDrag;
    if (d === undefined) return;
    const b = box.getBoundingClientRect();
    const dy = d.y < b.top ? -1 : d.y > b.bottom ? 1 : 0;
    if (dy === 0) {
      clearInterval(dragTimer);
      dragTimer = undefined;
      return;
    }
    box.scrollTop += dy * rowHeight.height();
    extendTo(d.x, d.y);
  };
  const onMouseMove = (e: MouseEvent) => {
    if (linkDrag === undefined) return;
    // The button went up where we didn't see it.
    if ((e.buttons & 1) === 0) {
      onMouseUp();
      return;
    }
    linkDrag = { x: e.clientX, y: e.clientY };
    extendTo(e.clientX, e.clientY);
    const b = box.getBoundingClientRect();
    const out = e.clientY < b.top || e.clientY > b.bottom;
    if (out && dragTimer === undefined) dragTimer = setInterval(dragStep, 50);
  };
  function endDrag(): void {
    window.removeEventListener("blur", endDrag);
    linkDrag = undefined;
    clearInterval(dragTimer);
    dragTimer = undefined;
  }
  const onMouseUp = (e?: MouseEvent) => {
    if (linkDrag !== undefined && e !== undefined) {
      extendTo(e.clientX, e.clientY);
    }
    endDrag();
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
    col = Math.min(col, lineLength(line));
    goal = col;
    // A click on a link (even past its last character's middle, where
    // the caret is at its end): that link.
    const a = downLink;
    const cell = a?.closest(".code-cell");
    if (
      sel?.isCollapsed !== false &&
      a !== undefined &&
      a.isConnected &&
      cell != null &&
      line === down
    ) {
      const start = colIn(cell, a, 0);
      goal = start;
      focusAt(line, { link: true, col: start }, false);
      return;
    }
    focusAt(line, col, false);
  };

  // Focus went elsewhere: a waiting focus must not take it back later.
  const onAnyFocus = (e: FocusEvent) => {
    if (!(e.target instanceof Node) || !box.contains(e.target)) {
      pendingFocus = undefined;
    }
  };
  // A drag or Shift+click that ends on a link selects, it doesn't
  // follow the link. Native, not delegated: before the router's.
  // Ctrl, Cmd or Alt clicks (a new tab) are the browser's.
  const onClick = (e: MouseEvent) => {
    const a =
      e.target instanceof Element ? e.target.closest("a.code-link") : null;
    if (a === null || e.detail === 0 || e.button !== 0) return;
    if (e.ctrlKey || e.metaKey || e.altKey) return;
    const sel = getSelection();
    const ours =
      sel !== null &&
      !sel.isCollapsed &&
      sel.anchorNode !== null &&
      box.contains(sel.anchorNode) &&
      sel.focusNode !== null &&
      box.contains(sel.focusNode);
    if (e.shiftKey || ours) e.preventDefault();
    // Followed: the late check must not take focus back from the jump.
    else clearTimeout(upTimer);
  };
  // Focus that came without focusAt (a screen reader, a script): the
  // cursor follows it, so the roving tabindex stays right.
  const onFocusIn = (e: FocusEvent) => {
    const t = e.target;
    if (!(t instanceof HTMLElement)) return;
    const cell = t.closest(".code-cell");
    const r = t.closest(".code-row");
    if (cell === null || r === null) return;
    const line = Number(r.getAttribute("data-line"));
    const cur = cursor();
    let c: Cursor;
    if (t.matches("a.code-link")) {
      c = { line, col: colIn(cell, t, 0), link: true };
    } else if (t === cell) {
      c = { line, col: cur.line === line ? cur.col : 0, link: false };
    } else return;
    if (c.line === cur.line && c.col === cur.col && c.link === cur.link) {
      return;
    }
    goal = c.col;
    // A focus waiting for another line must not pull it back.
    if (pendingFocus !== undefined && pendingFocus.line !== line) {
      pendingFocus = undefined;
    }
    setCursor(c);
    applyStop(cell, c);
  };
  onMount(() => {
    rowHeight.observe(probe);
    box.addEventListener("click", onClick);
    box.addEventListener("focusin", onFocusIn);
    document.addEventListener("focusin", onAnyFocus);
    // On window: a drag can end outside it.
    window.addEventListener("mouseup", onMouseUp);
    document.addEventListener("mousemove", onMouseMove);
    onCleanup(() => {
      document.removeEventListener("focusin", onAnyFocus);
      window.removeEventListener("mouseup", onMouseUp);
      document.removeEventListener("mousemove", onMouseMove);
      clearTimeout(upTimer);
      endDrag();
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
    // The outline of a find hit moved with its row.
    repaint();
  });
  createEffect(() => {
    setVar(box, "--code-digits", String(String(count()).length));
    setVar(box, "--code-cols", String(props.index.maxCols));
  });
  createEffect(() => setVar(rowsEl, "--code-h", `${v.getTotalSize()}px`));

  const isCursor = createSelector(() => cursor().line);
  const isFindLine = createSelector(() => {
    const f = props.find;
    return f === undefined ? 0 : (f.matches.line[f.current] ?? 0);
  });
  const onLink = createMemo(() => cursor().link);
  const marked = (n: number) =>
    props.marks !== undefined && n >= props.marks.from && n <= props.marks.to;

  // One row. A row is for one line all its life.
  function lineRow(n: number): Node {
    let el!: HTMLDivElement;
    let cell!: HTMLDivElement;
    createEffect(() =>
      setVar(el, "--code-y", `${(n - 1) * rowHeight.height()}px`),
    );
    const text = lineText(props.text, props.index, n);
    const decor = createMemo(() => props.lineDecor?.(n));
    const classes = () => {
      const kind = decor()?.kind;
      return {
        "code-line-target": marked(n),
        "code-find-line": isFindLine(n),
        "code-excluded": kind === "excluded",
        "code-directive": kind === "directive",
      };
    };
    const label = () => {
      const kind = decor()?.kind;
      return kind === undefined ? String(n) : `${n}, ${kindLabel[kind]}`;
    };
    const row = (
      <div
        ref={(e) => (el = e)}
        class="code-row"
        classList={classes()}
        role="row"
        aria-rowindex={n}
        data-line={n}
      >
        <div
          class="code-num"
          role="rowheader"
          aria-label={label()}
          data-n={n}
        />
        {/* Markers (outline, CPP), never text: user-select none. */}
        <div class="code-mark" role="gridcell">
          {decor()?.mark}
        </div>
        <div
          ref={(e) => (cell = e)}
          class="code-cell"
          role="gridcell"
          tabindex={isCursor(n) ? (onLink() ? -1 : 0) : undefined}
        />
      </div>
    ) as Node;
    // Children by hand: a new renderLine swaps them under a live
    // selection, which must stay where it was.
    createRenderEffect(() => {
      const render = props.renderLine;
      const nodes =
        render === undefined ? lineNodes(plainPieces(text)) : render(n, text);
      const active = document.activeElement;
      const had = active !== null && cell.contains(active);
      replaceKeepingSelection(cell, nodes);
      untrack(() => {
        const c = cursor();
        if (c.line !== n) return;
        // New <a>: the stop again.
        applyStop(cell, c);
        // Focus was on a link that went: back to the cursor. Not if a
        // focus waits for another line (it takes over).
        const p = pendingFocus;
        if (
          had &&
          !cell.contains(document.activeElement) &&
          (p === undefined || p.line === n)
        ) {
          focusAt(n, p?.want ?? { link: cursor().link, col: c.col }, false);
        }
      });
    });
    return row;
  }

  // eslint-disable-next-line solid/reactivity -- mountRows tracks lines
  mountRows(() => rowsEl, lines, lineRow, owner);
  // Find: draw the hits of the mounted rows. Again when rows come or
  // redraw (from the effects below), or the hits change. The place
  // matters: a separate effect on lines made the Ctrl+A e2e fail once
  // (cause not found).
  const paint = createPaint();
  onCleanup(paint.clear);
  // The outline of the current hit: boxes in the scroll box, outside
  // the cells (so not in a copy), in content coordinates.
  const boxes = document.createElement("div");
  boxes.className = "code-find-boxes";
  boxes.setAttribute("aria-hidden", "true");
  const outline = (r: Range | undefined) => {
    boxes.replaceChildren();
    if (r === undefined || typeof r.getClientRects !== "function") return;
    // One box: rows don't wrap. The union of the rects with a width
    // (a hit that starts on a token border gives an empty one, and a
    // link gives its own rect too).
    let left = Infinity;
    let top = Infinity;
    let right = -Infinity;
    let bottom = -Infinity;
    for (const q of r.getClientRects()) {
      if (q.width <= 0) continue;
      left = Math.min(left, q.left);
      top = Math.min(top, q.top);
      right = Math.max(right, q.right ?? q.left + q.width);
      bottom = Math.max(bottom, q.bottom ?? q.top + q.height);
    }
    if (left === Infinity) return;
    const b = box.getBoundingClientRect();
    const el = document.createElement("div");
    el.className = "code-find-box";
    const x = left - b.left - box.clientLeft + box.scrollLeft;
    const y = top - b.top - box.clientTop + box.scrollTop;
    setVar(el, "--box-x", `${x}px`);
    setVar(el, "--box-y", `${y}px`);
    setVar(el, "--box-w", `${right - left}px`);
    setVar(el, "--box-h", `${bottom - top}px`);
    boxes.append(el);
  };
  onMount(() => box.append(boxes));
  // The view moves to the current hit, once its row is drawn.
  let reveal = false;
  const repaint = () => {
    const f = props.find;
    if (f === undefined) {
      paint.clear();
      outline(undefined);
      return;
    }
    const m = f.matches;
    const ranges: Range[] = [];
    let cur: Range | undefined;
    const range = (a: [Node, number], z: [Node, number]) => {
      const r = document.createRange();
      r.setStart(...a);
      r.setEnd(...z);
      return r;
    };
    for (const n of lines()) {
      const cell = cellOf(n);
      if (cell === undefined) continue;
      const [lo, hi] = inLine(m, n);
      const end = Math.min(hi, lo + maxRangesPerRow);
      const cols: number[] = [];
      for (let i = lo; i < end; i++) cols.push(m.from[i]!, m.to[i]!);
      const at = nodesAt(cell, cols);
      for (let i = lo; i < end; i++) {
        const r = range(at[2 * (i - lo)]!, at[2 * (i - lo) + 1]!);
        if (i === f.current) cur = r;
        else ranges.push(r);
      }
      // The current one is always drawn, past the cap too.
      if (f.current >= end && f.current < hi) {
        cur = range(
          nodeAt(cell, m.from[f.current]!),
          nodeAt(cell, m.to[f.current]!),
        );
      }
    }
    paint.set(ranges, cur);
    outline(cur);
    if (reveal && cur !== undefined) {
      reveal = false;
      const cell = cellOf(m.line[f.current]!);
      if (cell !== undefined) {
        revealX(cell, cur.getBoundingClientRect());
        // It may have moved the view: the boxes again.
        outline(cur);
      }
    }
  };
  // A focus that waited for its row or the cell's tabindex (user
  // effects run after the DOM ones).
  const retryFocus = () => {
    const p = pendingFocus;
    if (p !== undefined && tryFocus(p.line, p.want, p.page)) {
      pendingFocus = undefined;
    }
  };
  createEffect(
    on(
      cursor,
      (c) => {
        applyStop(cellOf(c.line), c);
        retryFocus();
      },
      { defer: true },
    ),
  );
  // After the rows changed: a waiting focus, and still all selected if
  // it was.
  createEffect(
    on(
      lines,
      () => {
        retryFocus();
        copy.afterMount();
        repaint();
      },
      { defer: true },
    ),
  );
  // After the rows redrew (render effects run first).
  createEffect(
    on(
      () => props.renderLine,
      () => {
        hover.relink();
        repaint();
      },
      { defer: true },
    ),
  );

  // A new hit: the cursor goes there (before the scroll, so the view
  // doesn't move it), no focus change.
  createEffect(
    on(
      () => props.find,
      (f) => {
        const i = f?.current ?? -1;
        if (f === undefined || i < 0) {
          repaint();
          return;
        }
        const line = f.matches.line[i]!;
        const col = f.matches.from[i]!;
        clearTimeout(upTimer);
        goal = col;
        setCursor({ line, col, link: false });
        reveal = true;
        // Centre only if the row isn't on screen already.
        const r = row(line);
        const b = box.getBoundingClientRect();
        const rr = r?.getBoundingClientRect();
        // Inside the borders and the scrollbar.
        const top = b.top + box.clientTop;
        const bottom = top + box.clientHeight;
        if (rr === undefined || rr.top < top || rr.bottom > bottom) {
          scrollToLine(line, "center");
        }
        repaint();
      },
    ),
  );

  return (
    <div class="code-wrap">
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
      {/* The hover card: over the rows, outside the grid and its
          copy. Text only. */}
      <div
        ref={(el) => (cardEl = el)}
        id={cardId}
        class="code-card"
        role="tooltip"
        hidden
      />
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
