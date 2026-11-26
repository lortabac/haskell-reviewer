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
  createSelector,
  createSignal,
  createUniqueId,
  getOwner,
  type JSX,
  on,
  onCleanup,
  onMount,
} from "solid-js";
import type { DiffSessionSide } from "../sides";
import { setVar } from "../ui/cssom";
import { bringOnPage, inBox } from "../ui/scroll";
import { createCopy } from "./copy";
import { createHoverCard } from "./createHoverCard";
import { plainPieces } from "./display";
import { focusKeepingSelection } from "./focus";
import type { FindHits } from "./find";
import {
  addedLine,
  changeWord,
  edgeAbove,
  edgeBelow,
  modifiedLine,
  removedLine,
  type SideChanges,
} from "./diffMarks";
import { createFindDraw } from "./findDraw";
import {
  isPlain,
  isSelectAll,
  keyTarget,
  landOn,
  modsOf,
  nextStop,
  pageRows,
  sideStep,
  type Spot,
} from "./keys";
import type { LineKind } from "./highlight";
import type { CardText } from "./hover";
import { type LineIndex, lineText, wholeText } from "./lines";
import { lineNodes } from "./paint";
import {
  colIn,
  copiedText,
  domPoint,
  linkStops,
  pointAt,
  selectedLines,
  selectionLines,
} from "./selection";
import {
  createPendingFocus,
  createRovingStop,
  drawCell,
  focusSpot,
  keepTopOnNewHeight,
  revealX,
  sameFields,
} from "./grid";
import {
  createRowHeight,
  fallbackRowHeight,
  mountRows,
  overscan,
  withPins,
} from "./virtual";

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
  // The lines on screen in the view (no layout: undefined).
  onScreen: () => { from: number; to: number } | undefined;
  // Focus is somewhere in the view.
  hasFocus: () => boolean;
  // Lines to keep mounted (on top of the cursor's and the selection's).
  // For find (T09) and links (T08).
  pin: (lines: readonly number[]) => void;
  // The selection's ends in the view, as lines. For T08/T09 (copy
  // location, find from the selection).
  selectionLines: () => number[];
  // The lines the selection covers (select-all: the whole file), for
  // Copy location. Undefined: no selection, or none in this view.
  selectedLines: () => { from: number; to: number } | undefined;
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

export type { FindHits } from "./find";

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
  // Diff mode (D-348): this side, and its lines' change signs once
  // known. The gutter has room for the signs whenever this is set, so
  // the text doesn't move when they come.
  changes?:
    { side: DiffSessionSide; marks: SideChanges | undefined } | undefined;
  ref?: (api: CodeApi) => void;
}

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
    if (!sameFields(cursor(), c)) setCursorRaw(c);
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
  const linksOf = linkStops;

  // Roving tabindex: the cursor's link has tabindex 0, the cell -1. One
  // Tab stop in the view.
  const roving = createRovingStop(linksOf);
  const applyStop = (cell: Element | undefined, c: Cursor) => {
    // The link went (a redraw): the cell again.
    if (!roving(cell, c)) setCursor({ ...c, link: false });
  };
  const cellOf = (n: number) =>
    row(n)?.querySelector<HTMLElement>(".code-cell") ?? undefined;

  // Scroll sideways so el isn't under the sticky gutter or past the
  // right edge.
  const scrollToReveal = (el: Element, rect?: DOMRect) => {
    const b = box.getBoundingClientRect();
    const gutter =
      el
        .closest(".code-row")
        ?.querySelector(".code-mark")
        ?.getBoundingClientRect().right ?? b.left;
    box.scrollLeft += revealX(
      rect ?? el.getBoundingClientRect(),
      gutter,
      b.left + box.clientWidth,
    );
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
      scrollToReveal(link);
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
  const pendingFocus = createPendingFocus<{
    line: number;
    want: number | Spot;
    page: boolean;
  }>(
    () => box,
    (p) => tryFocus(p.line, p.want, p.page),
  );
  const focusAt = (n: number, want: number | Spot, page = true) => {
    pendingFocus.clear();
    const line = clamp(n);
    // The cursor first: its row gets pinned (mounted).
    if (cursor().line !== line) {
      const col = typeof want === "number" ? want : want.col;
      setCursor({ line, col: Math.min(col, lineLength(line)), link: false });
    }
    if (page) scrollToLine(line, "auto");
    pendingFocus.request({ line, want, page });
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
    // props.text and props.index never change (see CodeViewProps).
    copied: (sel) => copiedText(box, props.text, props.index, sel),
    all: () => wholeText(props.text, props.index),
    onSelection: (sel) => {
      const ls = selectionLines(box, sel);
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
    onScreen: () => {
      const r = v.range;
      return r === null || count() === 0
        ? undefined
        : { from: clamp(r.startIndex + 1), to: clamp(r.endIndex + 1) };
    },
    hasFocus: () => box.contains(document.activeElement),
    pin: (ls) => setExtraPins([...ls]),
    selectionLines: selLines,
    selectedLines: () =>
      copy.isAllSelected()
        ? { from: 1, to: props.index.count }
        : selectedLines(box, getSelection(), props.index.count),
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
    const mods = modsOf(e);
    if (isSelectAll(e.key, mods)) {
      e.preventDefault();
      copy.selectAll();
      return;
    }
    const cur = cursor();
    if (isPlain(mods) && (e.key === "ArrowRight" || e.key === "ArrowLeft")) {
      e.preventDefault();
      const dir = e.key === "ArrowRight" ? 1 : -1;
      const cell = cellOf(cur.line);
      const to =
        cell === undefined ? undefined : nextStop(linksOf(cell), cur, dir);
      if (to === undefined) {
        box.scrollLeft += sideStep * dir;
        return;
      }
      goal = to.col;
      focusAt(cur.line, to, false);
      return;
    }
    const page = pageRows(box.clientHeight, rowHeight.height());
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
    const spot = focusSpot(
      t,
      cell,
      linksOf(cell),
      cur.line === line ? cur.col : 0,
    );
    if (spot === undefined) return;
    const c: Cursor = { line, ...spot };
    if (sameFields(c, cur)) return;
    goal = c.col;
    // A focus waiting for another line must not pull it back.
    if (pendingFocus.get()?.line !== line) pendingFocus.clear();
    setCursor(c);
    applyStop(cell, c);
  };
  onMount(() => {
    rowHeight.observe(probe);
    box.addEventListener("click", onClick);
    box.addEventListener("focusin", onFocusIn);
    // On window: a drag can end outside it.
    window.addEventListener("mouseup", onMouseUp);
    document.addEventListener("mousemove", onMouseMove);
    onCleanup(() => {
      window.removeEventListener("mouseup", onMouseUp);
      document.removeEventListener("mousemove", onMouseMove);
      clearTimeout(upTimer);
      endDrag();
    });
  });

  // A new row height: the same line at the top, and the outline of a
  // find hit moved with its row.
  keepTopOnNewHeight({
    height: rowHeight.height,
    fallback: fallbackRowHeight,
    box: () => box,
    rows: () => rowsEl,
    v,
    after: () => draw.repaint(),
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
    // The change word of the line, and its classes (the sign is CSS).
    const word = () => {
      const c = props.changes;
      return c?.marks === undefined
        ? undefined
        : changeWord(c.marks, n, c.side);
    };
    const own = () => props.changes?.marks?.own[n] ?? 0;
    const edge = () => props.changes?.marks?.edge[n] ?? 0;
    const classes = () => {
      const kind = decor()?.kind;
      const o = own();
      const e = edge();
      return {
        "code-chg": o > 0,
        "code-chg-added": o === addedLine,
        "code-chg-removed": o === removedLine,
        "code-chg-modified": o === modifiedLine,
        "code-edge": e > 0,
        "code-edge-below": (e & edgeBelow) !== 0,
        "code-edge-above": (e & edgeAbove) !== 0,
        // What the other side lost (head) or gained (base).
        "code-edge-added": e > 0 && props.changes?.side === "base",
        "code-line-target": marked(n),
        "code-find-line": isFindLine(n),
        "code-cursor-line": isCursor(n),
        "code-excluded": kind === "excluded",
        "code-directive": kind === "directive",
      };
    };
    const label = () => {
      const kind = decor()?.kind;
      return [
        String(n),
        word(),
        kind === undefined ? undefined : kindLabel[kind],
      ]
        .filter((x) => x !== undefined)
        .join(", ");
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
    // Children by hand: a new renderLine redraws them.
    drawCell(
      () => cell,
      // eslint-disable-next-line solid/reactivity -- drawCell tracks it
      () => {
        const render = props.renderLine;
        return render === undefined
          ? lineNodes(plainPieces(text))
          : render(n, text);
      },
      // eslint-disable-next-line solid/reactivity -- run untracked, after a draw
      (had) => {
        const c = cursor();
        if (c.line !== n) return;
        // New <a>: the stop again.
        applyStop(cell, c);
        // Focus was on a link that went: back to the cursor. Not if a
        // focus waits for another line (it takes over).
        const p = pendingFocus.get();
        if (
          had &&
          !cell.contains(document.activeElement) &&
          (p === undefined || p.line === n)
        ) {
          focusAt(n, p?.want ?? { link: cursor().link, col: c.col }, false);
        }
      },
    );
    return row;
  }

  // eslint-disable-next-line solid/reactivity -- mountRows tracks lines
  mountRows(() => rowsEl, lines, lineRow, owner);
  // Find: draw the hits of the mounted rows. Again when rows come or
  // redraw (from the effects below), or the hits change. The place
  // matters: a separate effect on lines made the Ctrl+A e2e fail once
  // (cause not found).
  const draw = createFindDraw({
    box: () => box,
    cells: function* () {
      for (const n of lines()) {
        const cell = cellOf(n);
        if (cell !== undefined) yield { cell, line: n };
      }
    },
    find: () => props.find,
    reveal: scrollToReveal,
  });
  // A focus that waited for its row or the cell's tabindex (user
  // effects run after the DOM ones).
  const retryFocus = pendingFocus.retry;
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
        draw.repaint();
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
        draw.repaint();
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
          draw.repaint();
          return;
        }
        const line = f.matches.line[i]!;
        const col = f.matches.from[i]!;
        clearTimeout(upTimer);
        goal = col;
        setCursor({ line, col, link: false });
        draw.revealNext();
        // Centre only if the row isn't on screen already.
        const r = row(line);
        if (r === undefined || !inBox(r, box)) scrollToLine(line, "center");
        draw.repaint();
      },
    ),
  );

  return (
    <div class="code-wrap">
      <div
        ref={(el) => (box = el)}
        class="code-view"
        classList={{ "code-view-changes": props.changes !== undefined }}
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
