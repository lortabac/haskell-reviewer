// The diff as an ARIA grid, virtualised like the source view (D-70,
// D-341): fixed row height, only rows near the screen mounted (plus
// the cursor's), rows never moved (mountRows, keyed by row). Gap rows
// stand for hidden equal lines, with buttons to show them.
//
// Two layouts over one row space (split.ts, D-344). Unified: one code
// cell a row; removed rows show base's text, the rest head's. Split:
// a base cell and a head cell a row. The cursor is on one cell: a row
// key and a side.
import {
  createVirtualizer,
  type Range as VRange,
} from "@tanstack/solid-virtual";
import {
  createEffect,
  createMemo,
  createSelector,
  createSignal,
  getOwner,
  Index,
  type JSX,
  on,
  onCleanup,
  onMount,
  untrack,
} from "solid-js";
import { setVar } from "../ui/cssom";
import { bringOnPage } from "../ui/scroll";
import { plainPieces } from "../source/display";
import { createFindDraw } from "../source/findDraw";
import { focusKeepingSelection } from "../source/focus";
import {
  createPendingFocus,
  createRovingStop,
  drawCell,
  focusSpot,
  keepTopOnNewHeight,
  revealX,
  sameFields,
} from "../source/grid";
import {
  isPlain,
  keyTarget,
  landOn,
  modsOf,
  nextStop,
  pageRows,
  sideStep,
  type Spot,
  type StopAt,
} from "../source/keys";
import { type LineIndex, lineText } from "../source/lines";
import { lineNodes } from "../source/paint";
import { colIn, linkStops } from "../source/selection";
import {
  createRowHeight,
  fallbackRowHeight,
  mountRows,
  overscan,
  withPins,
} from "../source/virtual";
import { type CellSpace, cellLine, type DiffHits, isFor } from "./find";
import { plainScroll, sharedScroll, type SideScroll } from "./hscroll";
import {
  allShown,
  buttonText,
  deleteRow,
  expand,
  type Expand,
  gapButtons,
  gapKey,
  gapLabel,
  indexOfKey,
  indexOfRow,
  insertRow,
  isGapKey,
  keyAt,
  reveal,
  rowLabel,
  rowOfKey,
  type Rows,
  step,
  visibleOf,
} from "./rows";
import { type DiffSessionSide, otherSide } from "../sides";
import {
  type DiffLayout,
  lineOn,
  noLine,
  type RowSpace,
  sideOfRow,
  splitCellLabel,
  type Split,
  splitSpace,
  unifiedSpace,
} from "./split";

const diffSides: readonly DiffSessionSide[] = ["base", "head"];
const noClip = { left: -Infinity, right: Infinity };

// One side's text and its lines.
export interface SideText {
  text: string;
  index: LineIndex;
}

// key: a row key (rows.ts keyAt) over display rows. side: the cell's
// (unified: from the row, removed is base). link: on a stop rather
// than the cell. In a line row a stop is a link and col its UTF-16
// column (from 0). In a gap row a stop is a button, col i + 1 for the
// i-th; on the cell col is 0. This way keys.ts nextStop and landOn
// work on both.
interface Cursor {
  key: number;
  side: DiffSessionSide;
  col: number;
  link: boolean;
}

// Where the cursor was, to carry it over a layout switch. row: a
// unified row (a gap: its first). offset: the cursor row's distance
// from the box's top, px. focus: the grid had focus (or was about to).
export interface GridState {
  row: number;
  side: DiffSessionSide;
  col: number;
  link: boolean;
  offset: number;
  focus: boolean;
}

export interface DiffApi {
  // Focus a unified row at a column (the link there, else the cell),
  // and bring it into view. A hidden row: its gap. side: split only;
  // default from the row (removed: base, else head).
  focusRow: (row: number, col: number, side?: DiffSessionSide) => void;
  // Move the cursor, no focus change and no scroll. col: on the row's
  // line (0 on a gap).
  setCursorRow: (row: number, side?: DiffSessionSide, col?: number) => void;
  scrollToRow: (row: number, align?: "auto" | "center" | "start") => void;
  // Show a hidden row and a few around it.
  reveal: (row: number) => void;
  hasFocus: () => boolean;
  // The cursor:
  // - row: its unified row; a gap: its first row; an empty cell: the
  //   other side's row;
  // - side: the side of that row's line;
  // - lo, hi: the first and last unified rows of its display row (a
  //   split row may hold two);
  // - col: its column, 0 on a gap;
  // - gap: on a gap; first: on the first visible item.
  cursor: () => {
    row: number;
    side: DiffSessionSide;
    lo: number;
    hi: number;
    col: number;
    gap: boolean;
    first: boolean;
  };
  // The mounted element of a unified row, if any.
  rowEl: (row: number) => HTMLElement | undefined;
}

export interface DiffGridProps {
  path: string;
  rows: Rows;
  base: SideText;
  head: SideText;
  // Read once: a new layout is a new grid.
  layout: DiffLayout;
  // The rows' Split (split.ts splitOf). Read once.
  split: Split;
  // Shown unified rows (rows.ts); kept by the page over layout
  // switches.
  shown: Uint8Array;
  setShown: (s: Uint8Array) => void;
  // The nodes of a line on that side (paint.ts), as CodeView's
  // renderLine; undefined: plain. A new function redraws that side's
  // rows.
  renderBase?: ((n: number, text: string) => Node[]) | undefined;
  renderHead?: ((n: number, text: string) => Node[]) | undefined;
  // Show every row.
  whole: boolean;
  // The row and side of the hash, if any. Unified marks the row,
  // split the cell's half.
  marked?: { row: number; side: DiffSessionSide } | undefined;
  // Find hits to draw (diff/find.ts); others' rows or layout: none.
  find?: DiffHits | undefined;
  // Where the cursor starts (after a layout switch). Read once.
  start?: GridState | undefined;
  // When the grid goes: where its cursor was.
  onLeave?: (s: GridState) => void;
  ref?: (api: DiffApi) => void;
}

export function DiffGrid(props: DiffGridProps): JSX.Element {
  let box!: HTMLDivElement;
  let rowsEl!: HTMLDivElement;
  let probe!: HTMLDivElement;
  const owner = getOwner();
  // Read once: a new diff or layout needs a new grid. DiffView mounts
  // it only with rows, so the visible list is never empty.
  const { rows, layout, carried, rowsSplit } = untrack(() => ({
    rows: props.rows,
    layout: props.layout,
    carried: props.start,
    rowsSplit: props.split,
  }));
  const space: RowSpace =
    layout === "split" ? splitSpace(rowsSplit) : unifiedSpace(rows);
  // How find counts this grid's cells (diff/find.ts).
  const cells: CellSpace = {
    rows,
    split: layout === "split" ? rowsSplit : undefined,
  };
  const split = space.split;
  // Unified: numbers, sign, code. Split: number and code a side.
  const colCount = split ? 4 : 3;
  const rowHeight = createRowHeight(fallbackRowHeight);
  const whole = createMemo(() => props.whole);
  const vis = createMemo(() =>
    visibleOf(space.shown(whole() ? allShown(rows) : props.shown)),
  );
  const count = () => vis().length;

  // The unified row in a cell; -1: empty or a gap.
  const cellRow = (k: number, side: DiffSessionSide) =>
    isGapKey(k) ? -1 : space.rowOf(k, side);
  // An item's side: its first row's (sideOfRow; a gap's rows are
  // equal: head).
  const sideOfKey = (k: number) => sideOfRow(rows, space.first(rowOfKey(k)));
  // A cell's side: split keeps the one asked for, unified's is the
  // item's.
  const sideAt = (k: number, want: DiffSessionSide): DiffSessionSide =>
    split ? want : sideOfKey(k);
  const textAt = (k: number, side: DiffSessionSide) => {
    const r = cellRow(k, side);
    if (r < 0) return "";
    const t = side === "base" ? props.base : props.head;
    return lineText(t.text, t.index, lineOn(rows, r, side));
  };
  const lineLength = (k: number, side: DiffSessionSide) =>
    textAt(k, side).length;
  // The key of the item a display row is in now.
  const keyOfDisplay = (d: number) =>
    keyAt(vis(), Math.max(0, indexOfRow(vis(), d)));
  const keyOfRow = (r: number) => keyOfDisplay(space.displayOf(r));

  const startCursor = (): Cursor => {
    if (carried === undefined) {
      const k = keyAt(vis(), 0);
      return { key: k, side: sideOfKey(k), col: 0, link: false };
    }
    const k = keyOfRow(carried.row);
    const same = !isGapKey(k);
    return {
      key: k,
      side: sideAt(k, carried.side),
      col: same ? carried.col : 0,
      link: same && carried.link,
    };
  };
  const [rawCursor, setRawCursor] = createSignal<Cursor>(untrack(startCursor));
  // The cursor's item: its row's, or the gap the row is hidden in.
  const curIdx = createMemo(() =>
    Math.max(0, indexOfRow(vis(), rowOfKey(rawCursor().key))),
  );
  const cursor = createMemo((): Cursor => {
    const c = rawCursor();
    const k = keyAt(vis(), curIdx());
    return k === c.key
      ? c
      : { key: k, side: sideAt(k, c.side), col: 0, link: false };
  });
  const setCursor = (c: Cursor) => {
    if (!sameFields(rawCursor(), c)) setRawCursor(c);
  };
  // Where ↑ and ↓ aim (UTF-16), kept over short lines.
  let goal = untrack(rawCursor).col;

  const follow = () => {
    const r = v.range;
    if (!v.isScrolling || r === null || box.contains(document.activeElement))
      return;
    const i = curIdx();
    if (i < r.startIndex || i > r.endIndex) {
      const k = keyAt(vis(), r.startIndex);
      setCursor({
        key: k,
        side: sideAt(k, cursor().side),
        col: 0,
        link: false,
      });
    }
  };
  const v = createVirtualizer<HTMLDivElement, HTMLDivElement>({
    get count() {
      return count();
    },
    getScrollElement: () => box,
    estimateSize: () => rowHeight.height(),
    overscan,
    get rangeExtractor() {
      // eslint-disable-next-line solid/reactivity -- read again on each change
      const p = [curIdx()];
      return (r: VRange) => withPins(r, p);
    },
    onChange: follow,
  });
  // The keys to mount, in order. The virtualiser may lag a list that
  // just shrank: indexes past the end are dropped.
  const keys = createMemo(() => {
    const vs = vis();
    return v
      .getVirtualItems()
      .map((i) => i.index)
      .filter((i) => i < vs.length)
      .map((i) => keyAt(vs, i));
  });

  const rowByKey = (k: number) =>
    rowsEl.querySelector<HTMLElement>(`.code-row[data-key="${k}"]`) ??
    undefined;
  // A gap has one cell, with no side.
  const cellOf = (k: number, side: DiffSessionSide) =>
    rowByKey(k)?.querySelector<HTMLElement>(
      isGapKey(k) ? ".code-cell" : `.code-cell[data-side="${side}"]`,
    ) ?? undefined;
  const sideOfCell = (
    cell: Element,
    fallback: DiffSessionSide,
  ): DiffSessionSide => {
    const s = cell instanceof HTMLElement ? cell.dataset.side : undefined;
    return s === "base" || s === "head" ? s : fallback;
  };
  const scrollToIdx = (i: number, align: "auto" | "center" | "start") => {
    v.scrollToIndex(Math.min(Math.max(0, i), count() - 1), { align });
  };

  // Sideways (hscroll.ts): set on mount.
  let hx: SideScroll | undefined;
  // Bring a box in el's cell into view (focus doesn't scroll).
  const revealIn = (el: Element, r: { left: number; right: number }) => {
    if (hx === undefined) return;
    const b = hx.bounds(el);
    hx.by(revealX(r, b.left, b.right));
  };
  // A cell's start, or the end of its text with as much text before
  // it as fits.
  const revealEdge = (cell: HTMLElement, end: boolean) => {
    if (hx === undefined) return;
    const c = cell.getBoundingClientRect();
    if (!end) {
      revealIn(cell, { left: c.left, right: c.left + 1 });
      return;
    }
    let x = c.left;
    if (cell.firstChild !== null) {
      const range = document.createRange();
      range.selectNodeContents(cell);
      const t = range.getBoundingClientRect();
      if (t.width > 0) x = t.right;
    }
    const b = hx.bounds(cell);
    const room = b.right - b.left;
    revealIn(cell, { left: Math.max(c.left, x - room + 1), right: x });
  };

  // A cell's stops for → and ←: links in a line, buttons in a gap.
  const stopsOf = (cell: Element): StopAt[] =>
    cell.classList.contains("diff-gap-cell")
      ? [...cell.querySelectorAll<HTMLElement>("button")].map((el, i) => ({
          start: i + 1,
          end: i + 2,
          el,
        }))
      : linkStops(cell);

  // Roving tabindex, as CodeView: the cursor's stop has tabindex 0.
  const roving = createRovingStop(stopsOf);
  const applyStop = (cell: Element | undefined, c: Cursor) => {
    if (!roving(cell, c)) setCursor({ ...c, link: false });
  };

  // want: a goal column (a gap row: its cell, never a button), or an
  // exact spot.
  const tryFocus = (
    k: number,
    side: DiffSessionSide,
    want: number | Spot,
    page: boolean,
  ) => {
    const r = rowByKey(k);
    const cell = cellOf(k, side);
    if (r === undefined || cell === undefined) return false;
    const stops = stopsOf(cell);
    const spot =
      typeof want !== "number"
        ? want
        : isGapKey(k)
          ? { link: false, col: 0 }
          : landOn(stops, want);
    const el = spot.link
      ? stops.find((s) => s.start === spot.col)?.el
      : undefined;
    const c: Cursor = {
      key: k,
      side,
      col:
        el !== undefined ? spot.col : Math.min(spot.col, lineLength(k, side)),
      link: el !== undefined,
    };
    setCursor(c);
    applyStop(cell, c);
    if (el !== undefined) {
      if (!isGapKey(k)) revealIn(el, el.getBoundingClientRect());
      focusKeepingSelection(el);
    } else {
      if (!cell.hasAttribute("tabindex")) return false;
      focusKeepingSelection(cell);
    }
    if (page) bringOnPage(r);
    return true;
  };
  const pendingFocus = createPendingFocus<{
    key: number;
    side: DiffSessionSide;
    want: number | Spot;
    page: boolean;
  }>(
    () => box,
    (p) => tryFocus(p.key, p.side, p.want, p.page),
  );
  // page: scroll it into view.
  const focusKey = (
    k: number,
    side: DiffSessionSide,
    want: number | Spot,
    page = true,
  ) => {
    pendingFocus.clear();
    const c = rawCursor();
    if (c.key !== k || c.side !== side) {
      const col = typeof want === "number" ? want : want.col;
      setCursor({
        key: k,
        side,
        col: Math.min(col, lineLength(k, side)),
        link: false,
      });
    }
    if (page) scrollToIdx(indexOfKey(vis(), k), "auto");
    pendingFocus.request({ key: k, side, want, page });
  };

  const doReveal = (r: number) => {
    if (whole() || r < 0 || r >= rows.count) return;
    const i = indexOfRow(vis(), space.displayOf(r));
    if (vis().gap[i] === 1) props.setShown(reveal(props.shown, r));
  };

  const apiCursor = () => {
    const c = cursor();
    const d = rowOfKey(c.key);
    const gap = isGapKey(c.key);
    const span = { lo: space.first(d), hi: space.last(d) };
    const first = curIdx() === 0;
    if (gap) return { row: span.lo, side: c.side, ...span, col: 0, gap, first };
    const own = cellRow(c.key, c.side);
    const side = own >= 0 ? c.side : otherSide(c.side);
    const row = own >= 0 ? own : cellRow(c.key, side);
    return { row, side, ...span, col: c.col, gap, first };
  };

  // eslint-disable-next-line solid/reactivity -- once, at setup
  props.ref?.({
    focusRow: (r, col, side) => {
      goal = col;
      const k = keyOfRow(r);
      focusKey(k, sideAt(k, side ?? sideOfRow(rows, r)), col);
    },
    setCursorRow: (r, side, col = 0) => {
      const k = keyOfRow(r);
      const s = sideAt(k, side ?? sideOfRow(rows, r));
      const c = isGapKey(k) ? 0 : Math.min(col, lineLength(k, s));
      goal = c;
      setCursor({ key: k, side: s, col: c, link: false });
    },
    scrollToRow: (r, align = "center") =>
      scrollToIdx(indexOfRow(vis(), space.displayOf(r)), align),
    reveal: doReveal,
    hasFocus: () => box.contains(document.activeElement),
    cursor: apiCursor,
    rowEl: (r) => rowByKey(keyOfRow(r)),
  });

  // A gap button: show part of the gap, then focus the same button on
  // what is left of the gap, else its last one ("Show N"); with no gap
  // left, the first line shown. "↓ 20" moves the gap down: scroll it
  // into view. Gaps hold equal rows only, so display rows and unified
  // rows of a gap go one to one.
  const onExpand = (k: number, how: Expand) => {
    const i = indexOfKey(vis(), k);
    if (i < 0) return;
    const d = rowOfKey(k);
    const r = space.first(d);
    const len = vis().len[i] ?? 0;
    const next = expand(props.shown, r, len, how);
    props.setShown(next);
    const left = visibleOf(space.shown(next));
    const moved = how === "down" ? step : 0;
    const j = indexOfKey(left, gapKey(d + moved));
    const side = cursor().side;
    if (how === "all" || j < 0) {
      const to = d + (how === "up" ? Math.max(0, len - step) : 0);
      focusKey(to, sideAt(to, side), 0, false);
      return;
    }
    const buttons = gapButtons(rows, r + moved, left.len[j] ?? 0);
    const b = buttons.indexOf(how);
    const g = gapKey(d + moved);
    focusKey(
      g,
      sideAt(g, side),
      { link: true, col: (b < 0 ? buttons.length - 1 : b) + 1 },
      how === "down",
    );
  };

  const onKeyDown = (e: KeyboardEvent) => {
    if (
      !(e.target instanceof Element) ||
      e.target.closest(".code-cell") === null
    )
      return;
    const mods = modsOf(e);
    const cur = cursor();
    if (isPlain(mods) && (e.key === "ArrowRight" || e.key === "ArrowLeft")) {
      e.preventDefault();
      const dir = e.key === "ArrowRight" ? 1 : -1;
      const cell = cellOf(cur.key, cur.side);
      const to =
        cell === undefined ? undefined : nextStop(stopsOf(cell), cur, dir);
      if (to !== undefined) {
        // A button's col is no text column.
        if (!isGapKey(cur.key)) goal = to.col;
        focusKey(cur.key, cur.side, to, false);
        return;
      }
      // Split: → past base's last stop goes to head's start, ← before
      // head's first goes to base's end, in view. At the outer ends:
      // scroll.
      const inner = dir === 1 ? "base" : "head";
      if (split && !isGapKey(cur.key) && cur.side === inner) {
        const side = otherSide(inner);
        const col = dir === 1 ? 0 : lineLength(cur.key, "base");
        goal = col;
        focusKey(cur.key, side, { link: false, col }, false);
        const to = cellOf(cur.key, side);
        if (to !== undefined) revealEdge(to, dir === -1);
        return;
      }
      hx?.by(sideStep * dir);
      return;
    }
    const page = pageRows(box.clientHeight, rowHeight.height());
    const to = keyTarget(e.key, mods, curIdx() + 1, count(), page);
    if (to === undefined) return;
    e.preventDefault();
    if (e.key === "Home" || e.key === "End") goal = 0;
    const k = keyAt(vis(), to - 1);
    focusKey(k, sideAt(k, cur.side), goal);
  };

  // The code cell an event is in: the cell, or the one of the half or
  // row (a click on a number).
  const cellAt = (t: Element): HTMLElement | null =>
    t.closest<HTMLElement>(".code-cell") ??
    (
      t.closest(".diff-half") ?? t.closest(".code-row")
    )?.querySelector<HTMLElement>(".code-cell") ??
    null;

  // A click on a line: the cursor goes there (the link clicked, else
  // the caret's column). A drag that selects only moves the cursor.
  const onMouseUp = (e: MouseEvent) => {
    if (e.button !== 0 || !(e.target instanceof Element)) return;
    const r = e.target.closest<HTMLElement>(".code-row");
    const cell = cellAt(e.target);
    if (r === null || cell === null || !box.contains(r)) return;
    const k = Number(r.dataset.key);
    const side = sideAt(k, sideOfCell(cell, cursor().side));
    const sel = getSelection();
    if (sel !== null && !sel.isCollapsed) {
      setCursor({ key: k, side, col: 0, link: false });
      return;
    }
    const stop = e.target.closest<HTMLElement>("a.code-link, button");
    if (stop !== null && cell.contains(stop)) {
      const s = stopsOf(cell).find((x) => x.el === stop);
      if (s !== undefined) {
        focusKey(k, side, { link: true, col: s.start }, false);
        return;
      }
    }
    const col =
      sel?.focusNode != null && cell.contains(sel.focusNode)
        ? colIn(cell, sel.focusNode, sel.focusOffset)
        : 0;
    // A gap row's col is no text column.
    if (!isGapKey(k)) goal = col;
    focusKey(k, side, col, false);
  };
  // Focus that came another way (a screen reader): the cursor follows.
  const onFocusIn = (e: FocusEvent) => {
    const t = e.target;
    if (!(t instanceof HTMLElement)) return;
    const cell = t.closest(".code-cell");
    const r = t.closest<HTMLElement>(".code-row");
    if (cell === null || r === null) return;
    const k = Number(r.dataset.key);
    const cur = cursor();
    const side = sideAt(k, sideOfCell(cell, cur.side));
    const spot = focusSpot(
      t,
      cell,
      stopsOf(cell),
      cur.key === k && cur.side === side && !cur.link ? cur.col : 0,
    );
    if (spot === undefined) return;
    const c: Cursor = { key: k, side, ...spot };
    if (sameFields(c, cur)) return;
    if (!isGapKey(k)) goal = c.col;
    const p = pendingFocus.get();
    if (p?.key !== k || p.side !== side) pendingFocus.clear();
    setCursor(c);
    applyStop(cell, c);
  };
  onMount(() => {
    rowHeight.observe(probe);
    box.addEventListener("focusin", onFocusIn);
    hx = split ? sharedScroll(box, rowsEl) : plainScroll(box);
    // The hit's outline is cut to its cell's view: again on a sideways
    // scroll (split: after sharedScroll moved the cells).
    let x = box.scrollLeft;
    const onScroll = () => {
      if (box.scrollLeft === x) return;
      x = box.scrollLeft;
      draw.outline();
    };
    box.addEventListener("scroll", onScroll, { passive: true });
    onCleanup(() => box.removeEventListener("scroll", onScroll));
  });
  onCleanup(() => {
    const at = apiCursor();
    const cur = cursor();
    props.onLeave?.({
      row: at.row,
      side: cur.side,
      col: at.col,
      link: cur.link,
      offset: curIdx() * rowHeight.height() - box.scrollTop,
      focus:
        box.contains(document.activeElement) ||
        pendingFocus.get() !== undefined,
    });
  });

  // A new row height: the same item at the top.
  keepTopOnNewHeight({
    height: rowHeight.height,
    fallback: fallbackRowHeight,
    box: () => box,
    rows: () => rowsEl,
    v,
    after: () => repaintSoon(),
  });
  createEffect(() => {
    const n = Math.max(rows.baseCount, rows.headCount);
    setVar(box, "--code-digits", String(String(n).length));
    setVar(
      box,
      "--code-cols",
      String(Math.max(props.base.index.maxCols, props.head.index.maxCols)),
    );
  });
  createEffect(() => setVar(rowsEl, "--code-h", `${v.getTotalSize()}px`));
  // Whole file on or off: the cursor's row stays where it is on screen.
  let lastVis = untrack(vis);
  let lastWhole = untrack(whole);
  createEffect(
    on(
      vis,
      (vs) => {
        const before = lastVis;
        const wasWhole = lastWhole;
        lastVis = vs;
        lastWhole = whole();
        if (wasWhole === lastWhole) return;
        const r = rowOfKey(rawCursor().key);
        const moved = indexOfRow(vs, r) - indexOfRow(before, r);
        const h = rowHeight.height();
        setVar(rowsEl, "--code-h", `${vs.length * h}px`);
        box.scrollTop += moved * h;
      },
      { defer: true },
    ),
  );

  const isCursor = createSelector(() => cursor().key);
  // A cell's id for the selector: its key and side.
  const cellId = (k: number, side: DiffSessionSide) =>
    k * 2 + (side === "head" ? 1 : 0);
  const isCursorCell = createSelector(() =>
    cellId(cursor().key, cursor().side),
  );
  const onLink = createMemo(() => cursor().link);
  // Find: hits made for these rows and this layout only.
  const hits = createMemo(() => {
    const f = props.find;
    return f !== undefined && isFor(f.finder, cells) ? f : undefined;
  });
  // The current hit's cell line; 0: none.
  const isFindLine = createSelector(() => {
    const f = hits();
    return f === undefined ? 0 : (f.matches.line[f.current] ?? 0);
  });

  // A code cell: drawn by hand, its stop kept on a redraw. r: the
  // unified row shown; -1: an empty split cell (named, no text).
  function codeCell(
    k: number,
    side: DiffSessionSide,
    r: number,
    aria: { describedby?: string; label?: string } = {},
  ): Node {
    let cell!: HTMLDivElement;
    const el = (
      <div
        ref={(e) => (cell = e)}
        class="code-cell"
        role="gridcell"
        data-side={side}
        aria-describedby={aria.describedby}
        aria-label={aria.label}
        tabindex={
          isCursorCell(cellId(k, side)) ? (onLink() ? -1 : 0) : undefined
        }
      />
    ) as Node;
    if (r < 0) return el;
    const n = lineOn(rows, r, side);
    const text = textAt(k, side);
    drawCell(
      () => cell,
      // eslint-disable-next-line solid/reactivity -- drawCell tracks it
      () => {
        const render = side === "base" ? props.renderBase : props.renderHead;
        return render === undefined
          ? lineNodes(plainPieces(text))
          : render(n, text);
      },
      // eslint-disable-next-line solid/reactivity -- run untracked, after a draw
      (had) => {
        const c = cursor();
        if (c.key !== k || c.side !== side) return;
        // New <a>: the stop again. Focus was on a link that went: back
        // to the cursor, unless a focus waits for another cell.
        applyStop(cell, c);
        const p = pendingFocus.get();
        if (
          had &&
          !cell.contains(document.activeElement) &&
          (p === undefined || (p.key === k && p.side === side))
        ) {
          focusKey(k, side, p?.want ?? { link: c.link, col: c.col }, false);
        }
      },
    );
    return el;
  }

  // --code-y from the item's place in the visible list.
  const placeAt = (k: number, el: () => HTMLElement) => {
    const idx = createMemo(() => indexOfKey(vis(), k));
    createEffect(() =>
      setVar(el(), "--code-y", `${Math.max(0, idx()) * rowHeight.height()}px`),
    );
    return idx;
  };

  function unifiedRow(r: number): Node {
    let el!: HTMLDivElement;
    const idx = placeAt(r, () => el);
    const kind = rows.kind[r];
    const b = rows.base[r] ?? 0;
    const h = rows.head[r] ?? 0;
    return (
      <div
        ref={(e) => (el = e)}
        // diff-row: a test hook, no style.
        class="code-row diff-row"
        classList={{
          "diff-delete": kind === deleteRow,
          "diff-insert": kind === insertRow,
          "code-cursor-line": isCursor(r),
          "code-line-target": props.marked?.row === r,
          "code-find-line": isFindLine(cellLine(cells, r, sideOfRow(rows, r))),
        }}
        role="row"
        aria-rowindex={idx() + 1}
        data-key={r}
      >
        <div
          class="code-num"
          role="rowheader"
          aria-label={rowLabel(rows, r)}
          data-b={b === 0 ? "" : b}
          data-h={h === 0 ? "" : h}
        />
        <div class="diff-sign" role="gridcell" />
        {codeCell(r, sideOfRow(rows, r), r)}
      </div>
    ) as Node;
  }

  // Four cells: base number, base code, head number, head code. The
  // number names the line; the code cell points to it (describedby).
  function splitRow(d: number): Node {
    let el!: HTMLDivElement;
    const idx = placeAt(d, () => el);
    const half = (side: DiffSessionSide) => {
      const r = space.rowOf(d, side);
      const kind = r < 0 ? undefined : rows.kind[r];
      const n = r < 0 ? 0 : lineOn(rows, r, side);
      const id = `diff-${side === "base" ? "b" : "h"}-${d}`;
      return (
        <div
          class={`diff-half diff-${side}`}
          classList={{
            "diff-delete": kind === deleteRow,
            "diff-insert": kind === insertRow,
            "diff-empty": r < 0,
            "diff-cursor-half": isCursorCell(cellId(d, side)),
            "code-line-target":
              r >= 0 && props.marked?.row === r && props.marked.side === side,
            "code-find-line": r >= 0 && isFindLine(cellLine(cells, r, side)),
          }}
        >
          <div
            class="code-num"
            role="gridcell"
            id={id}
            aria-label={r < 0 ? undefined : splitCellLabel(rows, r, side)}
            data-n={n === 0 ? "" : n}
          />
          <div class="diff-clip">
            {codeCell(
              d,
              side,
              r,
              r < 0 ? { label: noLine[side] } : { describedby: id },
            )}
          </div>
        </div>
      );
    };
    return (
      <div
        ref={(e) => (el = e)}
        class="code-row diff-row"
        classList={{ "code-cursor-line": isCursor(d) }}
        role="row"
        aria-rowindex={idx() + 1}
        data-key={d}
      >
        <div class="diff-split">
          {half("base")}
          {half("head")}
        </div>
      </div>
    ) as Node;
  }

  function gapRow(k: number): Node {
    let el!: HTMLDivElement;
    // Its first unified row (gaps hold equal rows: one to one).
    const r = space.first(rowOfKey(k));
    const idx = placeAt(k, () => el);
    const len = createMemo(() => {
      const i = idx();
      return i < 0 ? 0 : (vis().len[i] ?? 0);
    });
    const buttons = createMemo(() =>
      len() === 0 ? [] : gapButtons(rows, r, len()),
    );
    const row = (
      <div
        ref={(e) => (el = e)}
        class="code-row diff-gap"
        classList={{ "code-cursor-line": isCursor(k) }}
        role="row"
        aria-rowindex={idx() + 1}
        data-key={k}
      >
        <div
          class="code-cell diff-gap-cell"
          role="gridcell"
          aria-colspan={colCount}
          tabindex={isCursor(k) ? (onLink() ? -1 : 0) : undefined}
        >
          <Index each={buttons()}>
            {(how) => {
              const t = () => buttonText(rows, r, len(), how());
              return (
                <button
                  type="button"
                  class={`diff-gap-button diff-gap-${how()}`}
                  tabindex="-1"
                  aria-label={t().name}
                  onClick={() => onExpand(k, how())}
                >
                  {t().text}
                </button>
              );
            }}
          </Index>
          <span class="diff-gap-text">
            {len() === 0 ? "" : gapLabel(rows, r, len())}
          </span>
        </div>
      </div>
    ) as Node;
    // New buttons: the stop again.
    createEffect(
      on(buttons, () => {
        const c = untrack(cursor);
        if (c.key === k)
          applyStop(el.querySelector(".code-cell") ?? undefined, c);
      }),
    );
    return row;
  }

  const renderRow = (k: number) =>
    isGapKey(k) ? gapRow(k) : split ? splitRow(k) : unifiedRow(k);
  // eslint-disable-next-line solid/reactivity -- mountRows tracks keys
  mountRows(() => rowsEl, keys, renderRow, owner);

  createEffect(
    on(
      cursor,
      (c) => {
        applyStop(cellOf(c.key, c.side), c);
        pendingFocus.retry();
      },
      { defer: true },
    ),
  );
  // Find: the hits of the mounted cells. Again when rows come or
  // redraw, or the hits change.
  const draw = createFindDraw({
    box: () => box,
    cells: function* () {
      for (const k of keys()) {
        if (isGapKey(k)) continue;
        for (const side of split ? diffSides : [sideOfKey(k)]) {
          const r = cellRow(k, side);
          if (r < 0) continue;
          const cell = cellOf(k, side);
          const line = cellLine(cells, r, side);
          if (cell !== undefined && line > 0) yield { cell, line };
        }
      }
    },
    find: hits,
    reveal: revealIn,
    // Not over the sticky gutter, nor past the cell's view.
    clip: (cell) => hx?.bounds(cell) ?? noClip,
  });
  // Rows are placed (--code-y) by their own effects, which may run
  // after ours: draw once they have, in a microtask.
  let queued = false;
  let alive = true;
  onCleanup(() => (alive = false));
  const repaintSoon = () => {
    if (queued) return;
    queued = true;
    queueMicrotask(() => {
      queued = false;
      if (alive) draw.repaint();
    });
  };
  createEffect(
    on(
      keys,
      () => {
        pendingFocus.retry();
        repaintSoon();
      },
      { defer: true },
    ),
  );
  // After a side redrew (render effects run first).
  createEffect(
    on([() => props.renderBase, () => props.renderHead], repaintSoon, {
      defer: true,
    }),
  );
  // A new hit: draw it, and bring it into view sideways once drawn.
  createEffect(
    on(hits, (f) => {
      if (f !== undefined && f.current >= 0) draw.revealNext();
      repaintSoon();
    }),
  );
  // After a layout switch: the cursor's row where it was on screen, and
  // focus back if the grid had it. After mountRows: the row is in.
  onMount(() => {
    if (carried === undefined) return;
    const h = rowHeight.height();
    setVar(rowsEl, "--code-h", `${count() * h}px`);
    box.scrollTop = Math.max(0, curIdx() * h - carried.offset);
    const c = rawCursor();
    if (carried.focus) focusKey(c.key, c.side, c, false);
  });

  return (
    <div class="code-wrap">
      <div
        ref={(el) => (box = el)}
        class="code-view diff-view"
        classList={{ "diff-split-view": split }}
        role="grid"
        aria-readonly="true"
        tabindex="-1"
        aria-rowcount={count()}
        aria-colcount={colCount}
        aria-label={`Diff of ${props.path}`}
        onKeyDown={onKeyDown}
        onMouseUp={onMouseUp}
      >
        <div ref={(el) => (rowsEl = el)} class="code-rows" role="rowgroup" />
      </div>
      <div
        ref={(el) => (probe = el)}
        class="code-row code-probe"
        aria-hidden="true"
      />
    </div>
  );
}
