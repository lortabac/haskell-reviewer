// The unified diff as an ARIA grid, virtualised like the source view
// (D-70, D-341): fixed row height, only rows near the screen mounted
// (plus the cursor's), rows never moved (mountRows, keyed by row).
// Removed rows show base's text, the rest head's. Gap rows stand for
// hidden equal lines, with buttons to show them.
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
  onMount,
  untrack,
} from "solid-js";
import { setVar } from "../ui/cssom";
import { bringOnPage } from "../ui/scroll";
import { plainPieces } from "../source/display";
import { focusKeepingSelection } from "../source/focus";
import {
  createPendingFocus,
  createRovingStop,
  drawCell,
  focusSpot,
  keepTopOnNewHeight,
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
import {
  allShown,
  buttonText,
  defaultShown,
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

// One side's text and its lines.
export interface SideText {
  text: string;
  index: LineIndex;
}

// key: a row key (rows.ts keyAt). link: on a stop rather than the
// cell. In a line row a stop is a link and col its UTF-16 column (from
// 0). In a gap row a stop is a button, col i + 1 for the i-th; on the
// cell col is 0. This way keys.ts nextStop and landOn work on both.
interface Cursor {
  key: number;
  col: number;
  link: boolean;
}

export interface DiffApi {
  // Focus a row at a column (the link there, else the cell), and bring
  // it into view. A hidden row: its gap.
  focusRow: (row: number, col: number) => void;
  // Move the cursor, no focus change and no scroll.
  setCursorRow: (row: number) => void;
  scrollToRow: (row: number, align?: "auto" | "center" | "start") => void;
  // Show a hidden row and a few around it.
  reveal: (row: number) => void;
  hasFocus: () => boolean;
  // The mounted element of a row, if any.
  rowEl: (row: number) => HTMLElement | undefined;
}

export interface DiffGridProps {
  path: string;
  rows: Rows;
  base: SideText;
  head: SideText;
  // The nodes of a line on that side (paint.ts), as CodeView's
  // renderLine; undefined: plain. A new function redraws that side's
  // rows.
  renderBase?: ((n: number, text: string) => Node[]) | undefined;
  renderHead?: ((n: number, text: string) => Node[]) | undefined;
  // Show every row.
  whole: boolean;
  // The row of the hash, if any.
  marked?: number | undefined;
  ref?: (api: DiffApi) => void;
}

export function DiffGrid(props: DiffGridProps): JSX.Element {
  let box!: HTMLDivElement;
  let rowsEl!: HTMLDivElement;
  let probe!: HTMLDivElement;
  const owner = getOwner();
  // Read once: a new diff needs a new grid. DiffView mounts it only
  // with rows, so the visible list is never empty.
  const rows = untrack(() => props.rows);
  const rowHeight = createRowHeight(fallbackRowHeight);
  const [shown, setShown] = createSignal(defaultShown(rows));
  const whole = createMemo(() => props.whole);
  const vis = createMemo(() => visibleOf(whole() ? allShown(rows) : shown()));
  const count = () => vis().length;

  const [rawCursor, setRawCursor] = createSignal<Cursor>({
    key: keyAt(vis(), 0),
    col: 0,
    link: false,
  });
  // The cursor's item: its row's, or the gap the row is hidden in.
  const curIdx = createMemo(() =>
    Math.max(0, indexOfRow(vis(), rowOfKey(rawCursor().key))),
  );
  const cursor = createMemo((): Cursor => {
    const c = rawCursor();
    const k = keyAt(vis(), curIdx());
    return k === c.key ? c : { key: k, col: 0, link: false };
  });
  const setCursor = (c: Cursor) => {
    if (!sameFields(rawCursor(), c)) setRawCursor(c);
  };
  // Where ↑ and ↓ aim (UTF-16), kept over short lines.
  let goal = 0;

  const sideOf = (r: number) =>
    rows.kind[r] === deleteRow
      ? { side: props.base, n: rows.base[r] ?? 0 }
      : { side: props.head, n: rows.head[r] ?? 0 };
  const textOf = (r: number) => {
    const { side, n } = sideOf(r);
    return lineText(side.text, side.index, n);
  };
  const lineLength = (key: number) => (isGapKey(key) ? 0 : textOf(key).length);

  const follow = () => {
    const r = v.range;
    if (!v.isScrolling || r === null || box.contains(document.activeElement))
      return;
    const i = curIdx();
    if (i < r.startIndex || i > r.endIndex) {
      setCursor({ key: keyAt(vis(), r.startIndex), col: 0, link: false });
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
  const cellOf = (k: number) =>
    rowByKey(k)?.querySelector<HTMLElement>(".code-cell") ?? undefined;
  const scrollToIdx = (i: number, align: "auto" | "center" | "start") => {
    v.scrollToIndex(Math.min(Math.max(0, i), count() - 1), { align });
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
  const tryFocus = (k: number, want: number | Spot, page: boolean) => {
    const r = rowByKey(k);
    const cell = r?.querySelector<HTMLElement>(".code-cell");
    if (r === undefined || cell == null) return false;
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
      col: el !== undefined ? spot.col : Math.min(spot.col, lineLength(k)),
      link: el !== undefined,
    };
    setCursor(c);
    applyStop(cell, c);
    if (el !== undefined) {
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
    want: number | Spot;
    page: boolean;
  }>(
    () => box,
    (p) => tryFocus(p.key, p.want, p.page),
  );
  // page: scroll it into view.
  const focusKey = (k: number, want: number | Spot, page = true) => {
    pendingFocus.clear();
    if (rawCursor().key !== k) {
      const col = typeof want === "number" ? want : want.col;
      setCursor({ key: k, col: Math.min(col, lineLength(k)), link: false });
    }
    if (page) scrollToIdx(indexOfKey(vis(), k), "auto");
    pendingFocus.request({ key: k, want, page });
  };
  // The key of the item a row is in now.
  const keyOfRow = (r: number) =>
    keyAt(vis(), Math.max(0, indexOfRow(vis(), r)));

  const doReveal = (r: number) => {
    if (whole() || r < 0 || r >= rows.count) return;
    const i = indexOfRow(vis(), r);
    if (vis().gap[i] === 1) setShown(reveal(shown(), r));
  };

  // eslint-disable-next-line solid/reactivity -- once, at setup
  props.ref?.({
    focusRow: (r, col) => {
      goal = col;
      focusKey(keyOfRow(r), col);
    },
    setCursorRow: (r) => {
      goal = 0;
      setCursor({ key: keyOfRow(r), col: 0, link: false });
    },
    scrollToRow: (r, align = "center") =>
      scrollToIdx(indexOfRow(vis(), r), align),
    reveal: doReveal,
    hasFocus: () => box.contains(document.activeElement),
    rowEl: (r) => rowByKey(keyOfRow(r)),
  });

  // A gap button: show part of the gap, then focus the same button on
  // what is left of the gap, else its last one ("Show N"); with no gap
  // left, the first line shown. "↓ 20" moves the gap down: scroll it
  // into view.
  const onExpand = (k: number, how: Expand) => {
    const i = indexOfKey(vis(), k);
    if (i < 0) return;
    const start = rowOfKey(k);
    const len = vis().len[i] ?? 0;
    const next = expand(shown(), start, len, how);
    setShown(next);
    const left = visibleOf(next);
    const gapStart = how === "down" ? start + step : start;
    const j = indexOfKey(left, gapKey(gapStart));
    if (how === "all" || j < 0) {
      focusKey(start + (how === "up" ? Math.max(0, len - step) : 0), 0, false);
      return;
    }
    const buttons = gapButtons(rows, gapStart, left.len[j] ?? 0);
    const b = buttons.indexOf(how);
    focusKey(
      gapKey(gapStart),
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
      const cell = cellOf(cur.key);
      const to =
        cell === undefined ? undefined : nextStop(stopsOf(cell), cur, dir);
      if (to === undefined) {
        box.scrollLeft += sideStep * dir;
        return;
      }
      // A button's col is no text column.
      if (!isGapKey(cur.key)) goal = to.col;
      focusKey(cur.key, to, false);
      return;
    }
    const page = pageRows(box.clientHeight, rowHeight.height());
    const to = keyTarget(e.key, mods, curIdx() + 1, count(), page);
    if (to === undefined) return;
    e.preventDefault();
    if (e.key === "Home" || e.key === "End") goal = 0;
    focusKey(keyAt(vis(), to - 1), goal);
  };

  // A click on a line: the cursor goes there (the link clicked, else
  // the caret's column). A drag that selects only moves the cursor.
  const onMouseUp = (e: MouseEvent) => {
    if (e.button !== 0 || !(e.target instanceof Element)) return;
    const r = e.target.closest<HTMLElement>(".code-row");
    const cell = r?.querySelector<HTMLElement>(".code-cell");
    if (r == null || cell == null || !box.contains(r)) return;
    const k = Number(r.dataset.key);
    const sel = getSelection();
    if (sel !== null && !sel.isCollapsed) {
      setCursor({ key: k, col: 0, link: false });
      return;
    }
    const stop = e.target.closest<HTMLElement>("a.code-link, button");
    if (stop !== null && cell.contains(stop)) {
      const s = stopsOf(cell).find((x) => x.el === stop);
      if (s !== undefined) {
        focusKey(k, { link: true, col: s.start }, false);
        return;
      }
    }
    const col =
      sel?.focusNode != null && cell.contains(sel.focusNode)
        ? colIn(cell, sel.focusNode, sel.focusOffset)
        : 0;
    // A gap row's col is no text column.
    if (!isGapKey(k)) goal = col;
    focusKey(k, col, false);
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
    const spot = focusSpot(
      t,
      cell,
      stopsOf(cell),
      cur.key === k && !cur.link ? cur.col : 0,
    );
    if (spot === undefined) return;
    const c: Cursor = { key: k, ...spot };
    if (sameFields(c, cur)) return;
    if (!isGapKey(k)) goal = c.col;
    if (pendingFocus.get()?.key !== k) pendingFocus.clear();
    setCursor(c);
    applyStop(cell, c);
  };
  onMount(() => {
    rowHeight.observe(probe);
    box.addEventListener("focusin", onFocusIn);
  });

  // A new row height: the same item at the top.
  keepTopOnNewHeight({
    height: rowHeight.height,
    fallback: fallbackRowHeight,
    box: () => box,
    rows: () => rowsEl,
    v,
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
  const onLink = createMemo(() => cursor().link);

  function lineRow(r: number): Node {
    let el!: HTMLDivElement;
    let cell!: HTMLDivElement;
    const idx = createMemo(() => indexOfKey(vis(), r));
    createEffect(() =>
      setVar(el, "--code-y", `${Math.max(0, idx()) * rowHeight.height()}px`),
    );
    const kind = rows.kind[r];
    const b = rows.base[r] ?? 0;
    const h = rows.head[r] ?? 0;
    const text = textOf(r);
    const row = (
      <div
        ref={(e) => (el = e)}
        // diff-row: a test hook, no style.
        class="code-row diff-row"
        classList={{
          "diff-delete": kind === deleteRow,
          "diff-insert": kind === insertRow,
          "code-cursor-line": isCursor(r),
          "code-line-target": props.marked === r,
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
        <div
          ref={(e) => (cell = e)}
          class="code-cell"
          role="gridcell"
          tabindex={isCursor(r) ? (onLink() ? -1 : 0) : undefined}
        />
      </div>
    ) as Node;
    const { n } = sideOf(r);
    drawCell(
      () => cell,
      // eslint-disable-next-line solid/reactivity -- drawCell tracks it
      () => {
        const render = kind === deleteRow ? props.renderBase : props.renderHead;
        return render === undefined
          ? lineNodes(plainPieces(text))
          : render(n, text);
      },
      // eslint-disable-next-line solid/reactivity -- run untracked, after a draw
      (had) => {
        const c = cursor();
        if (c.key !== r) return;
        // New <a>: the stop again. Focus was on a link that went: back
        // to the cursor, unless a focus waits for another row.
        applyStop(cell, c);
        const p = pendingFocus.get();
        if (
          had &&
          !cell.contains(document.activeElement) &&
          (p === undefined || p.key === r)
        ) {
          focusKey(r, p?.want ?? { link: c.link, col: c.col }, false);
        }
      },
    );
    return row;
  }

  function gapRow(k: number): Node {
    let el!: HTMLDivElement;
    const start = rowOfKey(k);
    const idx = createMemo(() => indexOfKey(vis(), k));
    const len = createMemo(() => {
      const i = idx();
      return i < 0 ? 0 : (vis().len[i] ?? 0);
    });
    createEffect(() =>
      setVar(el, "--code-y", `${Math.max(0, idx()) * rowHeight.height()}px`),
    );
    const buttons = createMemo(() =>
      len() === 0 ? [] : gapButtons(rows, start, len()),
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
          aria-colspan={3}
          tabindex={isCursor(k) ? (onLink() ? -1 : 0) : undefined}
        >
          <Index each={buttons()}>
            {(how) => {
              const t = () => buttonText(rows, start, len(), how());
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
            {len() === 0 ? "" : gapLabel(rows, start, len())}
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

  const renderRow = (k: number) => (isGapKey(k) ? gapRow(k) : lineRow(k));
  // eslint-disable-next-line solid/reactivity -- mountRows tracks keys
  mountRows(() => rowsEl, keys, renderRow, owner);

  createEffect(
    on(
      cursor,
      (c) => {
        applyStop(cellOf(c.key), c);
        pendingFocus.retry();
      },
      { defer: true },
    ),
  );
  createEffect(on(keys, () => pendingFocus.retry(), { defer: true }));

  return (
    <div class="code-wrap">
      <div
        ref={(el) => (box = el)}
        class="code-view diff-view"
        role="grid"
        aria-readonly="true"
        tabindex="-1"
        aria-rowcount={count()}
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
