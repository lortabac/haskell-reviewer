// A declaration's source, inline: a button that shows or hides it.
// Line numbers, "⋯" rows for lines left out (D-195). Highlighted and
// linked when the page's source has come (pageSource.ts), plain before.
// Built only when open.
import {
  createEffect,
  createMemo,
  createSignal,
  createUniqueId,
  For,
  type JSX,
  on,
  onCleanup,
  onMount,
  Show,
} from "solid-js";
import type { SourcePart } from "../api/module";
import { usePage } from "./context";
import { styledPieces } from "../source/display";
import { createHoverCard } from "../source/createHoverCard";
import { focusKeepingSelection } from "../source/focus";
import { lineNodes } from "../source/paint";
import { colIn } from "../source/selection";
import { type SourceRow, rowSegs, sourceRows, textParts } from "./source";

export function SourceToggle(props: {
  parts: SourcePart[];
  // For screen readers: whose source.
  label: string;
  // Rest of the actions row (go to source).
  children?: JSX.Element;
}): JSX.Element {
  const page = usePage();
  const [open, setOpen] = createSignal(false);
  const id = createUniqueId();
  return (
    <>
      <p class="decl-actions">
        <button
          type="button"
          class="decl-source-toggle"
          aria-expanded={open()}
          aria-controls={id}
          onClick={() => {
            if (!open()) page.wantSource();
            setOpen(!open());
          }}
        >
          {open() ? "Hide source" : "Show source"}
          <span class="sr-only"> of {props.label}</span>
        </button>
        {props.children}
      </p>
      <div id={id} class="decl-source">
        <Show when={open()}>
          <SourceRows parts={props.parts} />
        </Show>
      </div>
    </>
  );
}

function SourceRows(props: { parts: SourcePart[] }): JSX.Element {
  const page = usePage();
  const rows = createMemo(() =>
    sourceRows(textParts(props.parts, (p) => page.ix.sentPart(p))),
  );
  const link = (k: number) => page.highlight()?.links[k - 1];
  const cardId = createUniqueId();
  let pre!: HTMLPreElement;
  let card!: HTMLDivElement;
  const hover = createHoverCard({
    view: () => pre,
    card: () => card,
    row: (n) =>
      pre.querySelector<HTMLElement>(`.src-row[data-line="${n}"]`) ?? undefined,
    text: (k) => page.highlight()?.cards[k - 1],
    dragging: () => false,
    parts: { row: ".src-row", cell: ".src-code", gutter: ".src-num" },
  });
  // The focused link's spot (line, column): the rows are drawn again
  // when the highlighting comes or changes (a refetch while indexing),
  // and the link is a new element then. Kept while focus stays; a
  // focus out of a link that is still there (checked in a microtask)
  // ends it.
  let spot: { line: number; col: number } | undefined;
  const spotOf = (el: Element) => {
    const r = el.closest(".src-row");
    const cell = el.closest(".src-code");
    return r === null || cell === null
      ? undefined
      : { line: Number(r.getAttribute("data-line")), col: colIn(cell, el, 0) };
  };
  onMount(() => {
    const onIn = (e: FocusEvent) => {
      spot =
        e.target instanceof Element && e.target.matches("a.code-link")
          ? spotOf(e.target)
          : undefined;
    };
    const onOut = (e: FocusEvent) => {
      const t = e.target;
      if (!(t instanceof Node)) return;
      // Chromium sends this when a redraw removes the link: look once
      // the redraw is done. Still in the page: focus really left.
      queueMicrotask(() => {
        if (t.isConnected && !pre.contains(document.activeElement)) {
          spot = undefined;
        }
      });
    };
    pre.addEventListener("focusin", onIn);
    pre.addEventListener("focusout", onOut);
    onCleanup(() => {
      pre.removeEventListener("focusin", onIn);
      pre.removeEventListener("focusout", onOut);
    });
  });
  // After the redraw: the card follows to the new link, and focus goes
  // back to the link at the same spot, or to the block if it is gone.
  createEffect(
    on(
      page.highlight,
      () => {
        hover.relink();
        const s = spot;
        spot = undefined;
        const a = document.activeElement;
        if (s === undefined || (a !== null && a !== document.body)) return;
        const cell = pre.querySelector(
          `.src-row[data-line="${s.line}"] .src-code`,
        );
        const link =
          cell === null
            ? undefined
            : [...cell.querySelectorAll<HTMLElement>("a.code-link")].find(
                (x) => colIn(cell, x, 0) === s.col,
              );
        if (link !== undefined) hover.beforeFocus(link);
        focusKeepingSelection(link ?? pre);
      },
      { defer: true },
    ),
  );
  function row(r: SourceRow): JSX.Element {
    if (r.kind === "gap") {
      return (
        <span class="src-row src-gap">
          {r.from === r.to
            ? `⋯ line ${r.from}\n`
            : `⋯ lines ${r.from}–${r.to}\n`}
        </span>
      );
    }
    const nodes = createMemo(() => {
      const h = page.highlight();
      const segs = h === undefined ? [] : rowSegs(r, h.runs);
      return lineNodes(styledPieces(r.text, segs), link, { tabbable: true });
    });
    return (
      <span class="src-row" data-line={r.line}>
        <span class="src-num">{r.line}</span>
        <span class="src-code">{nodes()}</span>
        {"\n"}
      </span>
    );
  }
  return (
    <div class="src-wrap">
      <pre class="src" tabindex="0" ref={(el) => (pre = el)}>
        <For each={rows()}>{row}</For>
      </pre>
      <div
        id={cardId}
        class="code-card"
        role="tooltip"
        hidden
        ref={(el) => (card = el)}
      />
    </div>
  );
}
