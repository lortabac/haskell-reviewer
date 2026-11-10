// Copy for one source view (SPEC §5.7): the document copy handler, the
// selection's lines (to pin), and Ctrl+A for the whole file.
import { onCleanup, onMount } from "solid-js";
import type { LineIndex } from "./lines";
import { copiedText, selectionLines } from "./selection";

// Where the selection's ends are, to tell if it changed.
interface Shape {
  anchor: Node | null;
  anchorOffset: number;
  focus: Node | null;
  focusOffset: number;
}

function shapeOf(sel: Selection): Shape {
  return {
    anchor: sel.anchorNode,
    anchorOffset: sel.anchorOffset,
    focus: sel.focusNode,
    focusOffset: sel.focusOffset,
  };
}

function sameShape(a: Shape, b: Shape): boolean {
  return sameAnchor(a, b) && a.focusOffset === b.focusOffset;
}

// The same, but the focus offset may differ: rows going away move it.
function sameAnchor(a: Shape, b: Shape): boolean {
  return (
    a.anchor === b.anchor &&
    a.anchorOffset === b.anchorOffset &&
    a.focus === b.focus
  );
}

export interface Copy {
  // Ctrl+A in the view: select the mounted rows; copy gives the whole
  // file while the selection stays the one made here.
  selectAll: () => void;
  // Call after rows were mounted or removed: keeps the mounted rows
  // selected while select-all is on.
  afterMount: () => void;
  // Select-all is off now (a click that will change the selection).
  endSelectAll: () => void;
}

export function createCopy(opts: {
  view: () => HTMLElement;
  rows: () => HTMLElement;
  text: string;
  index: LineIndex;
  // The selection's ends in the view changed (lines to keep mounted).
  onSelectionLines: (lines: number[]) => void;
}): Copy {
  // The selection made by selectAll, while it's still the one there.
  let allSelected: Shape | undefined;

  const selectAll = () => {
    const sel = getSelection();
    if (sel === null) return;
    sel.selectAllChildren(opts.rows());
    allSelected = shapeOf(sel);
  };

  const onSelectionChange = () => {
    const sel = getSelection();
    opts.onSelectionLines(selectionLines(opts.view(), sel));
    if (allSelected !== undefined) {
      if (sel === null || !sameShape(allSelected, shapeOf(sel))) {
        allSelected = undefined;
      }
    }
  };

  const onCopy = (e: ClipboardEvent) => {
    if (e.defaultPrevented || e.clipboardData === null) return;
    const view = opts.view();
    // A field of its own (search box, a later text area): not ours.
    const t = e.target;
    if (
      t instanceof HTMLElement &&
      (t instanceof HTMLInputElement ||
        t instanceof HTMLTextAreaElement ||
        t.isContentEditable) &&
      !view.contains(t)
    ) {
      return;
    }
    const sel = getSelection();
    let text: string | undefined;
    if (allSelected !== undefined) {
      text = opts.text.slice(opts.index.bom ? 1 : 0);
    } else if (sel !== null) {
      text = copiedText(view, opts.text, opts.index, sel);
    }
    if (text === undefined) return;
    e.clipboardData.setData("text/plain", text);
    e.preventDefault();
  };

  onMount(() => {
    document.addEventListener("copy", onCopy);
    document.addEventListener("selectionchange", onSelectionChange);
    onCleanup(() => {
      document.removeEventListener("copy", onCopy);
      document.removeEventListener("selectionchange", onSelectionChange);
    });
  });

  return {
    selectAll,
    afterMount: () => {
      if (allSelected === undefined) return;
      // Anything more than rows going away (a click, whose
      // selectionchange may not have come yet) ends select-all.
      const sel = getSelection();
      if (sel !== null && sameAnchor(allSelected, shapeOf(sel))) {
        selectAll();
      } else {
        allSelected = undefined;
      }
    },
    endSelectAll: () => {
      allSelected = undefined;
    },
  };
}
