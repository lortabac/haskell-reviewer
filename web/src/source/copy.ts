// Copy for one view of a file (SPEC §5.7): the document copy handler,
// a hook on each selection change (to pin its ends), and Ctrl+A for
// the whole text. The source view and the diff grid use it.
import { onCleanup, onMount } from "solid-js";

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
  // Ctrl+A in the view: select the mounted rows; copy gives all()
  // while the selection stays the one made here.
  selectAll: () => void;
  // Call after rows were mounted or removed: keeps the mounted rows
  // selected while select-all is on.
  afterMount: () => void;
  // Select-all is off now (a click that will change the selection).
  endSelectAll: () => void;
  // The selection is the one made by selectAll.
  isAllSelected: () => boolean;
}

export function createCopy(opts: {
  view: () => HTMLElement;
  rows: () => HTMLElement;
  // What a copy of sel gives; undefined: leave it to the browser.
  copied: (sel: Selection) => string | undefined;
  // What a copy gives after selectAll.
  all: () => string;
  // The selection changed (its ends' rows to keep mounted).
  onSelection: (sel: Selection | null) => void;
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
    opts.onSelection(sel);
    if (allSelected !== undefined) {
      if (sel === null || !sameShape(allSelected, shapeOf(sel))) {
        allSelected = undefined;
      }
    }
  };

  const onCopy = (e: ClipboardEvent) => {
    if (e.defaultPrevented || e.clipboardData === null) return;
    // A field of its own (search box, a later text area): not ours.
    const t = e.target;
    if (
      t instanceof HTMLElement &&
      (t instanceof HTMLInputElement ||
        t instanceof HTMLTextAreaElement ||
        t.isContentEditable) &&
      !opts.view().contains(t)
    ) {
      return;
    }
    const sel = getSelection();
    let text: string | undefined;
    if (allSelected !== undefined) {
      text = opts.all();
    } else if (sel !== null) {
      text = opts.copied(sel);
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
    isAllSelected: () => allSelected !== undefined,
  };
}
