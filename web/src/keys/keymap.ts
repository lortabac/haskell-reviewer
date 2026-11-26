// The single-key shortcuts (SPEC §5.3, D-297, D-298): one table for
// the dispatcher and the ? overlay. Bindings are tinykeys strings,
// matched by event.key. Shift is allowed only where the key itself
// may need it on some layouts (? / 1 2 3).

export type ActionId =
  | "search"
  | "next"
  | "prev"
  | "nextChange"
  | "prevChange"
  | "nextFile"
  | "prevFile"
  | "layout"
  | "open"
  | "definition"
  | "references"
  | "source"
  | "find"
  | "copy"
  | "docsView"
  | "sourceView"
  | "diffView"
  | "otherSide"
  | "reload"
  | "escape"
  | "help";

export interface KeyRow {
  id: ActionId;
  // tinykeys bindings. Empty: no binding, the row is for the overlay
  // only (Enter is native).
  bindings: string[];
  // As shown in the overlay, one <kbd> each.
  keys: string[];
  label: string;
  where: string;
  // The UI control that does the same (D-45).
  control: string;
}

export const keymap: readonly KeyRow[] = [
  {
    id: "search",
    bindings: ["[Shift]+/"],
    keys: ["/"],
    label: 'Focus the search box ("text" in quotes searches files)',
    where: "Everywhere",
    control: "Search box in the header",
  },
  {
    id: "next",
    bindings: ["j"],
    keys: ["j"],
    label: "Next declaration or hunk",
    where: "Docs, source, diff",
    control: "Tab (docs), Outline list (source), Next change button (diff)",
  },
  {
    id: "prev",
    bindings: ["k"],
    keys: ["k"],
    label: "Previous declaration or hunk",
    where: "Docs, source, diff",
    control:
      "Shift+Tab (docs), Outline list (source), Previous change button (diff)",
  },
  {
    id: "nextChange",
    bindings: ["n"],
    keys: ["n"],
    label: "Next change",
    where: "Diff, source (diff mode)",
    control: "Next change button",
  },
  {
    id: "prevChange",
    bindings: ["p"],
    keys: ["p"],
    label: "Previous change",
    where: "Diff, source (diff mode)",
    control: "Previous change button",
  },
  {
    id: "nextFile",
    bindings: ["]"],
    keys: ["]"],
    label: "Next changed file (module in docs)",
    where: "Diff, source, docs (diff mode)",
    control: "Next file button (diff, source); docs: the sidebar, Changed only",
  },
  {
    id: "prevFile",
    bindings: ["["],
    keys: ["["],
    label: "Previous changed file (module in docs)",
    where: "Diff, source, docs (diff mode)",
    control:
      "Previous file button (diff, source); docs: the sidebar, Changed only",
  },
  {
    id: "layout",
    bindings: ["u"],
    keys: ["u"],
    label: "Unified or side by side",
    where: "Diff",
    control: "Unified / Side by side buttons",
  },
  {
    id: "open",
    bindings: [],
    keys: ["Enter"],
    label: "Open the focused link",
    where: "Everywhere",
    control: "Click",
  },
  {
    id: "definition",
    bindings: ["g d"],
    keys: ["g", "d"],
    label: "Go to definition",
    where: "Docs, source",
    control: "Click the name",
  },
  {
    id: "references",
    bindings: ["g r"],
    keys: ["g", "r"],
    label: "Find references",
    where: "Docs, source",
    control: "refs link (Find references)",
  },
  {
    id: "source",
    bindings: ["s"],
    keys: ["s"],
    label: "Show or hide the source of the declaration",
    where: "Docs",
    control: "+ button (Show source)",
  },
  {
    id: "find",
    bindings: ["f"],
    keys: ["f"],
    label: "Find in file",
    where: "Source, diff",
    control: "Find button",
  },
  {
    id: "copy",
    bindings: ["y"],
    keys: ["y"],
    label: "Copy the name or path of the focused item",
    where: "Everywhere",
    control: "Copy buttons, Copy location in the header",
  },
  {
    id: "docsView",
    bindings: ["[Shift]+1"],
    keys: ["1"],
    label: "Docs view",
    where: "Everywhere",
    control: "Docs link in the header",
  },
  {
    id: "sourceView",
    bindings: ["[Shift]+2"],
    keys: ["2"],
    label: "Source view",
    where: "Everywhere",
    control: "Source link in the header",
  },
  {
    id: "diffView",
    bindings: ["[Shift]+3"],
    keys: ["3"],
    label: "Diff view",
    where: "Diff mode",
    control: "Diff link in the header; Show diff here (source)",
  },
  {
    id: "otherSide",
    bindings: ["b"],
    keys: ["b"],
    label: "View on the other side (base or head)",
    where: "Diff mode",
    control: "View on base / head in the header; Other side button (diff)",
  },
  {
    id: "reload",
    bindings: ["r"],
    keys: ["r"],
    label: "Reload the files from disk",
    where: "Browse mode, --diff REV",
    control: "Reload button in the header",
  },
  {
    id: "escape",
    bindings: ["Escape"],
    keys: ["Esc"],
    label: "Close the hover card, then the find bar; else clear the selection",
    where: "Everywhere",
    control: "Close buttons",
  },
  {
    id: "help",
    bindings: ["[Shift]+?"],
    keys: ["?"],
    label: "Show this list",
    where: "Everywhere",
    control: "Shortcuts button in the header",
  },
];

// Keys that may repeat while held down.
export const repeatable: ReadonlySet<string> = new Set(["j", "k", "n", "p"]);

// How long g waits for its second key, in ms.
export const sequenceTimeout = 1000;
