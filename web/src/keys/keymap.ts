// The single-key shortcuts (SPEC §5.3, D-297, D-298): one table for
// the dispatcher and the ? overlay. Bindings are tinykeys strings,
// matched by event.key. Shift is allowed only where the key itself
// may need it on some layouts (? / 1 2).

export type ActionId =
  | "search"
  | "next"
  | "prev"
  | "open"
  | "definition"
  | "references"
  | "source"
  | "find"
  | "docsView"
  | "sourceView"
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
    label: "Focus the search box",
    where: "Everywhere",
    control: "Search box in the header",
  },
  {
    id: "next",
    bindings: ["j"],
    keys: ["j"],
    label: "Next declaration",
    where: "Docs, source",
    control: "Tab (docs), Outline list (source)",
  },
  {
    id: "prev",
    bindings: ["k"],
    keys: ["k"],
    label: "Previous declaration",
    where: "Docs, source",
    control: "Shift+Tab (docs), Outline list (source)",
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
    control: "Find references link, References button",
  },
  {
    id: "source",
    bindings: ["s"],
    keys: ["s"],
    label: "Show or hide the source of the declaration",
    where: "Docs",
    control: "Show source button",
  },
  {
    id: "find",
    bindings: ["f"],
    keys: ["f"],
    label: "Find in file",
    where: "Source",
    control: "Find button",
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
export const repeatable: ReadonlySet<string> = new Set(["j", "k"]);

// How long g waits for its second key, in ms.
export const sequenceTimeout = 1000;
