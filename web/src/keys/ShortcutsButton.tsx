// "Shortcuts" in the header: opens the ? overlay, also when the keys
// are off (D-297).
import type { JSX } from "solid-js";
import { useKeys } from "./KeysProvider";

export function ShortcutsButton(): JSX.Element {
  const keys = useKeys();
  return (
    <button
      type="button"
      class="shortcuts-button"
      aria-haspopup="dialog"
      onClick={(e) => keys?.openHelp(e.currentTarget)}
    >
      Shortcuts
    </button>
  );
}
