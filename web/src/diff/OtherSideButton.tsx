// The diff toolbar's Other side button, like b (D-349). On its own, so
// it is easy to move: it reads the prefs and keys itself.
import type { JSX } from "solid-js";
import { useKeys } from "../keys/KeysProvider";
import { usePrefs } from "../prefs/store";

export function OtherSideButton(): JSX.Element {
  const keys = useKeys();
  const prefs = usePrefs();
  return (
    <button type="button" onClick={() => keys?.goSide()}>
      Other side{prefs.singleKeyShortcuts() ? " (b)" : ""}
    </button>
  );
}
