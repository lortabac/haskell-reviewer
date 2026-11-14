// "Copy location" in the header (SPEC §4.1, D-299). Shown while a view
// has a location to give.
import { type JSX, Show } from "solid-js";
import { useKeys } from "../keys/KeysProvider";

export function CopyLocation(): JSX.Element {
  const keys = useKeys();
  return (
    <Show when={keys?.location()}>
      <button
        type="button"
        class="copy-location"
        // Keep focus and the selection where they are (WebKit moves
        // them on a press). Keyboard and click still work.
        on:mousedown={(e) => e.preventDefault()}
        onClick={() => keys?.copyLocation()}
      >
        Copy location
      </button>
    </Show>
  );
}
