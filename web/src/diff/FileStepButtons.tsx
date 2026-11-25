// Previous file / Next file in the diff and source toolbars, like [ and
// ] (D-350). On its own, so it is easy to move: it reads the session,
// the prefs and the keys itself. Diff mode only.
//
// After a press, the page changes and this button with it. The same
// button on the new page takes focus back (KeysProvider's takeRefocus)
// when it mounts, if focus is nowhere or on the page's heading. So it
// works only while the buttons are part of each page (T22: keep that
// in mind if they move).
import { type JSX, onMount, Show } from "solid-js";
import { useKeys } from "../keys/KeysProvider";
import { usePrefs } from "../prefs/store";
import { useSession } from "../session";
import { focusLost } from "../ui/focus";
import { fileStepTexts } from "./words";

// Nothing has focus, or only the heading the new page focused.
function noRealFocus(): boolean {
  return focusLost() || document.activeElement?.matches("#main h1") === true;
}

function StepButton(props: { dir: 1 | -1 }): JSX.Element {
  const keys = useKeys();
  const prefs = usePrefs();
  const text = () => {
    const words =
      props.dir === 1 ? fileStepTexts.nextFile : fileStepTexts.prevFile;
    const key = props.dir === 1 ? "]" : "[";
    return prefs.singleKeyShortcuts() ? `${words} (${key})` : words;
  };
  let el!: HTMLButtonElement;
  onMount(() => {
    if (keys?.takeRefocus(props.dir) === true && noRealFocus()) {
      el.focus({ preventScroll: true });
    }
  });
  return (
    <button
      type="button"
      ref={(e) => (el = e)}
      onClick={() => keys?.stepFile(props.dir, true)}
    >
      {text()}
    </button>
  );
}

export function FileStepButtons(): JSX.Element {
  const session = useSession();
  return (
    <Show when={session.mode === "diff"}>
      <StepButton dir={-1} />
      <StepButton dir={1} />
    </Show>
  );
}
