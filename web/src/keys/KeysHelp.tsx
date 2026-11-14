// The ? overlay (D-297): a modal <dialog> with the on/off switch and
// the key table. Focus goes to the switch, Tab stays inside, and
// focus goes back where it was on close.
import {
  createEffect,
  createUniqueId,
  For,
  Index,
  type JSX,
  on,
  Show,
} from "solid-js";
import { usePrefs } from "../prefs/store";
import { keymap } from "./keymap";

export function KeysHelp(props: {
  open: boolean;
  // Focus goes there on close when nothing had it before.
  opener?: () => HTMLElement | undefined;
  onClose: () => void;
}): JSX.Element {
  const prefs = usePrefs();
  const titleId = createUniqueId();
  let dialog!: HTMLDialogElement;
  let toggle!: HTMLInputElement;
  let back: Element | null = null;

  createEffect(
    on(
      () => props.open,
      (open) => {
        if (open && !dialog.open) {
          back = document.activeElement;
          dialog.showModal();
          toggle.focus();
        } else if (!open && dialog.open) dialog.close();
      },
    ),
  );
  // Esc, the Close button or close() from above all end here.
  const onClose = () => {
    props.onClose();
    const to = back;
    back = null;
    const opener = props.opener?.();
    if (to instanceof HTMLElement && to.isConnected && to !== document.body) {
      to.focus();
    } else if (opener?.isConnected === true) opener.focus();
    else document.getElementById("main")?.focus();
  };
  // Tab wraps: it never leaves for the browser's own UI.
  const onKeyDown = (e: KeyboardEvent) => {
    if (e.key !== "Tab" || e.ctrlKey || e.metaKey || e.altKey) return;
    const stops = [
      ...dialog.querySelectorAll<HTMLElement>("input, button, a[href]"),
    ];
    const first = stops[0];
    const last = stops.at(-1);
    if (e.shiftKey && document.activeElement === first) {
      e.preventDefault();
      last?.focus();
    } else if (!e.shiftKey && document.activeElement === last) {
      e.preventDefault();
      first?.focus();
    }
  };

  return (
    <dialog
      ref={(el) => (dialog = el)}
      class="keys-help"
      aria-labelledby={titleId}
      onClose={onClose}
      onKeyDown={onKeyDown}
    >
      <h2 id={titleId}>Keyboard shortcuts</h2>
      <label class="keys-help-toggle">
        <input
          ref={(el) => (toggle = el)}
          type="checkbox"
          checked={prefs.singleKeyShortcuts()}
          onChange={(e) => prefs.setSingleKeyShortcuts(e.currentTarget.checked)}
        />{" "}
        Single-key shortcuts
      </label>
      <p class="keys-help-note" aria-live="polite">
        {prefs.singleKeyShortcuts()
          ? ""
          : "Off: the keys below do nothing. Use the buttons and links."}
      </p>
      <p>Keys don't work while you type in a text box.</p>
      <table class="keys-help-table">
        <thead>
          <tr>
            <th scope="col">Key</th>
            <th scope="col">Action</th>
            <th scope="col">Where</th>
            <th scope="col">Or use</th>
          </tr>
        </thead>
        <tbody>
          <For each={keymap}>
            {(row) => (
              <tr>
                <th scope="row">
                  <Index each={row.keys}>
                    {(k, i) => (
                      <>
                        <Show when={i > 0}> then </Show>
                        <kbd>{k()}</kbd>
                      </>
                    )}
                  </Index>
                </th>
                <td>{row.label}</td>
                <td>{row.where}</td>
                <td>{row.control}</td>
              </tr>
            )}
          </For>
        </tbody>
      </table>
      <button type="button" onClick={() => dialog.close()}>
        Close
      </button>
    </dialog>
  );
}
