// A copy button (SPEC §5.7, D-299), shown on hover and focus. It has
// no text: the word comes from CSS, so it is never in a copied
// selection. text is read when pressed.
import { createSignal, type JSX, onCleanup } from "solid-js";
import { useKeys } from "../keys/KeysProvider";
import { copyText } from "./clipboard";

export function CopyButton(props: {
  text: () => string | undefined;
  // Its accessible name, e.g. "Copy Data.Foo.bar". Keep it short.
  label: string;
  // Not a Tab stop, not in the accessibility tree (the trees: the
  // keyboard has y).
  mouseOnly?: boolean;
}): JSX.Element {
  const keys = useKeys();
  const [done, setDone] = createSignal(false);
  let timer: ReturnType<typeof setTimeout> | undefined;
  onCleanup(() => clearTimeout(timer));
  const onClick = () => {
    const t = props.text();
    if (t === undefined) return;
    // At once, in the click (WebKit).
    void copyText(t, keys?.say).then((ok) => {
      if (!ok) return;
      setDone(true);
      clearTimeout(timer);
      timer = setTimeout(() => setDone(false), 1500);
    });
  };
  return (
    <button
      type="button"
      class="copy-button"
      aria-label={props.label}
      tabindex={props.mouseOnly === true ? -1 : undefined}
      aria-hidden={props.mouseOnly === true ? "true" : undefined}
      data-state={done() ? "copied" : undefined}
      // No focus on a press: y must still see the row's link.
      on:mousedown={(e) => {
        if (props.mouseOnly === true) e.preventDefault();
      }}
      onClick={onClick}
    />
  );
}
