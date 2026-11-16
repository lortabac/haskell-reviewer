// A text hit's line: "…" if cut, the hits in <mark>, control
// characters as glyphs (as the source view draws them).
import { For, type JSX, Show } from "solid-js";
import type { TextHit } from "../api/textSearch";
import { plainPieces } from "../source/display";
import { textSnippet } from "./snippet";

export function SnippetText(props: { h: TextHit; lead?: number }): JSX.Element {
  const s = () => textSnippet(props.h, props.lead);
  const pieces = (t: string) => (
    <For each={plainPieces(t)}>
      {(p) => <span class={p.ctrl ? "code-ctrl" : undefined}>{p.text}</span>}
    </For>
  );
  return (
    <>
      <Show when={s().cut}>…</Show>
      <For each={s().parts}>
        {(p) =>
          p.hit ? <mark class="ref-hit">{pieces(p.text)}</mark> : pieces(p.text)
        }
      </For>
    </>
  );
}
