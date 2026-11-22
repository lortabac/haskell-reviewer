// A diff marker (D-337): a sign in colour for the eye, a word for
// screen readers. Colour is never the only cue: the sign differs too.
import { For, type JSX } from "solid-js";
import type { Side } from "../sides";

export type Change = "added" | "removed" | "modified" | "renamed" | "settings";

export const changeSigns: Record<Change, string> = {
  added: "+",
  removed: "−",
  modified: "~",
  renamed: "→",
  // Text style, not emoji.
  settings: "⚙︎",
};

export const changeWords: Record<Change, string> = {
  added: "added",
  removed: "removed",
  modified: "modified",
  renamed: "renamed",
  settings: "settings changed",
};

// word: what screen readers hear, if not the plain word (a rename's
// "renamed from a").
export function ChangeSign(props: {
  change: Change;
  word?: string | undefined;
}): JSX.Element {
  return (
    <span class={`change-sign change-${props.change}`}>
      {" "}
      <span aria-hidden="true">{changeSigns[props.change]}</span>
      <span class="sr-only">{props.word ?? changeWords[props.change]}</span>
    </span>
  );
}

// The legend's words: short ones.
const legendWords: Record<Change, string> = {
  ...changeWords,
  settings: "settings",
};

const legendOrder: readonly Change[] = [
  "added",
  "removed",
  "modified",
  "renamed",
  "settings",
];

// The legend under the "Changed only" box. The signs are hidden from
// screen readers: they hear the words on each row anyway.
export function ChangeLegend(): JSX.Element {
  return (
    <p class="change-legend">
      <For each={legendOrder}>
        {(c, i) => (
          <>
            {i() > 0 ? " · " : ""}
            <span class="change-legend-item">
              <span class={`change-${c}`} aria-hidden="true">
                {changeSigns[c]}
              </span>{" "}
              {legendWords[c]}
            </span>
          </>
        )}
      </For>
    </p>
  );
}

// What a ghost says: where it is.
export function ghostLabel(on: Side): string {
  return `(on ${on})`;
}

// "(on base)" after a ghost's name.
export function GhostSide(props: { on: Side }): JSX.Element {
  return (
    <>
      {" "}
      <span class="tree-ghost-side">{ghostLabel(props.on)}</span>
    </>
  );
}
