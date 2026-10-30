// A declaration's source, inline: a button that shows or hides it.
// Plain text with line numbers, "⋯" rows for lines left out (D-195).
// Built only when open.
import {
  createMemo,
  createSignal,
  createUniqueId,
  For,
  type JSX,
  Show,
} from "solid-js";
import type { SourcePart } from "../api/module";
import { usePage } from "./context";
import { type SourceRow, sourceRows, textParts } from "./source";

export function SourceToggle(props: {
  parts: SourcePart[];
  // For screen readers: whose source.
  label: string;
  // Rest of the actions row (go to source).
  children?: JSX.Element;
}): JSX.Element {
  const [open, setOpen] = createSignal(false);
  const id = createUniqueId();
  return (
    <>
      <p class="decl-actions">
        <button
          type="button"
          class="decl-source-toggle"
          aria-expanded={open()}
          aria-controls={id}
          onClick={() => setOpen(!open())}
        >
          {open() ? "Hide source" : "Show source"}
          <span class="sr-only"> of {props.label}</span>
        </button>
        {props.children}
      </p>
      <div id={id} class="decl-source">
        <Show when={open()}>
          <SourceRows parts={props.parts} />
        </Show>
      </div>
    </>
  );
}

function SourceRows(props: { parts: SourcePart[] }): JSX.Element {
  const page = usePage();
  const rows = createMemo(() =>
    sourceRows(textParts(props.parts, (p) => page.ix.sentPart(p))),
  );
  return (
    <pre class="src" tabindex="0">
      <For each={rows()}>{row}</For>
    </pre>
  );
}

function row(r: SourceRow): JSX.Element {
  if (r.kind === "line") {
    return (
      <span class="src-row">
        <span class="src-num">{r.line}</span>
        {r.text + "\n"}
      </span>
    );
  }
  return (
    <span class="src-row src-gap">
      {r.from === r.to ? `⋯ line ${r.from}\n` : `⋯ lines ${r.from}–${r.to}\n`}
    </span>
  );
}
