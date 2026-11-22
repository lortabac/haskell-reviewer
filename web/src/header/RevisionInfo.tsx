// The revisions in the header (SPEC §4.1, §5.7, D-335), then the count
// of changed files outside ROOT (D-338). Diff mode only: browse mode
// shows nothing. Short hashes are 7 hex; the copy
// buttons copy the full hash.
import { type JSX, onCleanup, Show } from "solid-js";
import type { SessionEnd } from "../api/session";
import { CopyButton } from "../copy/CopyButton";
import { useSession } from "../session";
import type { Side } from "../sides";
import { OutsideCount } from "./OutsideCount";
import { mergeBaseCount, mergeBaseText, shortHash } from "./revText";

// current: the page's side, on pages with a side in the URL.
export function RevisionInfo(props: { current?: Side }): JSX.Element {
  const session = useSession();
  return (
    <Show when={session.mode === "diff" ? session : undefined}>
      {(s) => (
        <div class="revisions">
          <span class="rev-item">
            <span class="rev-key">Diff</span> <Cut text={s().spec} />
          </span>
          <Base
            base={s().base}
            headLabel={s().head.label}
            mergeBases={s().mergeBases}
          />
          <Head end={s().head} />
          <Show when={props.current}>
            {(c) => (
              <span class="rev-item rev-current">
                <span class="rev-key">Viewing:</span> <span>{c()}</span>
              </span>
            )}
          </Show>
          <OutsideCount />
        </div>
      )}
    </Show>
  );
}

// mergeBases: set for "...": base is then the merge base, with the
// labels in a tip.
function Base(props: {
  base: SessionEnd<string>;
  headLabel: string;
  mergeBases: number | null;
}): JSX.Element {
  const many = () => mergeBaseCount(props.mergeBases);
  return (
    <span class="rev-item">
      <span class="rev-key">base:</span>{" "}
      <Show
        when={props.mergeBases !== null}
        fallback={<Cut text={props.base.label} />}
      >
        {/* A Tab stop, so the keyboard gets the tip too. */}
        <span
          class="rev-merge-base"
          tabindex="0"
          data-tip={mergeBaseText(props.base.label, props.headLabel)}
        >
          merge base
          <span class="sr-only">
            {` of ${props.base.label} and ${props.headLabel}`}
          </span>
        </span>
      </Show>{" "}
      <Hash side="base" commit={props.base.commit} />
      <Show when={many()}>
        {(n) => (
          <>
            {" "}
            <span class="rev-count">({n()})</span>
          </>
        )}
      </Show>
    </span>
  );
}

// No commit: the working tree, with no hash.
function Head(props: { end: SessionEnd<string | null> }): JSX.Element {
  return (
    <span class="rev-item">
      <span class="rev-key">head:</span> <Cut text={props.end.label} />
      <Show when={props.end.commit}>
        {(c) => (
          <>
            {" "}
            <Hash side="head" commit={c()} />
          </>
        )}
      </Show>
    </span>
  );
}

function Hash(props: { side: Side; commit: string }): JSX.Element {
  return (
    <>
      <code class="rev-hash">{shortHash(props.commit)}</code>{" "}
      <CopyButton
        text={() => props.commit}
        label={`Copy ${props.side} commit hash`}
      />
    </>
  );
}

// User text, cut with an ellipsis when long. Only when cut: the full
// text is a tip, and the span is a Tab stop so the keyboard gets the
// tip too. The tip is on the outer span: the inner one clips. A focused
// span that stops being cut keeps both until it loses focus, so focus
// doesn't drop to body.
function Cut(props: { text: string }): JSX.Element {
  let outer!: HTMLElement;
  let cut = false;
  // leaving: focus is going away (focusout), so it is not kept.
  const sync = (leaving = false): void => {
    if (cut) {
      outer.setAttribute("data-tip", props.text);
      outer.setAttribute("tabindex", "0");
    } else if (leaving || document.activeElement !== outer) {
      outer.removeAttribute("data-tip");
      outer.removeAttribute("tabindex");
    }
  };
  const measure = (inner: HTMLElement): void => {
    // None (old browsers, tests): never a tip.
    if (typeof ResizeObserver === "undefined") return;
    // It reports once at the start too, when laid out.
    const ro = new ResizeObserver(() => {
      cut = inner.scrollWidth > inner.clientWidth;
      sync();
    });
    ro.observe(inner);
    onCleanup(() => ro.disconnect());
  };
  return (
    <span
      class="rev-cut"
      ref={(el) => (outer = el)}
      on:focusout={() => sync(true)}
    >
      <code class="rev-cut-text" ref={measure}>
        {props.text}
      </code>
    </span>
  );
}
