// Code on the module page: heads, signatures, types. One span per
// token, with its class (no colours in M1, D-31 comes later); linked
// tokens are links to declarations of this page, or out of it. Static,
// unless the page is partial: then only the tokens whose link changes
// are drawn again (D-293).
import { createMemo, type JSX, untrack } from "solid-js";
import { type Code as Tokens, type CodeToken, outerLink } from "../api/code";
import { type Links, outerAttrs, useLinks } from "./links";
import { wrapParts } from "./signature";

function token(t: CodeToken, links: Links): JSX.Element {
  const [cls, text, n] = t;
  if (cls === "sp") return text;
  if (typeof n === "number") {
    // A declaration of this page.
    const href = links.decl(n);
    return href === undefined ? (
      <span class={`tok-${cls}`}>{text}</span>
    ) : (
      <a class={`tok-${cls}`} href={href}>
        {text}
      </a>
    );
  }
  // Out of the module: docs of another one, or Hoogle. Never a pragma.
  const k = outerLink(t);
  const info = k === null || cls === "prag" ? undefined : links.outer(k);
  if (info === undefined) return <span class={`tok-${cls}`}>{text}</span>;
  const { class: extra, ...attrs } = outerAttrs(text, info);
  return (
    <a
      class={extra === undefined ? `tok-${cls}` : `tok-${cls} ${extra}`}
      {...attrs}
    >
      {text}
    </a>
  );
}

function tokens(code: Tokens, links: Links): JSX.Element {
  // An accessor as a child: Solid tracks it on its own (the types
  // don't say so).
  return links.live
    ? (liveTokens(code, links) as unknown as JSX.Element)
    : code.map((t) => token(t, links));
}

// Tokens as they are now. One memo per block, not per token (a big
// page has many): a token with the same object as last time keeps its
// element, a changed one gets a new one. The elements are owned by the
// memo, so a re-run drops their effects: token() must stay static (no
// reactive attrs, onCleanup or cards), else each needs its own root.
function liveTokens(code: Tokens, links: Links): () => JSX.Element {
  let was: CodeToken[] = [];
  let els: JSX.Element[] = [];
  return createMemo((): JSX.Element => {
    const now = code.map((t) => links.token(t));
    // Nothing changed here: the same array, so nothing is redrawn.
    if (now.length === was.length && now.every((t, i) => t === was[i])) {
      return els;
    }
    els = now.map((t, i) =>
      t === was[i] ? els[i] : untrack(() => token(t, links)),
    );
    was = now;
    return els;
  });
}

// Code on one line (it can still wrap anywhere if too long).
export function Code(props: { code: Tokens }): JSX.Element {
  const links = useLinks();
  return <code class="code">{tokens(props.code, links)}</code>;
}

// A head or signature that wraps before top-level arrows (SPEC §5.5):
// each part is a box that stays whole if it fits, and the lines after
// the first are indented (docs.css). Never scrolls sideways.
export function Signature(props: { code: Tokens }): JSX.Element {
  const links = useLinks();
  // Cut by text only: links never move the cuts.
  const parts = () =>
    wrapParts(props.code).flatMap((part, i) => [
      i === 0 ? null : " ",
      <span class="sig-part">{tokens(part, links)}</span>,
    ]);
  return <code class="sig">{parts()}</code>;
}
