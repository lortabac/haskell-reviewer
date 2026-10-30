// Code on the module page: heads, signatures, types. One span per
// token, with its class (no colours in M1, D-31 comes later); linked
// tokens are links to declarations of this page. Static: code doesn't
// change while the page shows.
import type { JSX } from "solid-js";
import type { Code as Tokens, CodeToken } from "../api/code";
import { type Links, useLinks } from "./links";
import { wrapParts } from "./signature";

function token(t: CodeToken, links: Links): JSX.Element {
  const [cls, text, n] = t;
  if (cls === "sp") return text;
  const href = n === undefined ? undefined : links.decl(n);
  return href === undefined ? (
    <span class={`tok-${cls}`}>{text}</span>
  ) : (
    <a class={`tok-${cls}`} href={href}>
      {text}
    </a>
  );
}

function tokens(code: Tokens, links: Links): JSX.Element[] {
  return code.map((t) => token(t, links));
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
  const parts = () =>
    wrapParts(props.code).flatMap((part, i) => [
      i === 0 ? null : " ",
      <span class="sig-part">{tokens(part, links)}</span>,
    ]);
  return <code class="sig">{parts()}</code>;
}
