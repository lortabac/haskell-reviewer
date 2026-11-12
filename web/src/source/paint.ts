// A line's pieces as DOM nodes: plain text as text, a class as
// <span class="hl-<class>">, control glyphs as <span class="code-ctrl">.
// Adjacent pieces with the same link go in one <a class="code-link">.
// Text only (textContent), never markup. No Solid: a row can have
// thousands of them, and nothing in them changes.
import type { StyledPiece } from "./display";
import { type LinkInfo, linkName } from "./links";

// link: the LinkInfo of a link word; undefined: no link.
export function lineNodes(
  ps: readonly StyledPiece[],
  link?: (n: number) => LinkInfo | undefined,
): Node[] {
  const out: Node[] = [];
  const named: [HTMLAnchorElement, LinkInfo][] = [];
  let a: HTMLAnchorElement | undefined;
  let aLink = 0;
  for (const p of ps) {
    const node = pieceNode(p);
    const info = p.link === 0 ? undefined : link?.(p.link);
    if (info === undefined) {
      a = undefined;
      aLink = 0;
      out.push(node);
      continue;
    }
    if (a === undefined || aLink !== p.link) {
      a = anchor(p.link, info);
      aLink = p.link;
      out.push(a);
      named.push([a, info]);
    }
    a.append(node);
  }
  for (const [el, info] of named) {
    el.setAttribute("aria-label", linkName(el.textContent, info));
  }
  return out;
}

function pieceNode(p: StyledPiece): Node {
  if (!p.ctrl && p.cls === null) return document.createTextNode(p.text);
  const s = document.createElement("span");
  s.className = p.ctrl ? "code-ctrl" : `hl-${p.cls}`;
  s.textContent = p.text;
  return s;
}

function anchor(link: number, info: LinkInfo): HTMLAnchorElement {
  const a = document.createElement("a");
  a.className = info.guess ? "code-link code-link-guess" : "code-link";
  a.href = info.href;
  a.tabIndex = -1;
  a.setAttribute("draggable", "false");
  a.dataset.link = String(link);
  if (info.guess) a.title = "Resolved heuristically";
  if (info.external) {
    a.target = "_blank";
    a.rel = "noopener noreferrer";
  } else if (info.here) {
    // Same file: the router must not scroll the page to the top.
    a.setAttribute("noscroll", "");
  } else {
    // Another file: focus its target line (FileView).
    a.setAttribute("state", JSON.stringify({ codeJump: true }));
  }
  return a;
}
