// What a highlight gives CodeView: renderLine (the line's nodes) and
// lineDecor (CPP kind, outline marker).
import type { LineDecor } from "./CodeView";
import { styledPieces } from "./display";
import type { Highlight } from "./highlight";
import { lineNodes } from "./paint";

export function highlightedLine(h: Highlight) {
  return (n: number, text: string): Node[] =>
    lineNodes(styledPieces(text, h.runs[n] ?? []));
}

// docs: an id's docs URL, undefined if the file has no docs page. The
// marker is not a Tab stop: the outline list is the keyboard way.
export function highlightDecor(
  h: Highlight,
  docs: (id: string) => string | undefined,
) {
  return (n: number): LineDecor | undefined => {
    const kind = h.kinds[n];
    const e = h.byLine.get(n);
    const href = e === undefined ? undefined : docs(e[1]);
    let mark: HTMLAnchorElement | undefined;
    if (e !== undefined && href !== undefined) {
      mark = document.createElement("a");
      mark.className = "code-outline-mark";
      mark.href = href;
      mark.tabIndex = -1;
      mark.title = `Docs: ${e[2]}`;
      mark.setAttribute("aria-label", mark.title);
    }
    if (kind === "excluded" || kind === "directive") return { kind, mark };
    return mark === undefined ? undefined : { mark };
  };
}
