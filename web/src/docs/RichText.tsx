// Haddock comments, from the AST the server sends: JSX only, no raw
// HTML, so text in a comment is always text.
import { createMemo, onCleanup, type JSX } from "solid-js";
import { Dynamic } from "solid-js/web";
import type { Block, Cell, Doc, Inline } from "../api/richtext";
import { type Links, outerAttrs, useLinks } from "./links";
import type { NameInline } from "./relink";

// A code/example/property block (docs.css) only gets a Tab stop while
// it actually scrolls sideways: not every block at every width, only
// the ones with something to scroll to (C4, axe's
// scrollable-region-focusable). ResizeObserver, not just a one-time
// check: a window resize can turn scrolling on or off after the first
// render.
function scrollableRef(el: HTMLElement): void {
  const update = (): void => {
    if (el.scrollWidth > el.clientWidth) el.setAttribute("tabindex", "0");
    else el.removeAttribute("tabindex");
  };
  update();
  const ro = new ResizeObserver(update);
  ro.observe(el);
  onCleanup(() => ro.disconnect());
}

// A comment. level: the heading level it sits under, so a "= Usage"
// heading in it is one below, h6 at most (D-190). stack: tracks the
// comment's own headings so a later one never skips a level even if
// the Haddock markup jumps straight to "===", matching D-192's rule
// for the page's own headings; built inside the expression below, not
// outside it, so a re-run (this sits in a JSX child position, which
// Solid may re-invoke) always starts from an empty stack rather than
// one still holding entries from the run before (C5).
export function DocView(props: { doc: Doc; level: number }): JSX.Element {
  const links = useLinks();
  return (
    <div class="doc">
      {(() => {
        const stack: HeadingStack = [];
        return blocks(props.doc.blocks, props.level, links, stack);
      })()}
      {props.doc.since === null ? null : (
        <p class="doc-since">
          Since:{" "}
          {props.doc.since.package === null
            ? props.doc.since.version
            : `${props.doc.since.package}-${props.doc.since.version}`}
        </p>
      )}
    </div>
  );
}

// Inline text alone, like a section heading.
export function InlineText(props: { inlines: Inline[] }): JSX.Element {
  const links = useLinks();
  return <>{inlines(props.inlines, links)}</>;
}

// One comment's open headings, shallowest first: the Haddock level it
// was written at, and the HTML level it actually got.
type HeadingStack = { haddock: number; rendered: number }[];

// The HTML level for a heading written at Haddock level hLevel: one
// below the last heading still open at this depth or shallower, so it
// never skips one, however far hLevel jumps. The first heading in a
// comment is one below `base` (the level the whole comment sits
// under), whatever its own hLevel says.
function headingLevel(
  hLevel: number,
  base: number,
  stack: HeadingStack,
): number {
  while (stack.length > 0 && stack[stack.length - 1]!.haddock >= hLevel) {
    stack.pop();
  }
  const top = stack[stack.length - 1];
  const rendered = Math.min(6, (top?.rendered ?? base) + 1);
  stack.push({ haddock: hLevel, rendered });
  return rendered;
}

function blocks(
  bs: Block[],
  level: number,
  links: Links,
  stack: HeadingStack,
): JSX.Element[] {
  return bs.map((b) => block(b, level, links, stack));
}

function block(
  b: Block,
  level: number,
  links: Links,
  stack: HeadingStack,
): JSX.Element {
  switch (b.kind) {
    case "paragraph":
      return <p>{inlines(b.inlines, links)}</p>;
    case "heading":
      return (
        <Dynamic
          component={`h${headingLevel(b.level, level, stack)}`}
          class="doc-heading"
        >
          {inlines(b.inlines, links)}
        </Dynamic>
      );
    case "code":
      return (
        <pre class="doc-code" ref={scrollableRef}>
          <code>{inlines(b.inlines, links)}</code>
        </pre>
      );
    case "bullets": {
      const items = b.items.map((item) => (
        <li>{blocks(item, level, links, stack)}</li>
      ));
      return <ul>{items}</ul>;
    }
    case "numbered": {
      const labels = numberLabels(b.items.map((i) => i.number));
      const items = b.items.map((item, k) => (
        <li>
          <span class="doc-number">{labels[k]}.</span>{" "}
          {blocks(item.blocks, level, links, stack)}
        </li>
      ));
      return <ol class="doc-numbered">{items}</ol>;
    }
    case "definitions": {
      const items = b.items.map((item) => [
        <dt>{inlines(item.term, links)}</dt>,
        <dd>{blocks(item.blocks, level, links, stack)}</dd>,
      ]);
      return <dl>{items}</dl>;
    }
    case "examples":
      return (
        <pre class="doc-examples" ref={scrollableRef}>
          {b.items
            .map((e) => [">>> " + e.expression, ...e.results].join("\n"))
            .join("\n")}
        </pre>
      );
    case "property":
      return (
        <pre class="doc-property" ref={scrollableRef}>
          {"prop> " + b.text}
        </pre>
      );
    case "table":
      return (
        <table class="doc-table">
          {b.header.length === 0 ? null : (
            <thead>{rows(b.header, "th", links)}</thead>
          )}
          <tbody>{rows(b.body, "td", links)}</tbody>
        </table>
      );
  }
}

function rows(rs: Cell[][], cell: "th" | "td", links: Links): JSX.Element[] {
  return rs.map((r) => {
    const cells = r.map((c) => (
      <Dynamic
        component={cell}
        colspan={c.colspan > 1 ? c.colspan : undefined}
        rowspan={c.rowspan > 1 ? c.rowspan : undefined}
      >
        {inlines(c.inlines, links)}
      </Dynamic>
    ));
    return <tr>{cells}</tr>;
  });
}

// A name with no local target: a link out, if it has one.
function outName(i: NameInline, links: Links): JSX.Element {
  const name = <code>{i.written}</code>;
  const info = i.link === null ? undefined : links.outer(i.link);
  return info === undefined ? (
    name
  ) : (
    <a {...outerAttrs(i.written, info)}>{name}</a>
  );
}

// The same while the page is partial: drawn again only when its own
// link changes (D-293).
function LiveName(props: { name: NameInline; links: Links }): JSX.Element {
  const now = createMemo(() => props.links.name(props.name));
  return <>{outName(now(), props.links)}</>;
}

// Item labels: the number as written (decimal text, can be over
// 2^53), or one more than the item before when null.
export function numberLabels(numbers: (string | null)[]): string[] {
  let prev = 0n;
  return numbers.map((n) => {
    prev = n === null ? prev + 1n : BigInt(n);
    return prev.toString();
  });
}

function inlines(is: Inline[], links: Links): JSX.Element[] {
  return is.map((i) => inline(i, links));
}

function inline(i: Inline, links: Links): JSX.Element {
  switch (i.kind) {
    case "text":
      return i.text;
    case "emphasis":
      return <em>{inlines(i.inlines, links)}</em>;
    case "bold":
      return <strong>{inlines(i.inlines, links)}</strong>;
    case "code":
      return <code>{inlines(i.inlines, links)}</code>;
    case "math":
      // TeX as text.
      return (
        // eslint-disable-next-line no-restricted-syntax -- a rich-text field, not CSS
        <code class={i.style === "display" ? "doc-math-display" : "doc-math"}>
          {i.tex}
        </code>
      );
    case "link":
      // Off the app: a new tab, no opener, no referrer. docs.css adds a
      // visible ↗.
      return (
        <a
          class="ext-link"
          href={i.url}
          rel="noopener noreferrer"
          target="_blank"
        >
          {i.label === null ? i.url : inlines(i.label, links)}
          <span class="sr-only"> (opens in a new tab)</span>
        </a>
      );
    case "name": {
      if (i.target !== null) {
        const name = <code>{i.written}</code>;
        const href = links.decl(i.target);
        return href === undefined ? name : <a href={href}>{name}</a>;
      }
      return links.live ? (
        <LiveName name={i} links={links} />
      ) : (
        outName(i, links)
      );
    }
    case "module": {
      const text =
        i.label === null ? <code>{i.module}</code> : inlines(i.label, links);
      return i.ref === null ? (
        text
      ) : (
        <a href={links.module(i.ref, i.anchor)}>{text}</a>
      );
    }
  }
}
