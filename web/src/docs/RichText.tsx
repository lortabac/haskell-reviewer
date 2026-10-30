// Haddock comments, from the AST the server sends: JSX only, no raw
// HTML, so text in a comment is always text.
import type { JSX } from "solid-js";
import { Dynamic } from "solid-js/web";
import type { Block, Cell, Doc, Inline } from "../api/richtext";
import { type Links, useLinks } from "./links";

// A comment. level: the heading level it sits under, so a "= Usage"
// heading in it is one below (h6 at most).
export function DocView(props: { doc: Doc; level: number }): JSX.Element {
  const links = useLinks();
  return (
    <div class="doc">
      {blocks(props.doc.blocks, props.level, links)}
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

function blocks(bs: Block[], level: number, links: Links): JSX.Element[] {
  return bs.map((b) => block(b, level, links));
}

function block(b: Block, level: number, links: Links): JSX.Element {
  switch (b.kind) {
    case "paragraph":
      return <p>{inlines(b.inlines, links)}</p>;
    case "heading":
      return (
        <Dynamic
          component={`h${Math.min(6, level + Math.max(1, b.level))}`}
          class="doc-heading"
        >
          {inlines(b.inlines, links)}
        </Dynamic>
      );
    case "code":
      return (
        <pre class="doc-code">
          <code>{inlines(b.inlines, links)}</code>
        </pre>
      );
    case "bullets": {
      const items = b.items.map((item) => (
        <li>{blocks(item, level, links)}</li>
      ));
      return <ul>{items}</ul>;
    }
    case "numbered": {
      const labels = numberLabels(b.items.map((i) => i.number));
      const items = b.items.map((item, k) => (
        <li>
          <span class="doc-number">{labels[k]}.</span>{" "}
          {blocks(item.blocks, level, links)}
        </li>
      ));
      return <ol class="doc-numbered">{items}</ol>;
    }
    case "definitions": {
      const items = b.items.map((item) => [
        <dt>{inlines(item.term, links)}</dt>,
        <dd>{blocks(item.blocks, level, links)}</dd>,
      ]);
      return <dl>{items}</dl>;
    }
    case "examples":
      return (
        <pre class="doc-examples">
          {b.items
            .map((e) => [">>> " + e.expression, ...e.results].join("\n"))
            .join("\n")}
        </pre>
      );
    case "property":
      return <pre class="doc-property">{"prop> " + b.text}</pre>;
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
      const href = i.target === null ? undefined : links.decl(i.target);
      const name = <code>{i.written}</code>;
      return href === undefined ? name : <a href={href}>{name}</a>;
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
