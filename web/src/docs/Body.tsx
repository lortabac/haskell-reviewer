// The parts of a module page below the header: synopsis, body (by
// sections.ts), unattached instances, pragmas.
import { createMemo, createSignal, For, type JSX, Show } from "solid-js";
import { Dynamic } from "solid-js/web";
import type { Code as Tokens } from "../api/code";
import type {
  AnyDecl,
  Item,
  Located,
  ParsedModule,
  Pragma,
} from "../api/module";
import { useModules } from "../modules/store";
import { Code } from "./Code";
import { usePage } from "./context";
import { Decl } from "./Decl";
import { headingTag, sigHead, WarningNotice } from "./decls/common";
import { useLinks } from "./links";
import { DocView, InlineText } from "./RichText";
import { type Block, type Body, synopsis, type SynopsisDecl } from "./sections";
import { isOperator } from "./signature";

// A heading of the body: level 1 is h2 (h1 is the module name).
const headLevel = (level: number) => level + 1;
// A declaration under a heading of that level.
const declLevel = (depth: number) => depth + 2;

export function PageBody(props: {
  parsed: ParsedModule;
  // From sections.ts body, built once in ModulePage (anchors use it).
  body: Body;
  exportedOnly: boolean;
}): JSX.Element {
  const b = () => props.body;
  return (
    <>
      <Synopsis parsed={props.parsed} exportedOnly={props.exportedOnly} />
      <Show
        when={b().blocks.length > 0 || b().instances.length > 0}
        fallback={<p>No declarations.</p>}
      >
        <For each={b().blocks}>{blockView}</For>
      </Show>
      <Show when={b().instances.length > 0}>
        <h2 class="docs-section">Instances</h2>
        <For each={b().instances}>{(d) => <Decl decl={d} level={3} />}</For>
      </Show>
      <Pragmas pragmas={props.parsed.pragmas} />
    </>
  );
}

// A block never changes: a plain function, not a component.
function blockView(x: Block): JSX.Element {
  switch (x.kind) {
    case "heading":
      return (
        <Dynamic
          component={headingTag(headLevel(x.level))}
          class="docs-section"
        >
          <InlineText inlines={x.inlines} />
        </Dynamic>
      );
    case "doc":
      return <DocView doc={x.doc} level={headLevel(x.depth)} />;
    case "warning":
      return <WarningNotice warning={x.warning} />;
    case "decl":
      return <Decl decl={x.decl} level={declLevel(x.depth)} />;
    case "reexport":
      return <Reexport item={x.item} />;
    case "module":
      return <ModuleExport name={x.name} link={x.link} />;
    case "nested":
      return <NestedExport occ={x.name.occ} decl={x.decl} />;
  }
}

// How an export list writes a name: (op), with its keyword.
function written(item: Item): string {
  const n = item.name;
  const occ = isOperator(n.occ) ? `(${n.occ})` : n.occ;
  const q = n.qualifier === null ? occ : `${n.qualifier}.${occ}`;
  return n.keyword === null ? q : `${n.keyword} ${q}`;
}

function Reexport(props: { item: Item }): JSX.Element {
  return (
    <p class="docs-reexport">
      <code>{written(props.item)}</code> (re-exported)
    </p>
  );
}

// link: false when X is an import alias of another module.
function ModuleExport(props: { name: Located; link: boolean }): JSX.Element {
  const modules = useModules();
  const links = useLinks();
  const ref = () =>
    props.link
      ? modules.names().find((m) => m.name === props.name.value)?.ref
      : undefined;
  return (
    <p class="docs-reexport">
      <code>
        module{" "}
        <Show when={ref()} fallback={props.name.value}>
          {(r) => <a href={links.module(r(), null)}>{props.name.value}</a>}
        </Show>
      </code>{" "}
      (re-exported)
    </p>
  );
}

// A method or field exported on its own: a link to it, in its parent.
function NestedExport(props: { occ: string; decl: AnyDecl }): JSX.Element {
  const page = usePage();
  const links = useLinks();
  const parent = () => page.ix.topOf(props.decl).name;
  return (
    <p class="docs-reexport">
      <Show when={links.decl(props.decl.n)} fallback={<code>{props.occ}</code>}>
        {(h) => (
          <a href={h()}>
            <code>{props.occ}</code>
          </a>
        )}
      </Show>
      <Show when={parent()}>
        {(p) => (
          <>
            {" "}
            (in <code>{p()}</code>)
          </>
        )}
      </Show>
    </p>
  );
}

// Closed list of one line per top-level type, class, value and pattern
// synonym, each a link to it (D-193). Built only when open.
function Synopsis(props: {
  parsed: ParsedModule;
  exportedOnly: boolean;
}): JSX.Element {
  const page = usePage();
  const links = useLinks();
  const [open, setOpen] = createSignal(false);
  const decls = createMemo(() => synopsis(props.parsed, props.exportedOnly));
  const line = (d: SynopsisDecl) => {
    const code: Tokens =
      d.kind === "value"
        ? sigHead(d.signature, d.name ?? "", page.ix, null).code
        : d.kind === "patsyn"
          ? sigHead(d.signature, d.name ?? "", page.ix, "pattern").code
          : d.head;
    return code.map((t) => t[1]).join("");
  };
  return (
    <Show when={decls().length > 0}>
      <details
        class="docs-synopsis"
        open={open()}
        onToggle={(e) => setOpen(e.currentTarget.open)}
      >
        <summary>Synopsis ({decls().length})</summary>
        <Show when={open()}>
          <ul>
            <For each={decls()}>
              {(d) => (
                <li>
                  <Show
                    when={links.decl(d.n)}
                    fallback={<code>{line(d)}</code>}
                  >
                    {(h) => (
                      <a href={h()}>
                        <code>{line(d)}</code>
                      </a>
                    )}
                  </Show>
                </li>
              )}
            </For>
          </ul>
        </Show>
      </details>
    </Show>
  );
}

// RULES, COMPLETE, ANN, default: closed, at the end. Built only when
// open.
function Pragmas(props: { pragmas: Pragma[] }): JSX.Element {
  const [open, setOpen] = createSignal(false);
  return (
    <Show when={props.pragmas.length > 0}>
      <details
        class="docs-pragmas"
        open={open()}
        onToggle={(e) => setOpen(e.currentTarget.open)}
      >
        <summary>Pragmas ({props.pragmas.length})</summary>
        <Show when={open()}>
          <ul class="decl-lines">
            <For each={props.pragmas}>
              {(p) => (
                <li>
                  <Code code={p.code} />
                </li>
              )}
            </For>
          </ul>
        </Show>
      </details>
    </Show>
  );
}
