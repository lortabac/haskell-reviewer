// One declaration on the module page: a section with its head or
// signature, badges, warning, docs, what its kind has (constructors,
// methods...), the inline source and "go to source". Top-level ones,
// and instances in a list.
import { createMemo, createUniqueId, For, type JSX, Show } from "solid-js";
import { Dynamic } from "solid-js/web";
import type { Code as Tokens } from "../api/code";
import type {
  AssociatedFamInst,
  Declaration,
  DeclInfo,
  Handle,
} from "../api/module";
import { Signature } from "./Code";
import { usePage, useAnchor } from "./context";
import { ClassDetails } from "./decls/Classes";
import {
  Badges,
  Docs,
  exportMark,
  headingTag,
  type SharedName,
  SharedWith,
  sigHead,
  Sub,
  Title,
  Warning,
} from "./decls/common";
import { TypeDetails } from "./decls/Types";
import { PatSynDetails, ValueDetails } from "./decls/Values";
import { useLinks } from "./links";
import type { PageIndex } from "./pageIndex";
import { SourceToggle } from "./SourceToggle";
import { firstCodeLine } from "./source";

export { WarningNotice } from "./decls/common";

// What a section can show: a top-level declaration, or a family
// instance inside an instance (in its family's list).
export type Shown = Declaration | AssociatedFamInst;

export function Decl(props: {
  decl: Shown;
  // Its heading level.
  level: number;
  // The type, class or family whose instance list it is in.
  home?: Handle;
}): JSX.Element {
  const page = usePage();
  const head = createMemo(() => headOf(props.decl, page.ix));
  const headId = createUniqueId();
  const n = (d: Shown) => (d.kind === "splice" ? undefined : d.n);
  // eslint-disable-next-line solid/reactivity -- a handle never changes
  const anchor = useAnchor(n(props.decl));
  // For screen readers: "Show source of <this>".
  const what = () => readable(props.decl, page.ix);
  const source = () => ("source" in props.decl ? props.decl.source : []);
  const href = () => {
    const line = firstCodeLine(source());
    return line === undefined ? undefined : page.sourceHref?.(line);
  };
  const derivedFor = () => {
    const f = head().derivedFor;
    return f === undefined || f.n === props.home ? undefined : f;
  };
  return (
    <section
      ref={anchor}
      class="decl"
      tabindex="-1"
      data-handle={n(props.decl)}
      aria-labelledby={headId}
    >
      <Dynamic
        component={headingTag(props.level)}
        id={headId}
        class="decl-head"
      >
        <Signature code={head().code} />
        <Badges
          fixity={props.decl.fixity}
          labels={labels(props.decl)}
          mark={mark(props.decl, page.marks)}
        />
      </Dynamic>
      <Show when={props.decl.kind === "splice"}>
        <p class="decl-note">Template Haskell splice.</p>
      </Show>
      <SharedWith sender={head().shared} />
      <Show when={derivedFor()}>{(d) => <DerivedFor decl={d()} />}</Show>
      <Warning info={props.decl} />
      <Docs info={props.decl} level={props.level} />
      {details(props.decl, props.level)}
      <Show when={source().length > 0}>
        <SourceToggle parts={source()} label={what()}>
          <Show when={href()}>
            {(h) => (
              <>
                {" "}
                <a href={h()}>
                  Go to source
                  <span class="sr-only"> of {what()}</span>
                </a>
              </>
            )}
          </Show>
        </SourceToggle>
      </Show>
    </section>
  );
}

// A name a screen reader can say: the name, "instance Eq (Box a)",
// "the splice at line 7".
export function readable(d: Shown, ix: PageIndex): string {
  if (d.kind === "splice") return `the splice at line ${d.line}`;
  if (d.kind !== "instance" && d.kind !== "faminst") return d.name ?? "";
  // From the id's parts, not its text: a class can be an operator
  // with ":" in it (:<:).
  const id = d.id;
  let cls: string;
  let head: string;
  if (typeof id === "object" && "inst" in id) {
    cls = id.inst;
    head = id.head.map((k) => ix.page.idParts[k] ?? "").join("");
  } else {
    cls = d.kind === "faminst" ? d.family : ix.idOf(d);
    head = "";
  }
  const arg = /\s/.test(head) && !/^[([]/.test(head) ? `(${head})` : head;
  const kw = d.kind === "faminst" ? `${d.form} instance` : "instance";
  return arg === "" ? `${kw} ${cls}` : `${kw} ${cls} ${arg}`;
}

// orphan, foreign import. The export mark (D-203) is separate: mark().
function labels(d: Shown): string[] {
  const out: string[] = [];
  switch (d.kind) {
    case "instance":
    case "faminst":
      if (d.orphan) out.push("orphan");
      break;
    case "value":
      if (d.definition === "foreign-import") out.push("foreign import");
      break;
    default:
      break;
  }
  return out;
}

// D-203, plan choice 1a: nothing on an exported top-level declaration;
// a muted "private" on an unexported one. Never on instances or
// family instances (D-186: they're always exported) or splices (no
// name to mark); never in mode B, where the export-list grouping
// already says so in its own headings.
function mark(d: Shown, marks: boolean): string | undefined {
  if (!marks) return undefined;
  switch (d.kind) {
    case "instance":
    case "faminst":
    case "splice":
      return undefined;
    default:
      return exportMark(d.exported, true);
  }
}

// What only this kind has. A declaration never changes: a plain
// function, not a component.
function details(x: Shown, level: number): JSX.Element {
  switch (x.kind) {
    case "type":
      return (
        <TypeDetails t={x} home={x.n} exported={x.exported} level={level} />
      );
    case "class":
      return <ClassDetails c={x} n={x.n} exported={x.exported} level={level} />;
    case "value":
      return <ValueDetails v={x} level={level} />;
    case "patsyn":
      return <PatSynDetails p={x} exported={x.exported} level={level} />;
    case "faminst":
      // Own context (never a mark: labels()/mark() skip "faminst").
      return (
        <TypeDetails
          t={x}
          home={undefined}
          exported={x.exported}
          level={level}
        />
      );
    case "instance":
      return (
        <Show when={x.associated.length > 0}>
          <Title level={level + 1} text="Associated types" />
          <ul class="subdecls">
            <For each={x.associated}>
              {(a) => (
                // Own context: an instance's insides are always
                // exported (D-186), so this never marks.
                <Sub
                  d={a}
                  head={a.head}
                  parentExported={a.exported}
                  level={level + 1}
                >
                  <TypeDetails
                    t={a}
                    home={undefined}
                    exported={a.exported}
                    level={level + 1}
                  />
                </Sub>
              )}
            </For>
          </ul>
        </Show>
      );
    case "splice":
      return null;
  }
}

interface Head {
  code: Tokens;
  // The one it shares its signature with.
  shared: SharedName | undefined;
  // A derived instance: the type it is for.
  derivedFor?: DeclInfo | undefined;
}

function headOf(d: Shown, ix: PageIndex): Head {
  switch (d.kind) {
    case "value":
      return sigHead(d.signature, d.name ?? "", ix, null);
    case "patsyn":
      return sigHead(d.signature, d.name ?? "", ix, "pattern");
    case "instance": {
      if (d.form.kind !== "derived") return { code: d.head, shared: undefined };
      const strategy: Tokens =
        d.form.strategy === "none" || d.form.strategy === "via"
          ? []
          : [
              ["kw", d.form.strategy],
              ["sp", " "],
            ];
      const via = d.form.via;
      const viaCode =
        via === null
          ? undefined
          : Array.isArray(via)
            ? via
            : ix.sentVia(via.sharedWith);
      const on =
        d.form.onType === null ? undefined : ix.byHandle.get(d.form.onType);
      return {
        code: [
          ["kw", "deriving"],
          ["sp", " "],
          ...strategy,
          ...d.head,
          ...(viaCode === undefined
            ? []
            : ([
                ["sp", " "],
                ["kw", "via"],
                ["sp", " "],
                ...viaCode,
              ] as Tokens)),
        ],
        shared: undefined,
        derivedFor: on?.name === null ? undefined : on,
      };
    }
    case "type":
    case "class":
    case "faminst":
    case "splice":
      return { code: d.head, shared: undefined };
  }
}

function DerivedFor(props: { decl: DeclInfo }): JSX.Element {
  const links = useLinks();
  const href = () => links.decl(props.decl.n);
  return (
    <p class="decl-shared">
      Derived for{" "}
      <Show when={href()} fallback={<code>{props.decl.name}</code>}>
        {(h) => (
          <a href={h()}>
            <code>{props.decl.name}</code>
          </a>
        )}
      </Show>
      .
    </p>
  );
}
