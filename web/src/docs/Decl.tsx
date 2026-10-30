// One top-level declaration on the module page: its head or signature,
// badges, warning, docs and "go to source". Every kind's details come
// in T09b.
import { createMemo, createUniqueId, For, type JSX, Show } from "solid-js";
import type { Code as Tokens } from "../api/code";
import type { Declaration, DeclInfo, Handle, WarningText } from "../api/module";
import type { DocComment } from "../api/richtext";
import { Signature } from "./Code";
import { useLinks } from "./links";
import type { PageIndex } from "./pageIndex";
import { DocView } from "./RichText";
import { nameTokens, ownSignature } from "./signature";
import { firstCodeLine } from "./source";

export interface DeclProps {
  decl: Declaration;
  ix: PageIndex;
  // The file view URL at a line, if the module has a source.
  sourceHref: ((line: number) => string) | undefined;
  // Called with the section, for anchors.
  register: (n: Handle, el: HTMLElement) => void;
}

export function Decl(props: DeclProps): JSX.Element {
  const head = createMemo(() => headOf(props.decl, props.ix));
  const headId = createUniqueId();
  const handle = () =>
    props.decl.kind === "splice" ? undefined : props.decl.n;
  const id = () => {
    const d = props.decl;
    return d.kind === "splice" ? undefined : props.ix.idOf(d);
  };
  const href = () => {
    const line = firstCodeLine(props.decl.source);
    return line === undefined ? undefined : props.sourceHref?.(line);
  };
  return (
    <section
      ref={(el) => {
        const n = handle();
        if (n !== undefined) props.register(n, el);
      }}
      class="decl"
      tabindex="-1"
      data-handle={handle()}
      aria-labelledby={headId}
    >
      <h3 id={headId} class="decl-head">
        <Signature code={head().code} />
        <Badges decl={props.decl} />
      </h3>
      <Show when={head().shared.length > 0}>
        <SharedWith names={head().shared} ix={props.ix} />
      </Show>
      <Show when={head().derivedFor}>{(d) => <DerivedFor decl={d()} />}</Show>
      <Warning info={props.decl} ix={props.ix} />
      <Docs info={props.decl} ix={props.ix} />
      <Show when={href()}>
        {(h) => (
          <p class="decl-actions">
            <a href={h()}>
              Go to source
              <Show when={id()}>
                {(i) => <span class="sr-only">: {i()}</span>}
              </Show>
            </a>
          </p>
        )}
      </Show>
    </section>
  );
}

interface Head {
  code: Tokens;
  // Names it shares its signature with.
  shared: string[];
  // A derived instance: the type it is for.
  derivedFor?: DeclInfo | undefined;
}

function headOf(d: Declaration, ix: PageIndex): Head {
  switch (d.kind) {
    case "value":
    case "patsyn": {
      const sig = d.signature;
      const code =
        sig === null
          ? undefined
          : "code" in sig
            ? sig
            : ix.sentSignature(sig.sharedWith);
      if (code === undefined) {
        const name = nameTokens(d.name ?? "");
        return {
          code:
            d.kind === "patsyn"
              ? [["kw", "pattern"], ["sp", " "], ...name]
              : name,
          shared: [],
        };
      }
      if (code.names.length <= 1) return { code: code.code, shared: [] };
      return {
        code: ownSignature(code.code, d.name ?? ""),
        shared: code.names.filter((n) => n !== d.name),
      };
    }
    case "instance": {
      if (d.form.kind !== "derived") return { code: d.head, shared: [] };
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
        shared: [],
        derivedFor: on?.name === null ? undefined : on,
      };
    }
    case "type":
    case "class":
    case "faminst":
    case "splice":
      return { code: d.head, shared: [] };
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

// "Shares its signature with g, h": links to the others.
function SharedWith(props: { names: string[]; ix: PageIndex }): JSX.Element {
  const links = useLinks();
  return (
    <p class="decl-shared">
      Shares its signature with{" "}
      <For each={props.names}>
        {(name, i) => {
          const d = props.ix.byName.value.get(name);
          const href = d === undefined ? undefined : links.decl(d.n);
          return (
            <>
              {i() === 0 ? null : ", "}
              {href === undefined ? (
                <code>{name}</code>
              ) : (
                <a href={href}>
                  <code>{name}</code>
                </a>
              )}
            </>
          );
        }}
      </For>
      .
    </p>
  );
}

// Fixity, and "exported" on exported top-level declarations (not on
// instances: they always are).
function Badges(props: { decl: Declaration }): JSX.Element {
  const exported = () =>
    props.decl.exported &&
    props.decl.kind !== "instance" &&
    props.decl.kind !== "faminst";
  return (
    <Show when={props.decl.fixity !== null || exported()}>
      <span class="decl-badges">
        <Show when={props.decl.fixity}>
          {(f) => (
            <span class="decl-badge">
              {f().direction} {f().precedence}
            </span>
          )}
        </Show>
        <Show when={exported()}>
          <span class="decl-badge">exported</span>
        </Show>
      </span>
    </Show>
  );
}

// A DEPRECATED or WARNING pragma, shared ones too.
export function Warning(props: {
  info: Pick<DeclInfo, "warning">;
  ix: PageIndex;
}): JSX.Element {
  const text = () => {
    const w = props.info.warning;
    return w === null
      ? undefined
      : "sharedWith" in w
        ? props.ix.sentWarning(w)
        : w;
  };
  return <Show when={text()}>{(t) => <WarningNotice warning={t()} />}</Show>;
}

export function WarningNotice(props: { warning: WarningText }): JSX.Element {
  const what = () => {
    const w = props.warning;
    return w.kind === "deprecated"
      ? "Deprecated"
      : w.category === null
        ? "Warning"
        : `Warning (${w.category})`;
  };
  return (
    <div class="docs-notice decl-warning">
      <p>
        <strong>{what()}:</strong> {props.warning.messages.join(" ")}
      </p>
    </div>
  );
}

// The docs of a declaration, shared ones resolved.
function Docs(props: {
  info: Pick<DeclInfo, "docs">;
  ix: PageIndex;
}): JSX.Element {
  const docs = () =>
    props.info.docs
      .map((c): DocComment | undefined =>
        "doc" in c ? c : props.ix.sentDoc(c),
      )
      .filter((c) => c !== undefined);
  return <For each={docs()}>{(c) => <DocView doc={c.doc} level={3} />}</For>;
}
