// Bits every declaration kind uses: badges, warnings, docs, "shares
// its signature with", nested items and their titles.
import { For, type JSX, Show } from "solid-js";
import { Dynamic } from "solid-js/web";
import type { Code as Tokens } from "../../api/code";
import type {
  ArgMarks,
  DeclInfo,
  Fixity,
  Handle,
  Signature as Sig,
  WarningText,
} from "../../api/module";
import type { DocComment } from "../../api/richtext";
import { Signature } from "../Code";
import { usePage, useAnchor } from "../context";
import { useLinks } from "../links";
import type { PageIndex } from "../pageIndex";
import { DocView } from "../RichText";
import { nameTokens, ownSignature } from "../signature";

// A heading tag, h6 at most.
export function headingTag(level: number): string {
  return `h${Math.max(1, Math.min(6, level))}`;
}

// The title of a list in a declaration (Constructors, Methods...).
export function Title(props: { level: number; text: string }): JSX.Element {
  return (
    <Dynamic component={headingTag(props.level)} class="decl-sub-title">
      {props.text}
    </Dynamic>
  );
}

// Fixity and labels (exported, not exported, orphan...).
export function Badges(props: {
  fixity: Fixity | null;
  labels: string[];
}): JSX.Element {
  return (
    <Show when={props.fixity !== null || props.labels.length > 0}>
      <span class="decl-badges">
        <Show when={props.fixity}>
          {(f) => (
            <span class="decl-badge">
              {f().direction} {f().precedence}
            </span>
          )}
        </Show>
        <For each={props.labels}>
          {(l) => <span class="decl-badge">{l}</span>}
        </For>
      </span>
    </Show>
  );
}

// The declaration that sends a shared signature (the first name): its
// name and handle.
export interface SharedName {
  name: string;
  n: Handle;
}

// "Shares its signature with f": a link to the one that shows it
// whole. One link per name, so N names give N links, not N² (D-187).
export function SharedWith(props: {
  sender: SharedName | undefined;
}): JSX.Element {
  const links = useLinks();
  return (
    <Show when={props.sender}>
      {(s) => (
        <p class="decl-shared">
          Shares its signature with{" "}
          <Show when={links.decl(s().n)} fallback={<code>{s().name}</code>}>
            {(h) => (
              <a href={h()}>
                <code>{s().name}</code>
              </a>
            )}
          </Show>
          .
        </p>
      )}
    </Show>
  );
}

// The head of a value, method or pattern synonym (D-187): the
// signature as written for the one that sends it (all names), its own
// signature and the sender for the others, else its name.
export function sigHead(
  sig: Sig | null,
  name: string,
  ix: PageIndex,
  keyword: "pattern" | null,
): { code: Tokens; shared: SharedName | undefined } {
  if (sig !== null && "code" in sig) {
    return { code: sig.code, shared: undefined };
  }
  const code = sig === null ? undefined : ix.sentSignature(sig.sharedWith);
  const sender = sig === null ? undefined : ix.byHandle.get(sig.sharedWith);
  if (code === undefined || sender === undefined) {
    const n = nameTokens(name);
    return {
      code: keyword === null ? n : [["kw", keyword], ["sp", " "], ...n],
      shared: undefined,
    };
  }
  return {
    code: ownSignature(code.code, name),
    shared: { name: sender.name ?? "", n: sender.n },
  };
}

// ! or ~, and {-# UNPACK #-}, before a type.
export function marks(m: ArgMarks): Tokens {
  const out: Tokens = [];
  if (m.unpack !== "none") {
    out.push([
      "prag",
      m.unpack === "unpack" ? "{-# UNPACK #-}" : "{-# NOUNPACK #-}",
    ]);
    out.push(["sp", " "]);
  }
  if (m.strictness === "strict") out.push(["rop", "!"]);
  if (m.strictness === "lazy") out.push(["rop", "~"]);
  return out;
}

// A DEPRECATED or WARNING pragma, shared ones too.
// A warning shared by several names shows whole only under the first
// (the sender); the others link to it (D-187).
export function Warning(props: {
  info: Pick<DeclInfo, "warning">;
}): JSX.Element {
  const w = () => props.info.warning;
  return (
    <Show when={w()}>
      {(x) => {
        const v = x();
        return "sharedWith" in v ? (
          <SameAs what="warning" sender={v.sharedWith} />
        ) : (
          <WarningNotice warning={v} />
        );
      }}
    </Show>
  );
}

// "Same docs as f.": a link to the declaration that shows them.
function SameAs(props: { what: string; sender: Handle }): JSX.Element {
  const page = usePage();
  const links = useLinks();
  const name = () => {
    const d = page.ix.byHandle.get(props.sender);
    return d === undefined ? undefined : (d.name ?? page.ix.idOf(d));
  };
  return (
    <Show when={name()}>
      {(n) => (
        <p class="decl-shared">
          Same {props.what} as{" "}
          <Show when={links.decl(props.sender)} fallback={<code>{n()}</code>}>
            {(h) => (
              <a href={h()}>
                <code>{n()}</code>
              </a>
            )}
          </Show>
          .
        </p>
      )}
    </Show>
  );
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

// The docs of a declaration. level: the heading level they sit
// under. A doc shared by several names shows whole only under the
// first (the sender); the others link to it, once per sender, so a big
// doc on N names stays linear (D-187).
export function Docs(props: {
  info: Pick<DeclInfo, "docs">;
  level: number;
}): JSX.Element {
  const items = () => {
    const out: (DocComment | { sender: Handle })[] = [];
    let last: Handle | undefined;
    for (const c of props.info.docs) {
      if ("doc" in c) {
        out.push(c);
        last = undefined;
      } else if (c.sharedWith !== last) {
        out.push({ sender: c.sharedWith });
        last = c.sharedWith;
      }
    }
    return out;
  };
  return (
    <For each={items()}>
      {(c) =>
        "doc" in c ? (
          <DocView doc={c.doc} level={props.level} />
        ) : (
          <SameAs what="docs" sender={c.sender} />
        )
      }
    </For>
  );
}

// A nested declaration (constructor, field, method, associated type),
// as a list item with its own anchor. "not exported" only when its
// parent is (D-186).
export function Sub(props: {
  d: DeclInfo;
  head: Tokens;
  parentExported: boolean;
  // The heading level it sits under.
  level: number;
  labels?: string[];
  shared?: SharedName | undefined;
  children?: JSX.Element;
}): JSX.Element {
  // eslint-disable-next-line solid/reactivity -- a handle never changes
  const anchor = useAnchor(props.d.n);
  const labels = () => [
    ...(props.parentExported && !props.d.exported ? ["not exported"] : []),
    ...(props.labels ?? []),
  ];
  return (
    <li class="subdecl" ref={anchor} tabindex="-1" data-handle={props.d.n}>
      <div class="subdecl-head">
        <Signature code={props.head} />
        <Badges fixity={props.d.fixity} labels={labels()} />
      </div>
      <SharedWith sender={props.shared} />
      <Warning info={props.d} />
      <Docs info={props.d} level={props.level} />
      {props.children}
    </li>
  );
}
