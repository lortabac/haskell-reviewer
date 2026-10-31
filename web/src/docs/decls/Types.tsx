// Types and family instances: kind signature, roles, constructors
// with their arguments and fields, deriving clauses, equations,
// defaults, instances.
import { For, type JSX, Show } from "solid-js";
import type { Code as Tokens } from "../../api/code";
import type {
  ConArg,
  Constructor,
  Deriving,
  FieldDecl,
  Handle,
  TypeFields,
} from "../../api/module";
import { Code, Signature } from "../Code";
import { usePage } from "../context";
import { InstancesList } from "../Instances";
import { DocView } from "../RichText";
import { nameTokens, ownSignature } from "../signature";
import { marks, type SharedName, Sub, Title } from "./common";

// What a type, an associated type or a family instance shows under
// its head. home: the handle that lists its instances.
export function TypeDetails(props: {
  t: Partial<Omit<TypeFields, "form">> &
    Pick<TypeFields, "constructors" | "derivings">;
  home: Handle | undefined;
  exported: boolean;
  level: number;
}): JSX.Element {
  const page = usePage();
  // D-204: exported only hides unexported constructors; an abstract
  // type (none left) gets a muted count instead of an empty list.
  const cons = () =>
    page.exportedOnly
      ? props.t.constructors.filter((c) => c.exported)
      : props.t.constructors;
  return (
    <>
      <Show when={props.t.kindSig}>
        {(k) => (
          <p class="decl-line">
            <Signature code={k()} />
          </p>
        )}
      </Show>
      <Show when={props.t.roles}>
        {(r) => (
          <p class="decl-line">
            <Code code={r()} />
          </p>
        )}
      </Show>
      <Constructors
        cons={cons()}
        total={props.t.constructors.length}
        exported={props.exported}
        level={props.level}
      />
      <Derivings derivings={props.t.derivings} level={props.level} />
      <CodeList
        title="Equations"
        codes={props.t.equations ?? []}
        level={props.level}
      />
      <CodeList
        title="Defaults"
        codes={props.t.defaults ?? []}
        level={props.level}
      />
      {/* Not <Show when={home}>: handle 0 is a home too. */}
      <Show when={props.home !== undefined}>
        <InstancesList
          home={props.home ?? 0}
          list={props.t.instances ?? []}
          level={props.level}
        />
      </Show>
    </>
  );
}

// A titled list of code lines, if any.
export function CodeList(props: {
  title: string;
  codes: Tokens[];
  level: number;
}): JSX.Element {
  return (
    <Show when={props.codes.length > 0}>
      <Title level={props.level + 1} text={props.title} />
      <ul class="decl-lines">
        <For each={props.codes}>
          {(c) => (
            <li>
              <Signature code={c} />
            </li>
          )}
        </For>
      </ul>
    </Show>
  );
}

function Constructors(props: {
  cons: Constructor[];
  // Before filtering: 0 if the declaration truly has none.
  total: number;
  exported: boolean;
  level: number;
}): JSX.Element {
  return (
    <Show
      when={props.cons.length > 0}
      fallback={
        <Show when={props.total > 0}>
          <p class="decl-note">Private constructors ({props.total}).</p>
        </Show>
      }
    >
      <Title level={props.level + 1} text="Constructors" />
      <ul class="subdecls">
        <For each={props.cons}>
          {(c) => (
            <ConstructorItem
              c={c}
              exported={props.exported}
              level={props.level + 1}
            />
          )}
        </For>
      </ul>
    </Show>
  );
}

function ConstructorItem(props: {
  c: Constructor;
  exported: boolean;
  level: number;
}): JSX.Element {
  const page = usePage();
  // On a GADT line A, B :: T, the first sends the code (and shows it
  // whole, D-187); the others show their own signature and link to it.
  const sender = (): Constructor | undefined => {
    const s =
      props.c.sharedWith === null
        ? undefined
        : page.ix.byHandle.get(props.c.sharedWith);
    return s !== undefined && "shape" in s ? (s as Constructor) : undefined;
  };
  const name = () => props.c.name ?? "";
  const head = (): Tokens => {
    const s = sender();
    const code = (s ?? props.c).code;
    if (code === null || props.c.shape === "record") return nameTokens(name());
    return s === undefined ? code : ownSignature(code, name());
  };
  const shared = (): SharedName | undefined => {
    const s = sender();
    return s === undefined ? undefined : { name: s.name ?? "", n: s.n };
  };
  // A constructor sharing its line has no args or fields of its own:
  // they show under the first one (no copies).
  const hasArgDocs = () => props.c.args.some((a) => a.doc !== null);
  // D-204: exported only hides unexported fields, quietly (no abstract
  // notice: a record missing a few fields isn't an "abstract type").
  const fields = () =>
    page.exportedOnly
      ? props.c.fields.filter((f) => f.exported)
      : props.c.fields;
  return (
    <Sub
      d={props.c}
      head={head()}
      parentExported={props.exported}
      level={props.level}
      shared={shared()}
    >
      <Show when={hasArgDocs()}>
        <ul class="decl-args">
          <For each={props.c.args}>
            {(a) => <ArgItem a={a} level={props.level} />}
          </For>
        </ul>
      </Show>
      <Show when={fields().length > 0}>
        <ul class="subdecls">
          <For each={fields()}>
            {(f) => (
              <FieldItem
                f={f}
                exported={props.exported}
                level={props.level + 1}
              />
            )}
          </For>
        </ul>
      </Show>
    </Sub>
  );
}

function ArgItem(props: { a: ConArg; level: number }): JSX.Element {
  return (
    <li>
      <Code code={[...marks(props.a), ...props.a.type]} />
      <Show when={props.a.doc}>
        {(d) => <DocView doc={d().doc} level={props.level} />}
      </Show>
    </li>
  );
}

function FieldItem(props: {
  f: FieldDecl;
  exported: boolean;
  level: number;
}): JSX.Element {
  const page = usePage();
  const head = (): Tokens => {
    const t = props.f.type;
    const type = Array.isArray(t)
      ? t
      : (page.ix.sentFieldType(t.sharedWith) ?? []);
    return [
      ...nameTokens(props.f.name ?? ""),
      ["sp", " "],
      ["pun", "::"],
      ["sp", " "],
      ...marks(props.f),
      ...type,
    ];
  };
  return (
    <Sub
      d={props.f}
      head={head()}
      parentExported={props.exported}
      level={props.level}
    />
  );
}

function Derivings(props: {
  derivings: Deriving[];
  level: number;
}): JSX.Element {
  const page = usePage();
  const line = (d: Deriving): Tokens => {
    const out: Tokens = [
      ["kw", "deriving"],
      ["sp", " "],
    ];
    if (d.strategy !== "none" && d.strategy !== "via") {
      out.push(["kw", d.strategy], ["sp", " "]);
    }
    const many = d.classes.length !== 1;
    if (many) out.push(["pun", "("]);
    d.classes.forEach((c, i) => {
      if (i > 0) out.push(["pun", ","], ["sp", " "]);
      out.push(...c.code);
    });
    if (many) out.push(["pun", ")"]);
    const via =
      d.via === null
        ? undefined
        : Array.isArray(d.via)
          ? d.via
          : page.ix.sentVia(d.via.sharedWith);
    if (via !== undefined)
      out.push(["sp", " "], ["kw", "via"], ["sp", " "], ...via);
    return out;
  };
  return (
    <CodeList
      title="Deriving"
      codes={props.derivings.map(line)}
      level={props.level}
    />
  );
}
