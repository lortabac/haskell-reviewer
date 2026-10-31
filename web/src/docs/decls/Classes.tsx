// Classes: kind signature, roles, associated types, methods with
// their default signatures, MINIMAL, instances.
import { For, type JSX, Show } from "solid-js";
import type {
  AssociatedType,
  ClassFields,
  Handle,
  Method,
} from "../../api/module";
import { Code, Signature } from "../Code";
import { usePage } from "../context";
import { InstancesList } from "../Instances";
import { sigHead, Sub, Title } from "./common";
import { TypeDetails } from "./Types";

export function ClassDetails(props: {
  c: ClassFields;
  n: Handle;
  exported: boolean;
  level: number;
}): JSX.Element {
  const page = usePage();
  // D-204: exported only hides unexported associated types and
  // methods, quietly (no abstract-type-style notice for classes).
  const associated = () =>
    page.exportedOnly
      ? props.c.associated.filter((a) => a.exported)
      : props.c.associated;
  const methods = () =>
    page.exportedOnly
      ? props.c.methods.filter((m) => m.exported)
      : props.c.methods;
  return (
    <>
      <Show when={props.c.kindSig}>
        {(k) => (
          <p class="decl-line">
            <Signature code={k()} />
          </p>
        )}
      </Show>
      <Show when={props.c.roles}>
        {(r) => (
          <p class="decl-line">
            <Code code={r()} />
          </p>
        )}
      </Show>
      <Show when={associated().length > 0}>
        <Title level={props.level + 1} text="Associated types" />
        <ul class="subdecls">
          <For each={associated()}>
            {(a) => (
              <AssociatedItem
                a={a}
                exported={props.exported}
                level={props.level + 1}
              />
            )}
          </For>
        </ul>
      </Show>
      <Show when={methods().length > 0}>
        <Title level={props.level + 1} text="Methods" />
        <ul class="subdecls">
          <For each={methods()}>
            {(m) => (
              <MethodItem
                m={m}
                exported={props.exported}
                level={props.level + 1}
              />
            )}
          </For>
        </ul>
      </Show>
      <Show when={props.c.minimal}>
        {(m) => (
          <p class="decl-line">
            <Code code={m()} />
          </p>
        )}
      </Show>
      <InstancesList
        home={props.n}
        list={props.c.instances}
        level={props.level}
      />
    </>
  );
}

function AssociatedItem(props: {
  a: AssociatedType;
  exported: boolean;
  level: number;
}): JSX.Element {
  return (
    <Sub
      d={props.a}
      head={props.a.head}
      parentExported={props.exported}
      level={props.level}
    >
      <TypeDetails
        t={props.a}
        home={props.a.n}
        exported={props.exported}
        level={props.level}
      />
    </Sub>
  );
}

function MethodItem(props: {
  m: Method;
  exported: boolean;
  level: number;
}): JSX.Element {
  const page = usePage();
  // Once: a method never changes.
  // eslint-disable-next-line solid/reactivity
  const head = sigHead(props.m.signature, props.m.name ?? "", page.ix, null);
  return (
    <Sub
      d={props.m}
      head={head.code}
      shared={head.shared}
      parentExported={props.exported}
      level={props.level}
    >
      <Show when={props.m.defaultSig}>
        {(s) => (
          <p class="decl-line">
            <Signature code={s()} />
          </p>
        )}
      </Show>
    </Sub>
  );
}
