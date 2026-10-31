// Values (foreign exports) and pattern synonyms (direction, fields).
import { For, type JSX, Show } from "solid-js";
import type { PatSynFields, ValueFields } from "../../api/module";
import { usePage } from "../context";
import { nameTokens } from "../signature";
import { Sub, Title } from "./common";
import { CodeList } from "./Types";

export function ValueDetails(props: {
  v: ValueFields;
  level: number;
}): JSX.Element {
  return (
    <CodeList
      title="Foreign exports"
      codes={props.v.foreignExports}
      level={props.level}
    />
  );
}

const directions: Record<PatSynFields["direction"], string> = {
  bidirectional: "Bidirectional: matches and builds.",
  unidirectional: "Unidirectional: matches only.",
  explicit: "Bidirectional, with its own builder.",
};

export function PatSynDetails(props: {
  p: PatSynFields;
  exported: boolean;
  level: number;
}): JSX.Element {
  const page = usePage();
  // D-204: exported only hides unexported fields, quietly.
  const fields = () =>
    page.exportedOnly
      ? props.p.fields.filter((f) => f.exported)
      : props.p.fields;
  return (
    <>
      <p class="decl-note">{directions[props.p.direction]}</p>
      <Show when={fields().length > 0}>
        <Title level={props.level + 1} text="Fields" />
        <ul class="subdecls">
          <For each={fields()}>
            {(f) => (
              <Sub
                d={f}
                head={nameTokens(f.name ?? "")}
                parentExported={props.exported}
                level={props.level + 1}
              />
            )}
          </For>
        </ul>
      </Show>
    </>
  );
}
