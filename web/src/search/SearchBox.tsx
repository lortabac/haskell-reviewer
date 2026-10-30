// The search box of "not found" pages. Until global search (T11) it
// filters the module names of the tree, case-insensitive, and links to
// their docs pages. Not a <form>: the CSP has form-action 'none'.
import { useResolvedPath } from "@solidjs/router";
import {
  createMemo,
  createSignal,
  createUniqueId,
  For,
  type JSX,
  Show,
} from "solid-js";
import { docsHref } from "../api/module";
import { withBase } from "../files/paths";
import { useModules } from "../modules/store";

const shown = 50;

export function SearchBox(props: { initialQuery: string }): JSX.Element {
  const modules = useModules();
  const base = useResolvedPath(() => "/");
  const [query, setQuery] = createSignal(props.initialQuery);
  const id = createUniqueId();
  const matches = createMemo(() => {
    const q = query().trim().toLowerCase();
    if (q === "") return [];
    return modules
      .names()
      .filter(
        (m) =>
          m.name.toLowerCase().includes(q) || m.ref.toLowerCase().includes(q),
      );
  });
  const status = () => {
    if (query().trim() === "") return "";
    if (modules.tree.loading) return "Loading modules…";
    const n = matches().length;
    if (n === 0) return "No modules match.";
    const head = n === 1 ? "1 module matches." : `${n} modules match.`;
    return n > shown ? `${head} Showing the first ${shown}.` : head;
  };
  return (
    <div class="search-box" role="search" aria-label="Modules">
      <label for={id}>Search modules by name</label>
      <input
        id={id}
        type="search"
        autocomplete="off"
        spellcheck={false}
        value={query()}
        onInput={(e) => setQuery(e.currentTarget.value)}
      />
      <p class="search-count" role="status">
        {status()}
      </p>
      <Show when={matches().length > 0}>
        <ul class="search-results">
          <For each={matches().slice(0, shown)}>
            {(m) => (
              <li>
                <a href={withBase(base(), docsHref(m.ref))}>{m.name}</a>
                {m.ref === m.name ? null : ` (${m.ref})`}
              </li>
            )}
          </For>
        </ul>
      </Show>
    </div>
  );
}
