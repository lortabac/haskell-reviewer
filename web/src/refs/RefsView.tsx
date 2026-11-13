// The references page: /<token>/refs/tree/<module>?decl=<id> (T12).
// Results in groups by module, a snippet each, a link to the line.
// While the answer is partial it is asked again (D-286); the list
// keeps its rows (keys), so focus stays where it is.
import { A, useLocation } from "@solidjs/router";
import {
  createEffect,
  createMemo,
  createSignal,
  createUniqueId,
  For,
  type JSX,
  Match,
  on,
  onCleanup,
  Show,
  Switch,
  useContext,
} from "solid-js";
import { createStore, reconcile } from "solid-js/store";
import { getJson } from "../api/client";
import { docsHref } from "../api/module";
import { decodeRefs, type Refs, refsUrl } from "../api/refs";
import { anchorName } from "../docs/urls";
import { createRefetchLoop } from "../events/refetchLoop";
import { IndexingContext } from "../events/indexing";
import { hrefFor, lineHref } from "../files/paths";
import { useModules } from "../modules/store";
import { NotFound } from "../NotFound";
import { PageTitle } from "../PageTitle";
import { plainPieces } from "../source/display";
import { reasonText } from "../source/hover";
import { createDelay, focusInMain, focusLost } from "../ui/focus";
import {
  guessCount,
  type Group,
  groupRefs,
  type Item,
  summary,
} from "./groups";
import { snippetParts } from "./snippet";
import { refsFromLocation } from "./urls";

// Complete answers of the last 4 pages, for Back (same snapshot only).
const cacheSize = 4;
const cache = new Map<string, Refs>();

export function clearRefsCache(): void {
  cache.clear();
}

// The history entry of a page says which row was followed from it
// (history.state.refsFollowed), so Back focuses that row again.
function followedKey(): string | undefined {
  const st: unknown = window.history.state;
  if (typeof st !== "object" || st === null) return undefined;
  const k = (st as { refsFollowed?: unknown }).refsFollowed;
  return typeof k === "string" ? k : undefined;
}

export function RefsView(): JSX.Element {
  const location = useLocation();
  const req = createMemo(
    () => refsFromLocation(location.pathname, location.search),
    undefined,
    { equals: (a, b) => a?.module === b?.module && a?.decl === b?.decl },
  );
  return (
    <Show when={req()} keyed fallback={<NotFound />}>
      {(r) => <RefsPage module={r.module} decl={r.decl} />}
    </Show>
  );
}

// The kind of declaration, from its id's first word.
const declWords: Record<string, string> = {
  type: "type",
  class: "class",
  val: "value",
  con: "constructor",
  pat: "pattern",
  field: "field",
};

type State = "loading" | "not-found" | "error" | "ok";

function RefsPage(props: { module: string; decl: string }): JSX.Element {
  // Read before the old page goes away.
  const fromMain = focusInMain();
  /* eslint-disable solid/reactivity -- the page is made for these */
  const { module, decl } = props;
  /* eslint-enable solid/reactivity */
  const pageKey = `${module}\0${decl}`;
  // Set when we come back to this entry (Back, Forward, reload).
  const followed = followedKey();
  const indexing = useContext(IndexingContext);
  const name = anchorName(decl);
  // The module's name from the tree, when it is loaded (stable from the
  // first render); else from the answer, else the ref.
  const modules = useModules();
  const treeName = () => modules.names().find((n) => n.ref === module)?.name;
  const word = declWords[/^([a-z]+):/.exec(decl)?.[1] ?? ""];

  const [state, setState] = createSignal<State>("loading");
  const [meta, setMeta] = createSignal<
    Pick<Refs, "module" | "total" | "partial" | "generation"> & {
      summary: string;
      guesses: string;
      // The module's name, as the groups show it.
      home: string;
    }
  >();
  const [groups, setGroups] = createStore<{ list: Group[] }>({ list: [] });
  const [said, setSaid] = createSignal("");
  let h1: HTMLHeadingElement | undefined;
  let list: HTMLDivElement | undefined;

  const show = (r: Refs) => {
    const had = list !== undefined && list.contains(document.activeElement);
    setGroups("list", reconcile(groupRefs(r.results), { key: "key" }));
    setMeta({
      module: r.module,
      total: r.total,
      partial: r.partial,
      generation: r.generation,
      summary: summary(r),
      guesses: guessCount(r.results, r.results.length < r.total),
      home: r.results.find((x) => x.ref === r.module)?.module ?? module,
    });
    setState("ok");
    if (had && focusLost()) h1?.focus();
  };

  // A cached answer, if it is for the snapshot we are in.
  const cached = cache.get(pageKey);
  const gen = indexing.indexing()?.generation;
  const useCached = cached !== undefined && cached.generation === gen;
  if (useCached) {
    cache.delete(pageKey);
    cache.set(pageKey, cached);
    show(cached);
    setSaid(summary(cached));
  }
  const fetchNow = createRefetchLoop(
    (signal) => getJson(refsUrl(module, decl), decodeRefs, { signal }),
    (r) => {
      if (r.kind === "not-found") {
        if (state() !== "ok") setState("not-found");
        return undefined;
      }
      if (r.kind === "error") {
        if (state() !== "ok") setState("error");
        return undefined;
      }
      const refs = r.value;
      show(refs);
      if (!refs.partial) {
        // Once, when it is complete.
        setSaid(summary(refs));
        cache.delete(pageKey);
        cache.set(pageKey, refs);
        if (cache.size > cacheSize) {
          cache.delete(cache.keys().next().value as string);
        }
      }
      return { generation: refs.generation, partial: refs.partial };
    },
    useCached ? { generation: cached.generation, partial: false } : undefined,
  );
  if (!useCached) fetchNow();

  const slow = createDelay(() => state() === "loading", 300);
  // Focus the heading when a link in main brought us here.
  // On a 404 the heading is a new one (NotFound).
  if (fromMain) {
    createEffect(
      on(state, () => {
        if (focusLost()) {
          document.querySelector<HTMLElement>("#main h1")?.focus();
        }
      }),
    );
  }
  // Back from a followed item: focus it again.
  createEffect(
    on(
      () => state() === "ok",
      (ok) => {
        if (!ok || followed === undefined) return;
        const frame = requestAnimationFrame(() => {
          const a = [
            ...document.querySelectorAll<HTMLElement>("[data-ref-key]"),
          ].find((e) => e.dataset.refKey === followed);
          a?.focus();
          a?.scrollIntoView({ block: "center" });
        });
        onCleanup(() => cancelAnimationFrame(frame));
      },
    ),
  );

  return (
    <Switch>
      <Match when={state() === "not-found"}>
        <NotFound note="No such declaration in this snapshot." query={module} />
      </Match>
      <Match when={true}>
        <PageTitle text={`References to ${name}`} />
        <h1 ref={(el) => (h1 = el)} class="refs-title" tabindex="-1">
          References to <code>{name}</code>
          <Show when={word}> ({word})</Show> in{" "}
          <A href={docsHref(module, decl)}>
            {treeName() ?? meta()?.home ?? module}
          </A>
        </h1>
        <Switch>
          <Match when={state() === "error"}>
            <p>Couldn't load the references.</p>
          </Match>
          <Match when={meta()} keyed>
            {(m) => (
              <>
                <p class="refs-summary">
                  {m.summary}
                  <Show when={m.guesses}> {m.guesses}</Show>
                </p>
                <p class="refs-tier">
                  Syntactic: found by name and imports, not by the type checker.
                </p>
              </>
            )}
          </Match>
          <Match when={slow()}>
            <p>Loading…</p>
          </Match>
        </Switch>
        <Show when={meta()?.partial}>
          <p class="refs-partial">
            <span class="refs-badge">partial</span> Still indexing: more
            references may come.
          </p>
        </Show>
        <p class="sr-only" role="status">
          {said()}
        </p>
        <div ref={(el) => (list = el)} class="refs-list">
          <For each={groups.list}>
            {(g) => <GroupView g={g} home={module} />}
          </For>
        </div>
      </Match>
    </Switch>
  );
}

function GroupView(props: { g: Group; home: string }): JSX.Element {
  const id = createUniqueId();
  return (
    <section class="refs-group" aria-labelledby={id}>
      <h2 id={id} class="refs-group-head">
        <A href={docsHref(props.g.ref)}>{props.g.module}</A>
        <Show when={props.g.ref === props.home}>
          <span class="refs-home"> (declared here)</span>
        </Show>
      </h2>
      <p class="refs-group-meta">
        <Show when={props.g.path}>
          {(p) => <A href={hrefFor(p())}>{p()}</A>}
        </Show>{" "}
        <span class="refs-count">
          {props.g.items.length}{" "}
          {props.g.items.length === 1 ? "reference" : "references"}
        </span>
      </p>
      <ol class="refs-items">
        <For each={props.g.items}>
          {(r) => <ItemView r={r} path={props.g.path} />}
        </For>
      </ol>
    </section>
  );
}

function Snippet(props: { r: Item }): JSX.Element {
  const s = createMemo(() => snippetParts(props.r));
  const pieces = (t: string) => (
    <For each={plainPieces(t)}>
      {(p) => <span class={p.ctrl ? "code-ctrl" : undefined}>{p.text}</span>}
    </For>
  );
  return (
    <code class="refs-snippet">
      <Show when={s().cut}>…</Show>
      {pieces(s().before)}
      <mark class="ref-hit">{pieces(s().hit)}</mark>
      {pieces(s().after)}
    </code>
  );
}

function ItemView(props: { r: Item; path: string | null }): JSX.Element {
  const label = () => (
    <>
      <span class="refs-line">
        <span class="sr-only">Line </span>
        {props.r.span[0]}
      </span>{" "}
      <Snippet r={props.r} />
    </>
  );
  return (
    <li class="refs-item">
      <Show when={props.path} fallback={label()}>
        {(p) => (
          <A
            href={lineHref(p(), props.r.span[0])}
            state={{ codeJump: true, cp: props.r.span[1] }}
            data-ref-key={props.r.key}
            on:click={(e: MouseEvent) => {
              if (e.button !== 0 || e.ctrlKey || e.metaKey) return;
              if (e.shiftKey || e.altKey) return;
              // Native: before the router's click handler, which
              // pushes the next entry.
              window.history.replaceState(
                {
                  ...(window.history.state as Record<string, unknown> | null),
                  refsFollowed: props.r.key,
                },
                "",
              );
            }}
          >
            {label()}
          </A>
        )}
      </Show>{" "}
      <span class="refs-badge">{props.r.kind}</span>
      <Show when={props.r.heuristic}>
        {" "}
        <span class="refs-badge refs-guess">guess</span>
      </Show>
      <Show when={props.r.heuristic}>
        <p class="refs-why">
          Resolved heuristically:{" "}
          {[...new Set(props.r.reasons)].map((c) => reasonText(c)).join(" ")}
        </p>
      </Show>
    </li>
  );
}
