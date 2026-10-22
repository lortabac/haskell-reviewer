import { useLocation, useResolvedPath } from "@solidjs/router";
import {
  createEffect,
  createMemo,
  createResource,
  createSignal,
  type JSX,
  Match,
  on,
  onCleanup,
  Show,
  Switch,
} from "solid-js";
import { getJson } from "../api/client";
import { decodeFileView, type FileView as View } from "../api/files";
import { bannerText } from "../modules/notices";
import { useModules } from "../modules/store";
import { NotFound } from "../NotFound";
import { PageTitle } from "../PageTitle";
import { apiFileUrl, hrefFor, pathFromPathname, withBase } from "./paths";

// The file view: /<token>/src/tree/<path>.
export function FileView(): JSX.Element {
  const location = useLocation();
  const path = createMemo(() => pathFromPathname(location.pathname));
  return (
    <Show when={path()} keyed fallback={<NotFound />}>
      {(p) => <FilePage path={p} />}
    </Show>
  );
}

function FilePage(props: { path: string }): JSX.Element {
  // Read before the old page goes away.
  const fromMain = focusInMain();
  const [file] = createResource(
    () => props.path,
    (p) => getJson(apiFileUrl(p), decodeFileView),
  );
  const slow = createDelay(() => file.loading, 300);
  // Generated or preprocessed: say why there's only text. Needs the
  // module tree, but never waits for it.
  const modules = useModules();
  const banner = () => {
    const info = modules.bySource().get(props.path);
    return info === undefined ? undefined : bannerText(info.source);
  };
  // Only the result for this path, never a stale one.
  const result = () => (file.state === "ready" ? file() : undefined);
  const notFound = () => result()?.kind === "not-found";
  const view = (): View | undefined => {
    const r = result();
    return r?.kind === "ok" ? r.value : undefined;
  };
  // A link in main (like a symlink target) took us here: its page is
  // gone, so put focus on the new heading instead of letting it drop to
  // body. Again if a 404 swaps the heading.
  if (fromMain) {
    createEffect(
      on(notFound, () => {
        if (focusLost()) {
          document.querySelector<HTMLElement>("#main h1")?.focus();
        }
      }),
    );
  }
  return (
    <Show when={!notFound()} fallback={<NotFound />}>
      <PageTitle text={props.path} />
      <h1 class="file-path" tabindex="-1">
        {props.path}
      </h1>
      <Show when={banner()}>{(b) => <p class="file-banner">{b()}</p>}</Show>
      <Switch
        fallback={
          <Show when={slow()}>
            <p>Loading…</p>
          </Show>
        }
      >
        <Match when={view()} keyed>
          {(v) => body(v)}
        </Match>
        <Match when={result()?.kind === "error"}>
          <p>Couldn't load this file.</p>
        </Match>
      </Switch>
    </Show>
  );
}

function focusInMain(): boolean {
  const el = document.activeElement;
  return el !== null && el.id !== "main" && el.closest("#main") !== null;
}

function focusLost(): boolean {
  const el = document.activeElement;
  return el === null || el === document.body || !el.isConnected;
}

function body(v: View): JSX.Element {
  switch (v.kind) {
    case "text":
      return <FileText text={v.text} />;
    case "binary":
      return (
        <p>This file is not UTF-8 text, so it isn't shown ({bytes(v.size)}).</p>
      );
    case "too-large":
      return (
        <p>
          This file is too large to show ({bytes(v.size)}; the limit is{" "}
          {limit(v.limit)}).
        </p>
      );
    case "symlink":
      return <Symlink target={v.target} resolved={v.resolved} />;
    case "submodule":
      return (
        <p>
          {v.commit === null
            ? "Nested git repository."
            : `Submodule at commit ${v.commit}.`}{" "}
          Its files aren't shown.
        </p>
      );
  }
}

function Symlink(props: {
  target: string;
  resolved: string | null;
}): JSX.Element {
  const base = useResolvedPath(() => "/");
  return (
    <p>
      Symbolic link to{" "}
      <Show when={props.resolved} fallback={<code>{props.target}</code>}>
        {(r) => (
          <a href={withBase(base(), hrefFor(r()))}>
            <code>{props.target}</code>
          </a>
        )}
      </Show>
    </p>
  );
}

// Line numbers and code as two text nodes, no element per line. Big
// files are fine like this until the views are virtualised (M2).
function FileText(props: { text: string }): JSX.Element {
  const numbers = () => {
    const n = lineCount(props.text);
    return Array.from({ length: n }, (_, i) => String(i + 1)).join("\n");
  };
  return (
    <div class="file-text">
      <pre class="file-gutter" aria-hidden="true">
        {numbers()}
      </pre>
      <pre class="file-code" tabindex="0" aria-label="File contents">
        {props.text}
      </pre>
    </div>
  );
}

// Lines as the <pre> shows them: split on "\n", and a final "\n" ends
// the last line instead of starting a new one. CRLF is one line. Other
// breaks (lone \r, U+2028/2029, NEL, VT, FF) may make the gutter drift;
// M2 picks the final line rule, with #L<n>.
export function lineCount(text: string): number {
  if (text === "") return 0;
  let n = 1;
  for (let i = text.indexOf("\n"); i !== -1; i = text.indexOf("\n", i + 1)) {
    n++;
  }
  return text.endsWith("\n") ? n - 1 : n;
}

function bytes(n: number): string {
  return n === 1 ? "1 byte" : `${n.toLocaleString("en-US")} bytes`;
}

function limit(n: number): string {
  const mib = 1024 * 1024;
  return n > 0 && n % mib === 0 ? `${n / mib} MiB` : bytes(n);
}

// True once loading() has been true for ms milliseconds, so fast loads
// don't flash a loading text.
function createDelay(loading: () => boolean, ms: number): () => boolean {
  const [slow, setSlow] = createSignal(false);
  createEffect(() => {
    setSlow(false);
    if (!loading()) return;
    const t = setTimeout(() => setSlow(true), ms);
    onCleanup(() => clearTimeout(t));
  });
  return slow;
}
