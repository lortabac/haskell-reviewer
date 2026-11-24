// A Markdown file, rendered: /<token>/md/<side>/<path>[#<anchor>] (SPEC
// §4.2, D-328). The docs view of a Markdown file; Source is its file
// view.
import { useLocation, useResolvedPath } from "@solidjs/router";
import {
  createEffect,
  createMemo,
  createResource,
  type JSX,
  Match,
  on,
  onCleanup,
  onMount,
  Show,
  Switch,
} from "solid-js";
import { getCbor } from "../api/client";
import { decodeMarkdownPage, type MarkdownPage } from "../api/markdown";
import type { Doc } from "../api/richtext";
import { CopyButton } from "../copy/CopyButton";
import { useDiffLink } from "../diff/link";
import { useDiffFiles } from "../diff/store";
import { markdownTarget, pathTarget } from "../files/otherSide";
import { type Links, LinksContext } from "../docs/links";
import { anchorId, DocView } from "../docs/RichText";
import {
  apiMarkdownUrl,
  hrefFor,
  isMarkdownPath,
  mdHref,
  mdPathFromPathname,
  withBase,
} from "../files/paths";
import {
  useCopyLocation,
  useSideTarget,
  useViewLinks,
} from "../keys/KeysProvider";
import { NotFound } from "../NotFound";
import { PageTitle } from "../PageTitle";
import { usePageSide } from "../session";
import { createDelay, focusInMain, focusLost } from "../ui/focus";
import { noteTexts } from "../ui/notes";

export function MarkdownView(): JSX.Element {
  const side = usePageSide();
  const location = useLocation();
  const path = createMemo(() => mdPathFromPathname(location.pathname, side()));
  return (
    <Show
      when={(() => {
        const p = path();
        return p !== undefined && isMarkdownPath(p) ? p : undefined;
      })()}
      keyed
      fallback={<NotFound />}
    >
      {(p) => <MarkdownPageView path={p} />}
    </Show>
  );
}

function MarkdownPageView(props: { path: string }): JSX.Element {
  const side = usePageSide();
  // Read before the old page goes away.
  const fromMain = focusInMain();
  const base = useResolvedPath(() => "/");
  const [res] = createResource(
    () => props.path,
    (p) => getCbor(apiMarkdownUrl(side(), p), decodeMarkdownPage),
  );
  const slow = createDelay(() => res.loading, 300);
  // Only the result for this path, never a stale one.
  const result = () => (res.state === "ready" ? res() : undefined);
  const page = (): MarkdownPage | undefined => {
    const r = result();
    return r?.kind === "ok" ? r.value : undefined;
  };
  const diffLink = useDiffLink();
  useViewLinks({
    current: "docs",
    docs: () => mdHref(side(), props.path),
    source: () => hrefFor(side(), props.path),
    diff: () => diffLink(side(), props.path),
  });
  // b: the same file on the other side, the hash kept (D-349).
  const diffFiles = useDiffFiles();
  const location = useLocation();
  useSideTarget(() => {
    const s = side();
    if (s === "tree") return undefined;
    return markdownTarget(
      pathTarget(diffFiles, s, props.path),
      s,
      () => location.hash,
    );
  });
  // No lines here: the path.
  useCopyLocation(() => props.path);
  if (fromMain) {
    createEffect(
      on(result, () => {
        if (focusLost()) {
          document.querySelector<HTMLElement>("#main h1")?.focus();
        }
      }),
    );
  }
  return (
    <Show when={result()?.kind !== "not-found"} fallback={<NotFound />}>
      <PageTitle text={props.path} />
      <div class="copy-host">
        <CopyButton text={() => props.path} label="Copy path" />
        <h1 class="file-path" tabindex="-1">
          {props.path}
        </h1>
      </div>
      {/* Read from disk, and git would filter it (D-343). */}
      <Show when={page()?.filtered === true}>
        <p class="file-note">{noteTexts.filtered}</p>
      </Show>
      <Switch
        fallback={
          <Show when={slow()}>
            <p>Loading…</p>
          </Show>
        }
      >
        <Match when={page()} keyed>
          {(p) =>
            p.kind === "doc" ? (
              <Rendered path={props.path} doc={p.doc} />
            ) : (
              <p>
                {p.kind === "not-text"
                  ? "This file is not text, so it can't be shown rendered."
                  : p.kind === "not-local"
                    ? noteTexts.notLocal
                    : "This file took too long to render."}{" "}
                <a href={withBase(base(), hrefFor(side(), props.path))}>
                  See the file
                </a>
                .
              </p>
            )
          }
        </Match>
        <Match when={result()?.kind === "error"}>
          <p>Couldn't load this file.</p>
        </Match>
      </Switch>
    </Show>
  );
}

// The doc, and #<anchor> in the URL: that heading, scrolled to the top
// and focused. Again on a hash change, and on a click on a link to the
// anchor already in the URL (the router ignores those).
function Rendered(props: { path: string; doc: Doc }): JSX.Element {
  const side = usePageSide();
  const location = useLocation();
  const base = useResolvedPath(() => "/");
  const links: Links = {
    // No names or modules in Markdown.
    decl: () => undefined,
    module: () => "",
    outer: () => undefined,
    live: false,
    token: (t) => t,
    name: (n) => n,
    file: (p, fragment) =>
      withBase(
        base(),
        isMarkdownPath(p)
          ? mdHref(side(), p, fragment)
          : hrefFor(side(), p) +
              (fragment === null ? "" : "#" + encodeURIComponent(fragment)),
      ),
    anchor: (a) => withBase(base(), mdHref(side(), props.path, a)),
  };
  const go = (hash: string): void => {
    if (hash.length < 2) return;
    let a: string;
    try {
      a = decodeURIComponent(hash.slice(1));
    } catch {
      return;
    }
    const el = document.getElementById(anchorId(a));
    if (el === null) return;
    el.setAttribute("tabindex", "-1");
    el.scrollIntoView({ block: "start" });
    el.focus({ preventScroll: true });
  };
  // After the router: it scrolls to the top on a hash it has no
  // element for.
  createEffect(
    on(
      () => location.hash,
      (hash) => {
        const frame = requestAnimationFrame(() => go(hash));
        onCleanup(() => cancelAnimationFrame(frame));
      },
    ),
  );
  let wrap!: HTMLDivElement;
  const onClick = (e: MouseEvent): void => {
    if (e.button !== 0 || e.ctrlKey || e.metaKey || e.shiftKey || e.altKey) {
      return;
    }
    const a =
      e.target instanceof Element
        ? e.target.closest<HTMLAnchorElement>("a")
        : null;
    if (a === null || a.pathname !== window.location.pathname) return;
    if (a.hash !== "" && a.hash === window.location.hash) {
      e.preventDefault();
      go(a.hash);
    }
  };
  onMount(() => {
    // Native: before the router's click handler on the document.
    wrap.addEventListener("click", onClick);
    onCleanup(() => wrap.removeEventListener("click", onClick));
  });
  return (
    <div class="md-page" ref={(el) => (wrap = el)}>
      <LinksContext.Provider value={links}>
        <DocView doc={props.doc} level={1} />
      </LinksContext.Provider>
    </div>
  );
}
