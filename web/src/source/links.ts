// Where each token link of api/source goes, and what a reader hears
// (D-283). Pure: paint.ts makes the <a>.
import { docsHref } from "../api/module";
import type { ExternalTarget, Target } from "../api/source";
import { hrefFor, lineHref, withBase } from "../files/paths";
import { utf16Cols } from "./columns";
import type { Seg } from "./highlight";

export interface LinkInfo {
  href: string;
  // What follows the token's text in its name: "x, <about>".
  about: string;
  // Resolved heuristically: dotted underline, said in the name.
  guess: boolean;
  // Off site (Hoogle): new tab.
  external: boolean;
  // A link into this same file.
  here: boolean;
}

const hoogle = "https://hoogle.haskell.org/?hoogle=";

export function hoogleHref(query: string): string {
  return hoogle + encodeURIComponent(query);
}

// The file and module the view shows.
export interface ViewPlace {
  path: string;
  ref: string | null;
}

function externalInfo(t: ExternalTarget): LinkInfo {
  const [, module, pkg, how, count, query] = t;
  // one-of: the module is just the first of the open imports.
  const where =
    how === "one-of"
      ? `one of ${count} modules`
      : `${module}${pkg === null ? "" : ` (${pkg})`}`;
  return {
    href: hoogleHref(query),
    about: `${where}, search on Hoogle`,
    guess: how !== "listed",
    external: true,
    here: false,
  };
}

// One per target, by index (link word − 1). base: the router base.
export function linkInfos(
  view: ViewPlace,
  targets: readonly Target[],
  base: string | undefined,
): (LinkInfo | undefined)[] {
  const app = (rel: string) => withBase(base, rel);
  return targets.map((t): LinkInfo => {
    switch (t[0]) {
      case "local":
        return {
          href: app(lineHref(view.path, t[1])),
          about: `bound at line ${t[1]}`,
          guess: false,
          external: false,
          here: true,
        };
      case "decl": {
        const [, ref, path, line, id, reasons, , , def] = t;
        const guess = reasons.length > 0;
        if (def) {
          return {
            href: app(docsHref(ref, id)),
            about: "documentation",
            guess,
            external: false,
            here: false,
          };
        }
        return {
          href: app(line === null ? hrefFor(path) : lineHref(path, line)),
          about:
            line === null
              ? `defined in ${path}`
              : `defined in ${path} line ${line}`,
          guess,
          external: false,
          here: path === view.path,
        };
      }
      case "module": {
        const [, ref, path, reasons] = t;
        return {
          href: app(path === null ? docsHref(ref) : hrefFor(path)),
          about: `module ${ref}`,
          guess: reasons.length > 0,
          external: false,
          here: path === view.path,
        };
      }
      case "external":
        return externalInfo(t);
    }
  });
}

// The module page's links (T10b): the page's own ref (as in the
// targets) and as typed in the URL (kept for same-module links). Every
// declaration goes to its docs, a local has no link (no line anchors
// on a docs page). One per target, by index.
export function pageLinkInfos(
  page: { ref: string; typedRef: string },
  targets: readonly Target[],
  base: string | undefined,
): (LinkInfo | undefined)[] {
  const app = (rel: string) => withBase(base, rel);
  return targets.map((t): LinkInfo | undefined => {
    switch (t[0]) {
      case "local":
        return undefined;
      case "decl": {
        const [, ref, , , id, reasons] = t;
        const same = ref === page.ref;
        return {
          href: app(docsHref(same ? page.typedRef : ref, id)),
          about: same ? "documentation" : `documentation in ${ref}`,
          guess: reasons.length > 0,
          external: false,
          here: false,
        };
      }
      case "module": {
        const [, ref, , reasons] = t;
        return {
          href: app(docsHref(ref)),
          about: `module ${ref}`,
          guess: reasons.length > 0,
          external: false,
          here: false,
        };
      }
      case "external":
        return externalInfo(t);
    }
  });
}

// A shorter name, for signatures, heads and Haddock names: the text
// and the marks only (T10b).
export function shortName(text: string, info: LinkInfo): string {
  return (
    text +
    (info.guess ? ", resolved heuristically" : "") +
    (info.external ? ", opens in a new tab" : "")
  );
}

// A link's accessible name: it starts with the visible text.
export function linkName(text: string, info: LinkInfo): string {
  return (
    `${text}, ${info.about}` +
    (info.guess ? ", resolved heuristically" : "") +
    (info.external ? ", opens in a new tab" : "")
  );
}

// Where following a same-file link puts the cursor: line, UTF-16
// column. A local: its binder. A use of a declaration here: the
// definition's name on its line (the seg linking to the same
// declaration with def set), else the line's start. Else undefined.
export function jumpOf(
  link: number,
  targets: readonly Target[],
  runs: readonly (readonly Seg[])[],
  line: (n: number) => string,
): { line: number; col: number } | undefined {
  const t = targets[link - 1];
  if (t === undefined) return undefined;
  const at = (n: number, cp: number) => ({
    line: n,
    col: utf16Cols(line(n), [cp])[0] ?? 0,
  });
  if (t[0] === "local") return at(t[1], t[2]);
  if (t[0] !== "decl" || t[3] === null || t[8]) return undefined;
  const [, ref, , n, id] = t;
  const def = (runs[n] ?? []).find((s) => {
    const u = targets[s.link - 1];
    return u?.[0] === "decl" && u[8] && u[1] === ref && u[4] === id;
  });
  return def === undefined ? { line: n, col: 0 } : at(n, def.from);
}
