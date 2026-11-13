// A new answer for a page that is on screen (T10c, D-293). While the
// page is partial, only its links can change: `partial`, the targets,
// a code token's link and a doc name's link. The page stays the first
// answer; the links that changed go in an overlay, keyed by the first
// answer's token or name object. Pure.
import { type CodeToken, tokenClasses } from "../api/code";
import type { ModulePage, PageTarget } from "../api/module";
import type { Inline } from "../api/richtext";

export type NameInline = Extract<Inline, { kind: "name" }>;

// What is shown now: the first answer, plus this.
export interface Overlay {
  partial: boolean;
  // Merged: old ones keep their index, new ones go at the end.
  targets: readonly PageTarget[];
  // Token or name of the first answer -> its current version.
  links: ReadonlyMap<object, CodeToken | NameInline>;
}

export interface Relinked {
  partial: boolean;
  // The same array when nothing was added.
  targets: readonly PageTarget[];
  // Only what changed with this answer.
  changes: Map<object, CodeToken | NameInline>;
}

// same: nothing to do. other: the answer differs beyond links (keep
// the old page, stop asking).
export type Relink = "same" | "other" | Relinked;

const classes = new Set<unknown>(tokenClasses);

// [class, text] or [class, text, null, link]: a token that may link
// out. [class, text, n] (a local handle) never changes.
function outToken(v: unknown[]): boolean {
  return (
    (v.length === 2 || (v.length === 4 && v[2] === null)) &&
    classes.has(v[0]) &&
    typeof v[1] === "string"
  );
}

function isName(v: object): v is NameInline {
  return (v as { kind?: unknown }).kind === "name";
}

export function relink(
  shown: ModulePage,
  now: Overlay,
  next: ModulePage,
): Relink {
  if (shown.parse.kind !== "parsed" || next.parse.kind !== "parsed") {
    return "other";
  }
  const nextTargets = next.parse.targets;
  let targets = now.targets;
  let index: Map<string, number> | undefined;
  // Next answer's link -> a link into the merged targets.
  const remapped = new Map<number, number>();
  const remap = (k: number): number => {
    const got = remapped.get(k);
    if (got !== undefined) return got;
    if (index === undefined) {
      index = new Map(targets.map((t, i) => [JSON.stringify(t), i + 1]));
    }
    const key = JSON.stringify(nextTargets[k - 1]);
    let m = index.get(key);
    if (m === undefined) {
      if (targets === now.targets) targets = [...now.targets];
      (targets as PageTarget[]).push(nextTargets[k - 1]!);
      m = targets.length;
      index.set(key, m);
    }
    remapped.set(k, m);
    return m;
  };
  const changes = new Map<object, CodeToken | NameInline>();

  const token = (a: unknown[], b: unknown[]): boolean => {
    if (a[0] !== b[0] || a[1] !== b[1]) return false;
    const cur = (now.links.get(a) ?? a) as unknown[];
    const was = cur.length === 4 ? (cur[3] as number) : null;
    const k = b.length === 4 ? remap(b[3] as number) : null;
    if (was !== k) {
      const [cls, text] = a as [CodeToken[0], string];
      changes.set(a, k === null ? [cls, text] : [cls, text, null, k]);
    }
    return true;
  };

  const name = (a: NameInline, b: NameInline): boolean => {
    if (
      a.written !== b.written ||
      a.ns !== b.ns ||
      a.target !== b.target ||
      Object.keys(a).length !== Object.keys(b).length
    ) {
      return false;
    }
    const cur = (now.links.get(a) ?? a) as NameInline;
    const k = b.link === null ? null : remap(b.link);
    if (cur.link !== k) changes.set(a, { ...a, link: k });
    return true;
  };

  const walk = (a: unknown, b: unknown): boolean => {
    if (a === b) return true;
    if (typeof a !== "object" || typeof b !== "object") return false;
    if (a === null || b === null) return false;
    if (Array.isArray(a)) {
      if (!Array.isArray(b)) return false;
      if (outToken(a) && outToken(b)) return token(a, b);
      if (a.length !== b.length) return false;
      for (let i = 0; i < a.length; i++) {
        if (!walk(a[i], b[i])) return false;
      }
      return true;
    }
    if (Array.isArray(b)) return false;
    if (isName(a) && isName(b)) return name(a, b);
    return fields(a, b);
  };

  // Same keys, same values; the skip keys are looked at elsewhere.
  const fields = (a: object, b: object, ...skip: string[]): boolean => {
    const ka = Object.keys(a);
    if (ka.length !== Object.keys(b).length) return false;
    const ra = a as Record<string, unknown>;
    const rb = b as Record<string, unknown>;
    for (const k of ka) {
      if (!(k in rb)) return false;
      if (!skip.includes(k) && !walk(ra[k], rb[k])) return false;
    }
    return true;
  };

  // The top and the parsed object by hand: partial and targets are
  // allowed to differ there, nowhere else.
  if (
    !fields(shown, next, "partial", "parse") ||
    !fields(shown.parse, next.parse, "targets")
  ) {
    return "other";
  }
  if (
    next.partial === now.partial &&
    changes.size === 0 &&
    targets === now.targets
  ) {
    return "same";
  }
  return { partial: next.partial, targets, changes };
}
