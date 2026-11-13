// What "References" does for the token at the cursor (T12). Pure.
import type { Target } from "../api/source";
import { utf16Cols } from "./columns";
import type { Seg } from "./highlight";

export type RefTarget =
  // A declaration of ours: the refs page.
  | { kind: "decl"; name: string; ref: string; id: string }
  // A local: the bar. binder: where it is bound, code points.
  | {
      kind: "local";
      name: string;
      binder: { line: number; cp: number };
    }
  // A module or an outside name: no refs.
  | { kind: "other" }
  | { kind: "none" };

// Local binders by "line:cp" (their segs have no link, dropBinderLinks).
export function binderMap(
  targets: readonly Target[],
): Map<string, { line: number; cp: number }> {
  const out = new Map<string, { line: number; cp: number }>();
  for (const t of targets) {
    if (t[0] === "local") out.set(`${t[1]}:${t[2]}`, { line: t[1], cp: t[2] });
  }
  return out;
}

// The code point column (from 1) of a UTF-16 column (from 0).
export function cpOf(text: string, col: number): number {
  let n = 1;
  for (let i = 0; i < Math.min(col, text.length); i++) {
    const c = text.charCodeAt(i);
    const d = text.charCodeAt(i + 1);
    // A pair counts once: skip its low half.
    if (c >= 0xd800 && c <= 0xdbff && d >= 0xdc00 && d <= 0xdfff) i++;
    n++;
  }
  return n;
}

// The text of a seg on its line.
function segText(text: string, s: Seg): string {
  const [a, b] = utf16Cols(text, [s.from, s.to]) as [number, number];
  return text.slice(a, b);
}

export function refTargetAt(
  runs: readonly (readonly Seg[])[],
  targets: readonly Target[],
  binders: ReadonlyMap<string, { line: number; cp: number }>,
  lineText: (n: number) => string,
  cursor: { line: number; col: number },
): RefTarget {
  const text = lineText(cursor.line);
  const cp = cpOf(text, cursor.col);
  const seg = (runs[cursor.line] ?? []).find((s) => s.from <= cp && cp < s.to);
  if (seg === undefined) return { kind: "none" };
  const name = segText(text, seg);
  if (seg.link === 0) {
    const binder = binders.get(`${cursor.line}:${seg.from}`);
    return binder === undefined
      ? { kind: "none" }
      : { kind: "local", name, binder };
  }
  const t = targets[seg.link - 1];
  if (t === undefined) return { kind: "none" };
  switch (t[0]) {
    case "local":
      return {
        kind: "local",
        name,
        binder: { line: t[1], cp: t[2] },
      };
    case "decl":
      return { kind: "decl", name, ref: t[1], id: t[4] };
    default:
      return { kind: "other" };
  }
}
