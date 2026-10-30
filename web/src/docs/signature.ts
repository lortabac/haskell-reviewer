// Signatures: where they may wrap (SPEC §5.5) and one name's own
// signature out of a shared one.
import type { Code, CodeToken } from "../api/code";

// Tokens that never count as syntax: text inside them is just text.
const notSyntax = new Set(["str", "chr", "com", "doc", "prag", "mac"]);

const arrows = new Set(["->", "=>", "→", "⇒", "⊸"]);
const opens = new Set(["(", "[", "{", "(#", "(|", "[|", "[||"]);
const closes = new Set([")", "]", "}", "#)", "|)", "|]", "||]"]);

// Depth after each token: brackets count, nothing else.
function depths(code: Code): number[] {
  let depth = 0;
  return code.map(([cls, text]) => {
    if (cls === "pun") {
      if (opens.has(text)) depth++;
      else if (closes.has(text)) depth = Math.max(0, depth - 1);
    }
    return depth;
  });
}

// The code cut before each top-level arrow (->, =>, and the unicode
// ones), where Haddock breaks a long signature. The space before an
// arrow is dropped: the page puts a breakable space between parts.
export function wrapParts(code: Code): Code[] {
  const ds = depths(code);
  const parts: Code[] = [];
  let cur: CodeToken[] = [];
  code.forEach((t, i) => {
    const [cls, text] = t;
    const top = (i === 0 ? 0 : (ds[i - 1] ?? 0)) === 0;
    if (top && !notSyntax.has(cls) && arrows.has(text) && cur.length > 0) {
      while (cur.length > 0 && cur[cur.length - 1]?.[0] === "sp") cur.pop();
      if (cur.length > 0) parts.push(cur);
      cur = [];
    }
    cur.push(t);
  });
  if (cur.length > 0) parts.push(cur);
  return parts;
}

// Index of the first top-level "::" (or "∷"), or -1.
function firstColons(code: Code): number {
  const ds = depths(code);
  return code.findIndex(
    ([cls, text], i) =>
      cls === "pun" &&
      (text === "::" || text === "∷") &&
      (i === 0 ? 0 : (ds[i - 1] ?? 0)) === 0,
  );
}

// An operator name: not a letter or "_" first.
export function isOperator(name: string): boolean {
  return !/^[\p{L}_]/u.test(name);
}

// The name as it starts a signature: (<+>) for an operator.
export function nameTokens(name: string): Code {
  if (!isOperator(name)) {
    return [[/^\p{Lu}/u.test(name) ? "con" : "var", name]];
  }
  return [
    ["pun", "("],
    [name.startsWith(":") ? "conop" : "op", name],
    ["pun", ")"],
  ];
}

// "name :: T" out of a signature with several names ("f, g :: T"), or
// a GADT line ("A, B :: T"). Cut at the first top-level "::": a
// record GADT has more "::" inside its braces. No "::": just the name.
// A leading keyword stays ("pattern A, B :: T" -> "pattern A :: T").
export function ownSignature(code: Code, name: string): Code {
  const first = code[0];
  const kw: Code =
    first?.[0] === "kw"
      ? [
          [first[0], first[1]],
          ["sp", " "],
        ]
      : [];
  const at = firstColons(code);
  if (at === -1) return [...kw, ...nameTokens(name)];
  return [...kw, ...nameTokens(name), ["sp", " "], ...code.slice(at)];
}
