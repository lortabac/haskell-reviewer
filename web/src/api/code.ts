// Code on the module page: heads, signatures, types. Shape: see Note
// [Module page] in HaskellReviewer.Server.Wire.Page.
import { DecodeError, nat, oneOf, positive, str } from "./decode";

// Token classes; "sp" is a space between tokens.
export const tokenClasses = [
  "sp",
  "kw",
  "var",
  "con",
  "op",
  "conop",
  // ! ~ @ . * - %: operators that are syntax too. Never linked.
  "rop",
  "pun",
  "num",
  "str",
  "chr",
  "prag",
  "com",
  "doc",
  "mac",
] as const;

export type TokenClass = (typeof tokenClasses)[number];

// [class, text]; [class, text, handle] when it links to a declaration
// of this module (a Handle, see module.ts); [class, text, null, link]
// when it links out of the module: link is 1 + the index of its target
// in the page's targets.
export type CodeToken =
  | [TokenClass, string]
  | [TokenClass, string, number]
  | [TokenClass, string, null, number];

export type Code = CodeToken[];

const tokenClass = oneOf(tokenClasses);

export function decodeCodeToken(value: unknown, path: string): CodeToken {
  if (!Array.isArray(value) || value.length < 2 || value.length > 4) {
    throw new DecodeError(
      path,
      "a token: [class, text], [class, text, n] or [class, text, null, link]",
    );
  }
  const cls = tokenClass(value[0], `${path}[0]`);
  const text = str(value[1], `${path}[1]`);
  if (value.length === 2) return [cls, text];
  if (value.length === 3) return [cls, text, nat(value[2], `${path}[2]`)];
  if (value[2] !== null) throw new DecodeError(`${path}[2]`, "null");
  return [cls, text, null, positive(value[3], `${path}[3]`)];
}

// A token's link out of the module (1 + its target's index), if any.
export function outerLink(t: CodeToken): number | null {
  return t.length === 4 ? t[3] : null;
}

export function decodeCode(value: unknown, path: string): Code {
  if (!Array.isArray(value)) throw new DecodeError(path, "an array");
  return value.map((v: unknown, i) => decodeCodeToken(v, `${path}[${i}]`));
}

// The text of some code, as one string.
export function codeText(code: Code): string {
  return code.map((t) => t[1]).join("");
}
