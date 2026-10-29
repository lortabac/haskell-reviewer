// Code on the module page: heads, signatures, types. Shape: see Note
// [Module page] in HaskellReviewer.Server.Wire.Page.
import { DecodeError, nat, oneOf, str } from "./decode";

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

// [class, text], or [class, text, handle] when it links to a
// declaration of this module (a Handle, see module.ts).
export type CodeToken = [TokenClass, string] | [TokenClass, string, number];

export type Code = CodeToken[];

const tokenClass = oneOf(tokenClasses);

export function decodeCodeToken(value: unknown, path: string): CodeToken {
  if (!Array.isArray(value) || (value.length !== 2 && value.length !== 3)) {
    throw new DecodeError(path, "a token: [class, text] or [class, text, id]");
  }
  const cls = tokenClass(value[0], `${path}[0]`);
  const text = str(value[1], `${path}[1]`);
  return value.length === 3
    ? [cls, text, nat(value[2], `${path}[2]`)]
    : [cls, text];
}

export function decodeCode(value: unknown, path: string): Code {
  if (!Array.isArray(value)) throw new DecodeError(path, "an array");
  return value.map((v: unknown, i) => decodeCodeToken(v, `${path}[${i}]`));
}

// The text of some code, as one string.
export function codeText(code: Code): string {
  return code.map((t) => t[1]).join("");
}
