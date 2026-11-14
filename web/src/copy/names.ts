// The texts of Copy (SPEC §5.7, D-299). Pure.
import type { Target } from "../api/source";

// Data.Foo + bar -> Data.Foo.bar. Operators too: Data.Foo.<+>.
export function qualified(mod: string, name: string): string {
  return `${mod}.${name}`;
}

// A token as written in the source, without its module qualifier:
// M.lookup -> lookup, Map.! -> !, A.B.Foo -> Foo.
export function stripQualifier(token: string): string {
  const rest = token.replace(/^(?:\p{Lu}[\p{L}\p{N}_']*\.)+/u, "");
  // "Foo." alone is not a qualifier of anything.
  return rest === "" ? token : rest;
}

// path:12 or path:12-15.
export function location(path: string, from: number, to?: number): string {
  return to === undefined || to <= from
    ? `${path}:${from}`
    : `${path}:${from}-${to}`;
}

// What y copies for a token with a link target. undefined: nothing
// good to copy (a local, a guess among several, a module not in the
// tree): the caller falls back to the place.
export function targetCopyText(
  token: string,
  target: Target,
  moduleName: (ref: string) => string | undefined,
): string | undefined {
  const name = stripQualifier(token);
  switch (target[0]) {
    case "decl": {
      const mod = moduleName(target[1]);
      return mod === undefined ? undefined : qualified(mod, name);
    }
    case "module":
      return moduleName(target[1]);
    case "external":
      // The token is the module itself (an import).
      if (token === target[1]) return token;
      // "one-of": the module is one of several candidates.
      return target[3] === "one-of" ? undefined : qualified(target[1], name);
    case "local":
      return undefined;
  }
}
