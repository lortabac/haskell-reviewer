// What the hover card of a token link says (G12, D-285). Pure: the
// card is built with textContent (createHoverCard.ts).
import type { ReasonCode, Target } from "../api/source";

// code: a line of code (a signature), shown in monospace; lines: plain
// sentences under it.
export interface CardText {
  code: string | null;
  lines: string[];
}

// Why a link is a guess, in words. k: the number of candidates.
export function reasonText(r: ReasonCode, k?: number): string {
  switch (r) {
    case "ambiguous":
      return k === undefined
        ? "One of several candidates in scope."
        : `One of ${k} candidates in scope.`;
    case "not-ready":
      return "A module it depends on wasn't indexed yet.";
    case "not-parsed":
      return "A module it depends on didn't parse.";
    case "made-by-splice":
      return "Its module makes names with Template Haskell.";
    case "made-by-splice-here":
      return "Not found; this module makes names with Template Haskell.";
    case "via-splice":
      return "Used in a splice; only this module has it.";
    case "wildcard":
      return "Maybe bound by a {..} pattern.";
    case "record-dot":
      return "A record field picked by name only.";
    case "data-kinds":
      return "A constructor used as a type (DataKinds).";
    case "cpp-import":
      return "Imported only under a CPP condition.";
    case "import-cycle":
      return "Its module is in an import cycle.";
    case "ambiguous-module":
      return "Several modules have this name; this is the first.";
    case "unlisted-module":
      return "Its module isn't listed in any component.";
    case "loose-import":
      return "Imported from a loose file, so no build-depends to go by.";
    case "listed-not-found":
      return "Named in an import list, but its module doesn't export it.";
    case "this-module":
      return "More than one declaration in this module could fit.";
  }
}

// "Resolved heuristically: <reasons>", each reason once; none if exact.
function why(reasons: readonly ReasonCode[], k: number): string[] {
  if (reasons.length === 0) return [];
  const words = [...new Set(reasons)].map((r) => reasonText(r, k));
  return [`Resolved heuristically: ${words.join(" ")}`];
}

export function cardOf(t: Target): CardText {
  switch (t[0]) {
    case "local":
      return { code: null, lines: [`Local, bound at line ${t[1]}.`] };
    case "decl": {
      const [, ref, , , , reasons, k, sig] = t;
      return {
        code: sig,
        lines: [
          ...(sig === null ? ["No type signature."] : []),
          `Module ${ref}.`,
          ...why(reasons, k),
        ],
      };
    }
    case "module": {
      const [, ref, , reasons, k] = t;
      return {
        code: null,
        lines: [
          `Module ${ref}. The declaration wasn't found in it.`,
          ...why(reasons, k),
        ],
      };
    }
    case "external": {
      const [, module, pkg, how, count] = t;
      const from = `From ${module}${pkg === null ? "" : ` (${pkg})`}`;
      const line =
        how === "listed"
          ? `${from}.`
          : how === "only-open"
            ? `${from}, the only open import that could have it. A guess.`
            : `From one of ${count} open imports. A guess.`;
      return { code: null, lines: [`${line} Search on Hoogle.`] };
    }
  }
}
