// Why a module page is text only: the banner text, and the line to
// open the source at.
import type { TextOnlyReason } from "../api/module";
import { reasonText } from "../modules/notices";
import { bytes, limit } from "../ui/format";

export function reasonBanner(r: TextOnlyReason): string {
  switch (r.kind) {
    case "parse-error":
    case "pragma-error": {
      const where: string[] = [];
      if (r.code !== null) where.push(r.code);
      if (r.span !== null) {
        where.push(`line ${r.span[0]}, column ${r.span[1]}`);
      }
      const what =
        r.kind === "parse-error"
          ? "This module doesn't parse"
          : "A pragma at the top of this module is wrong";
      const at = where.length === 0 ? "" : ` (${where.join(", ")})`;
      return `${what}: ${noDot(r.message)}${at}. Shown as text.`;
    }
    case "preprocessed":
      return `Preprocessed by ${r.tool}: shown as text, not indexed.`;
    case "generated":
      return `Generated from this file by ${r.tool}: shown as text, not indexed.`;
    case "parser-crashed":
      return `The parser crashed on this module: ${noDot(r.message)}. Shown as text.`;
    case "too-slow":
      return "Parsing this module took too long. Shown as text.";
    case "no-source":
      return `Listed, but there is no source (${reasonText(r.reason)}).`;
    case "not-text":
      return `This module is not UTF-8 text (${bytes(r.size)}).`;
    case "too-large":
      return `This module is too large to parse (${bytes(r.size)}; the limit is ${limit(r.limit)}).`;
  }
}

// Where "View source" should land: the error's line, if any.
export function reasonLine(r: TextOnlyReason): number | undefined {
  return (r.kind === "parse-error" || r.kind === "pragma-error") &&
    r.span !== null
    ? r.span[0]
    : undefined;
}

// No "..": the banner adds its own full stop.
const noDot = (s: string) => s.trim().replace(/\.$/, "");
