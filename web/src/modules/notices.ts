// Texts for the module tree: project notices, markers, banners.
import type {
  ModuleEntry,
  ModuleSource,
  NoSourceReason,
  Notice,
} from "../api/modules";

// "." is ROOT.
function dirName(dir: string): string {
  return dir === "." ? "ROOT" : dir;
}

export function noticeText(n: Notice): string {
  switch (n.kind) {
    case "stack":
      return "stack.yaml isn't supported: without a cabal.project, the .cabal files in ROOT are used. Commit a cabal.project to choose the packages.";
    case "implicit":
      return "No cabal.project or .cabal file: ROOT is read as one implicit package, with module names from file paths (GHC2021).";
    case "hpack": {
      const dirs = n.dirs.map(dirName).join(", ");
      // ROOT with no .cabal file is the implicit package, not a plain dir.
      const rest = n.dirs.includes(".")
        ? "until then ROOT is read as an implicit package"
        : n.dirs.length === 1
          ? "until then it's a plain directory"
          : "until then these are plain directories";
      return `Hpack isn't supported: package.yaml without a .cabal file in ${dirs}. Run hpack and commit the .cabal file; ${rest}.`;
    }
  }
}

export function reasonText(r: NoSourceReason): string {
  switch (r) {
    case "autogen":
      return "autogen";
    case "missing":
      return "file missing";
    case "outside":
      return "outside ROOT";
    case "symlink":
      return "symlink";
  }
}

export function sourceMarker(s: ModuleSource): string | undefined {
  switch (s.kind) {
    case "module":
      return undefined;
    case "generated":
      return `generated (${s.tool})`;
    case "preprocessed":
      return `preprocessed (${s.tool})`;
    case "no-source":
      return `listed, no source (${reasonText(s.reason)})`;
  }
}

// The marker after a module name, if any.
export function moduleMarker(m: ModuleEntry): string | undefined {
  const parts: string[] = [];
  if (m.role === "signature") parts.push("signature");
  const s = sourceMarker(m.source);
  if (s !== undefined) parts.push(s);
  return parts.length === 0 ? undefined : parts.join(", ");
}

// The banner on the file view of a generated or preprocessed source.
export function bannerText(s: ModuleSource): string | undefined {
  switch (s.kind) {
    case "generated":
      return `Generated from this file by ${s.tool}: shown as text, not indexed.`;
    case "preprocessed":
      return `Preprocessed by ${s.tool}: shown as text, not indexed.`;
    default:
      return undefined;
  }
}

export function warningsTitle(n: number): string {
  return n === 1 ? "1 project warning" : `${n} project warnings`;
}
