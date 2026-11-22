// Counts and texts for Home in diff mode and the header's outside-ROOT
// count (D-338). Pure.
import {
  type ChangedFile,
  type FileStatus,
  type Outside,
  statuses,
} from "../api/diffFiles";
import type { ModuleView } from "../modules/union";
import { plural } from "../ui/format";

export type Counts = Record<FileStatus, number>;

const zero = (): Counts => ({ added: 0, removed: 0, modified: 0, renamed: 0 });

export function fileCounts(files: readonly ChangedFile[]): Counts {
  const c = zero();
  for (const f of files) c[f.status]++;
  return c;
}

export interface ModuleCounts {
  counts: Counts;
  // Modules whose parse settings or role changed.
  settings: number;
}

// From head's view (unionModules on head): its ghosts are the removed
// modules, so Home counts what the sidebar shows. A module in two
// components counts twice, as it shows twice.
export function moduleCounts(view: ModuleView): ModuleCounts {
  const counts = zero();
  let settings = 0;
  const add = (m: {
    change: FileStatus | null;
    settingsChanged: boolean;
  }): void => {
    if (m.change !== null) counts[m.change]++;
    if (m.settingsChanged) settings++;
  };
  for (const p of view.packages) {
    for (const c of p.components) {
      for (const m of c.modules) {
        // A ghost's settingsChanged isn't counted: the server sets it
        // only on modules on both sides.
        if (m.ghostOn === undefined) add(m.entry);
        else counts.removed++;
      }
    }
  }
  for (const o of view.other) {
    if (o.ghostOn !== undefined) counts.removed++;
    else if (o.file.kind === "module") add(o.file);
  }
  return { counts, settings };
}

function total(c: Counts): number {
  return c.added + c.removed + c.modified + c.renamed;
}

// "5 files changed: 2 added, 2 modified" (zero parts left out), or "No
// files changed".
function countsText(c: Counts, one: string, many: string): string {
  const n = total(c);
  if (n === 0) return `No ${many} changed`;
  const parts = statuses
    .filter((s) => c[s] > 0)
    .map((s) => `${c[s].toLocaleString("en-US")} ${s}`);
  return `${plural(n, one, many)} changed: ${parts.join(", ")}`;
}

export function filesLine(c: Counts): string {
  return total(c) === 0
    ? "No files changed under ROOT."
    : `${countsText(c, "file", "files")}.`;
}

// "3 modules changed: 1 added, 2 modified; settings changed in 1
// module."
export function modulesLine(m: ModuleCounts): string {
  const settings =
    m.settings === 0
      ? ""
      : `; settings changed in ${plural(m.settings, "module", "modules")}`;
  return `${countsText(m.counts, "module", "modules")}${settings}.`;
}

// The count, "100+" when the server stopped counting.
export function outsideCount(o: Outside): string {
  return o.total.toLocaleString("en-US") + (o.truncated ? "+" : "");
}

// "3 files changed outside ROOT."
export function outsideLine(o: Outside): string {
  const files = o.total === 1 && !o.truncated ? "file" : "files";
  return `${outsideCount(o)} ${files} changed outside ROOT.`;
}

// How many paths the header's tip lists.
const tipPaths = 10;

// The header tip: the first paths, one per line, then what is left.
export function outsideTip(o: Outside): string {
  const lines = o.paths.slice(0, tipPaths);
  const rest = o.total - lines.length;
  if (rest > 0) {
    const n = rest.toLocaleString("en-US") + (o.truncated ? "+" : "");
    lines.push(`and ${n} more, listed on Home`);
  } else if (o.truncated) {
    lines.push("and more");
  }
  return lines.join("\n");
}
