// The notes of a diff page (D-342): why there are no lines, and what
// the lines don't show (line endings, BOM, type and mode). Pure.
import type { ChangeNote } from "../api/diffFiles";
import type { DiffSide, DiffView } from "../api/diff";
import { shortHash } from "../header/revText";
import { bytes, limit, plural } from "../ui/format";

export const noteTexts = {
  filtered:
    "Shown as on disk: a git filter applies; the stored content may differ.",
  modeChanged: "File mode changed.",
  symlink: "Symbolic link: the lines are its target.",
  unchanged: "No changes in this file.",
  noTextChange: "No changes in the text.",
  wsOnly: "No changes when whitespace is ignored.",
  tooSlow: "This diff took over 2 s, so it isn't shown.",
  notLocal: "Not in the local repo (partial clone).",
};

// The short words for a file's note, on Home.
export function changeNoteWords(n: ChangeNote): string {
  switch (n) {
    case "type-changed":
      return "type changed";
    case "mode-changed":
      return "mode changed";
    case "filtered":
      return "git filter";
  }
}

// What a side is, as one short phrase: for "base: …; head: …".
function sideWords(s: DiffSide): string {
  switch (s.kind) {
    case "absent":
      return "no file";
    case "text":
      return "text";
    case "binary":
      return `binary (${bytes(s.size)})`;
    case "too-large":
      return `too large (${bytes(s.size)}; the limit is ${limit(s.limit)})`;
    case "symlink":
      return `symbolic link to ${s.target}`;
    case "submodule":
      return subproject(s.commit);
    case "not-local":
      return "not in the local repo";
  }
}

const nestedRepo = "nested git repository";

function subproject(commit: string | null): string {
  return commit === null
    ? nestedRepo
    : `subproject commit ${shortHash(commit)}`;
}

// "Too large to diff (size; the limit is L)".
function tooLarge(sizes: string, lim: number): string {
  return `Too large to diff (${sizes}; the limit is ${limit(lim)}).`;
}

function sentence(s: string): string {
  return s.charAt(0).toUpperCase() + s.slice(1) + ".";
}

// Why a not-text diff has no lines: one line.
export function notTextNote(base: DiffSide, head: DiffSide): string {
  const one =
    base.kind === "absent" ? head : head.kind === "absent" ? base : undefined;
  const how = base.kind === "absent" ? "added" : "removed";
  if (one !== undefined) {
    switch (one.kind) {
      case "binary":
        return `Binary file ${how} (${bytes(one.size)}).`;
      case "submodule":
        return sentence(`${subproject(one.commit)} ${how}`);
      case "too-large":
        return tooLarge(bytes(one.size), one.limit);
      case "not-local":
        return noteTexts.notLocal;
      default:
        return sentence(sideWords(one));
    }
  }
  if (base.kind === "binary" && head.kind === "binary") {
    return `Binary file changed (${bytes(base.size)} → ${bytes(head.size)}).`;
  }
  if (base.kind === "submodule" && head.kind === "submodule") {
    const c = (x: string | null) => (x === null ? nestedRepo : shortHash(x));
    return `Subproject commit ${c(base.commit)} → ${c(head.commit)}.`;
  }
  // Too large on both sides, or on one with text on the other: say
  // which.
  if (base.kind === "too-large" && head.kind === "too-large") {
    return tooLarge(`${bytes(base.size)} → ${bytes(head.size)}`, head.limit);
  }
  if (base.kind === "too-large" && head.kind === "text") {
    return tooLarge(`base: ${bytes(base.size)}`, base.limit);
  }
  if (head.kind === "too-large" && base.kind === "text") {
    return tooLarge(`head: ${bytes(head.size)}`, head.limit);
  }
  if ([base, head].every((s) => s.kind === "not-local" || s.kind === "text")) {
    return noteTexts.notLocal;
  }
  return `Base: ${sideWords(base)}; head: ${sideWords(head)}.`;
}

// What git calls the entry's type.
type EntryKind = "file" | "symlink" | "submodule";

function kindOf(s: DiffSide): EntryKind | undefined {
  switch (s.kind) {
    case "absent":
      return undefined;
    case "symlink":
      return "symlink";
    case "submodule":
      return "submodule";
    default:
      return "file";
  }
}

const kindWords: Record<EntryKind, string> = {
  file: "a file",
  symlink: "a symbolic link",
  submodule: "a subproject",
};

export function typeChangedNote(base: DiffSide, head: DiffSide): string {
  const a = kindOf(base);
  const b = kindOf(head);
  return a === undefined || b === undefined || a === b
    ? "File type changed."
    : `Changed from ${kindWords[a]} to ${kindWords[b]}.`;
}

// Every note of a diff, in order. The too-slow note is first; the page
// adds its "Try again" button.
export function diffNotes(v: DiffView): string[] {
  const out: string[] = [];
  const d = v.diff;
  if (d.kind === "too-slow") out.push(noteTexts.tooSlow);
  if (d.kind === "not-text") out.push(notTextNote(v.base, v.head));
  if (v.status === "renamed" && v.oldPath !== null) {
    const sim = v.similarity === null ? "" : ` (${v.similarity}%)`;
    out.push(`Renamed from ${v.oldPath}${sim}.`);
  }
  const symlink = v.base.kind === "symlink" || v.head.kind === "symlink";
  if (v.note === "type-changed") out.push(typeChangedNote(v.base, v.head));
  else if (d.kind === "lines" && symlink) out.push(noteTexts.symlink);
  if (v.note === "mode-changed") out.push(noteTexts.modeChanged);
  if (v.note === "filtered") out.push(noteTexts.filtered);
  if (d.kind === "lines") {
    if (d.crlfLines > 0) {
      const n = plural(d.crlfLines, "line", "lines");
      out.push(`Line endings changed on ${n}; not shown as changes.`);
    }
    if (d.bom !== null) out.push(`Byte order mark ${d.bom}.`);
    const same = d.runs.every(([op]) => op === "equal");
    if (same && d.runs.length > 0) {
      if (v.status === "unchanged") out.push(noteTexts.unchanged);
      else if (v.ignoreWhitespace) out.push(noteTexts.wsOnly);
      else if (out.length === 0) out.push(noteTexts.noTextChange);
    }
  }
  return out;
}
