// File markers for the sidebar in diff mode (D-337), from
// api/diff-files. Pure.
import type { DiffFiles } from "../api/diffFiles";
import type { Entry } from "../api/files";
import { otherSide, type Side } from "../sides";
import type { DirNode, FileNode, GhostEntry, TreeEntry } from "./tree";

export type FileMark =
  | { status: "added" | "removed" | "modified" }
  // other: the path on the other side.
  | { status: "renamed"; other: string; similarity: number | null };

export interface FileMarks {
  // By this side's path.
  marks: ReadonlyMap<string, FileMark>;
  // Files only on the other side, by path.
  ghosts: readonly GhostEntry[];
}

// Each side keys its marks by its own path: on base a rename is under
// its old path. A removed file is a ghost on head, an added one on base.
export function fileMarks(diff: DiffFiles, side: Side): FileMarks {
  const marks = new Map<string, FileMark>();
  const ghosts: GhostEntry[] = [];
  const other = otherSide(side);
  if (other === undefined) return { marks, ghosts };
  for (const f of diff.files) {
    switch (f.status) {
      case "modified":
        marks.set(f.path, { status: f.status });
        break;
      case "renamed":
        if (f.oldPath === null) {
          // Not sent by the server; treat it as an edit.
          marks.set(f.path, { status: "modified" });
        } else if (side === "head") {
          marks.set(f.path, {
            status: f.status,
            other: f.oldPath,
            similarity: f.similarity,
          });
        } else {
          marks.set(f.oldPath, {
            status: f.status,
            other: f.path,
            similarity: f.similarity,
          });
        }
        break;
      // Added: on head only. Removed: on base only.
      case "added":
        if (side === "head") marks.set(f.path, { status: f.status });
        else ghosts.push({ path: f.path, kind: "ghost", side: other });
        break;
      case "removed":
        if (side === "base") marks.set(f.path, { status: f.status });
        else ghosts.push({ path: f.path, kind: "ghost", side: other });
        break;
    }
  }
  // diff.files is sorted by path, so ghosts are too.
  return { marks, ghosts };
}

// The side's entries plus its ghosts. A ghost at a path the side has
// (a list cut at 100,000 files, say) is dropped: one entry per path.
export function unionEntries(
  entries: readonly Entry[],
  ghosts: readonly GhostEntry[],
): TreeEntry[] {
  if (ghosts.length === 0) return [...entries];
  const have = new Set(entries.map((e) => e.path));
  return [...entries, ...ghosts.filter((g) => !have.has(g.path))];
}

// "Changed only" for the file tree: which files and folders stay. The
// tree decides as it renders (with the current file kept too), so its
// nodes stay the same objects and nothing mounts again on a navigation.
export interface TreeFilter {
  keep: (file: FileNode) => boolean;
  // Folders with a kept file under them.
  dirs: ReadonlySet<string>;
  // How many files are kept.
  count: number;
}

// Ghosts and marked files stay.
export function changedFilter(
  root: DirNode,
  marks: ReadonlyMap<string, FileMark>,
): TreeFilter {
  const keep = (f: FileNode): boolean =>
    f.entry.kind === "ghost" || marks.has(f.entry.path);
  const dirs = new Set<string>();
  // How many kept files are under the folder.
  const walk = (node: DirNode): number => {
    let n = node.files.filter(keep).length;
    for (const d of node.dirs) n += walk(d);
    if (n > 0) dirs.add(node.path);
    return n;
  };
  return { keep, dirs, count: walk(root) };
}

// What a rename row says: "renamed from a (90%)" on head, "renamed to b
// (90%)" on base.
export function renameText(
  mark: Extract<FileMark, { status: "renamed" }>,
  side: Side,
): string {
  const way = side === "base" ? "to" : "from";
  const pct = mark.similarity === null ? "" : ` (${mark.similarity}%)`;
  return `renamed ${way} ${mark.other}${pct}`;
}
