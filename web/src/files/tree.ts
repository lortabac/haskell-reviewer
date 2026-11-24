import type { Entry } from "../api/files";
import type { Side } from "../sides";

export interface DirNode {
  name: string;
  // "" for the root.
  path: string;
  dirs: DirNode[];
  files: FileNode[];
}

// A path that is only on the other side of a diff: a link there
// (D-337).
export interface GhostEntry {
  path: string;
  kind: "ghost";
  // The side it is on.
  side: Side;
}

export type TreeEntry = Entry | GhostEntry;

export interface FileNode {
  name: string;
  entry: TreeEntry;
}

// Code point order, like the server. JS "<" goes by UTF-16 code units,
// which differs for characters above U+FFFF: at the first unit that
// differs, a surrogate (D800-DFFF) must sort after E000-FFFF.
// Well-formed strings only (no lone surrogates; the server never sends
// them, D-81).
export function compareCodePoints(a: string, b: string): number {
  if (a === b) return 0;
  const n = Math.min(a.length, b.length);
  for (let i = 0; i < n; i++) {
    const ca = a.charCodeAt(i);
    const cb = b.charCodeAt(i);
    if (ca !== cb) return unitKey(ca) - unitKey(cb);
  }
  return a.length - b.length;
}

function unitKey(c: number): number {
  if (c >= 0xe000) return c - 0x800;
  if (c >= 0xd800) return c + 0x2000;
  return c;
}

// The file tree's order of two names in one folder, the one rule
// buildTree and ] / [ (D-350) both use: a folder before a file, then
// code point order.
export function compareEntries(
  a: string,
  aDir: boolean,
  b: string,
  bDir: boolean,
): number {
  if (aDir !== bDir) return aDir ? -1 : 1;
  return compareCodePoints(a, b);
}

// Two paths in file tree order: at the first segment that differs (as
// name or as folder/file), compareEntries.
export function compareTreePaths(a: string, b: string): number {
  const pa = a.split("/");
  const pb = b.split("/");
  const n = Math.min(pa.length, pb.length);
  for (let i = 0; i < n; i++) {
    const d = compareEntries(
      pa[i] ?? "",
      i < pa.length - 1,
      pb[i] ?? "",
      i < pb.length - 1,
    );
    if (d !== 0) return d;
  }
  return pa.length - pb.length;
}

// Paths in file tree order: the order of a flatten of buildTree.
export function sortTreePaths(paths: Iterable<string>): string[] {
  const out: string[] = [];
  const walk = (node: DirNode) => {
    for (const d of node.dirs) walk(d);
    for (const f of node.files) out.push(f.entry.path);
  };
  walk(buildTree([...paths].map((path) => ({ path, kind: "file" }))));
  return out;
}

// The folder tree of a flat list. Folders first, then files, each in
// code point order (compareEntries).
export function buildTree(entries: readonly TreeEntry[]): DirNode {
  const root: DirNode = { name: "", path: "", dirs: [], files: [] };
  const dirs = new Map<string, DirNode>([["", root]]);
  for (const entry of entries) {
    const parts = entry.path.split("/");
    const name = parts.pop() ?? "";
    let node = root;
    for (const part of parts) {
      const path = node.path === "" ? part : `${node.path}/${part}`;
      let child = dirs.get(path);
      if (child === undefined) {
        child = { name: part, path, dirs: [], files: [] };
        dirs.set(path, child);
        node.dirs.push(child);
      }
      node = child;
    }
    node.files.push({ name, entry });
  }
  for (const node of dirs.values()) {
    node.dirs.sort((a, b) => compareEntries(a.name, true, b.name, true));
    node.files.sort((a, b) => compareEntries(a.name, false, b.name, false));
  }
  return root;
}

// No files at all.
export function isEmptyTree(root: DirNode): boolean {
  return root.dirs.length === 0 && root.files.length === 0;
}

// The folders that hold path: "a/b/c" -> ["a", "a/b"].
export function ancestors(path: string): string[] {
  const parts = path.split("/");
  const out: string[] = [];
  for (let i = 1; i < parts.length; i++) {
    out.push(parts.slice(0, i).join("/"));
  }
  return out;
}
