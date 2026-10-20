import type { Entry } from "../api/files";

export interface DirNode {
  name: string;
  // "" for the root.
  path: string;
  dirs: DirNode[];
  files: FileNode[];
}

export interface FileNode {
  name: string;
  entry: Entry;
}

// Code point order, like the server. JS "<" goes by UTF-16 code units,
// which differs for characters above U+FFFF.
export function compareCodePoints(a: string, b: string): number {
  const ia = a[Symbol.iterator]();
  const ib = b[Symbol.iterator]();
  for (;;) {
    const ca = ia.next();
    const cb = ib.next();
    if (ca.done === true) return cb.done === true ? 0 : -1;
    if (cb.done === true) return 1;
    const d = (ca.value.codePointAt(0) ?? 0) - (cb.value.codePointAt(0) ?? 0);
    if (d !== 0) return d;
  }
}

// The folder tree of a flat list. Folders first, then files, each in
// code point order.
export function buildTree(entries: readonly Entry[]): DirNode {
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
    node.dirs.sort((a, b) => compareCodePoints(a.name, b.name));
    node.files.sort((a, b) => compareCodePoints(a.name, b.name));
  }
  return root;
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
