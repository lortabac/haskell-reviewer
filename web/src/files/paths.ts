// File paths in URLs. Each segment is encoded on its own, so "/" stays a
// separator and "#", "?" and "%" in names stay part of the name.

export function encodePath(path: string): string {
  return path.split("/").map(encodeURIComponent).join("/");
}

// The file view URL, relative to the token.
export function hrefFor(path: string): string {
  return "/src/tree/" + encodePath(path);
}

// The file API URL, relative to the token.
export function apiFileUrl(path: string): string {
  return "/api/file/tree/" + encodePath(path);
}

// The file path in a full pathname "/<token>/src/tree/<path>". Undefined
// if it is not a file view URL or the path is bad: an empty segment, ".",
// "..", a "/" inside a segment, or broken percent encoding.
export function pathFromPathname(pathname: string): string | undefined {
  const [, , src, tree, ...rest] = pathname.split("/");
  if (src !== "src" || tree !== "tree" || rest.length === 0) return undefined;
  const segments: string[] = [];
  for (const raw of rest) {
    let s: string;
    try {
      s = decodeURIComponent(raw);
    } catch {
      return undefined;
    }
    if (s === "" || s === "." || s === ".." || s.includes("/")) {
      return undefined;
    }
    segments.push(s);
  }
  return segments.join("/");
}

// base + rel, where base is the router base ("/<token>"). A base of "/"
// or "" gives rel as is, never "//src/...", which would be a host.
export function withBase(base: string | undefined, rel: string): string {
  return (base ?? "").replace(/\/+$/, "") + rel;
}
