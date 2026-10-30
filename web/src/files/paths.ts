// File paths in URLs. Each segment is encoded on its own, so "/" stays a
// separator and "#", "?" and "%" in names stay part of the name.

export function encodePath(path: string): string {
  return path.split("/").map(encodeURIComponent).join("/");
}

// The file view URL, relative to the token.
export function hrefFor(path: string): string {
  return "/src/tree/" + encodePath(path);
}

// The file view URL at a line, relative to the token.
export function lineHref(path: string, line: number): string {
  return hrefFor(path) + "#L" + String(line);
}

// The file API URL, relative to the token.
export function apiFileUrl(path: string): string {
  return "/api/file/tree/" + encodePath(path);
}

// The decoded segments of "/<token>/<prefix…>/<rest…>" after prefix.
// Undefined if the prefix doesn't match, there is no rest, or a
// segment is bad: empty, ".", "..", a "/" inside, or broken percent
// encoding.
export function segmentsAfter(
  pathname: string,
  prefix: string[],
): string[] | undefined {
  const [, , ...parts] = pathname.split("/");
  if (prefix.some((p, i) => parts[i] !== p)) return undefined;
  const rest = parts.slice(prefix.length);
  if (rest.length === 0) return undefined;
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
  return segments;
}

// The file path in a full pathname "/<token>/src/tree/<path>". Undefined
// if it is not a file view URL or the path is bad (segmentsAfter).
export function pathFromPathname(pathname: string): string | undefined {
  return segmentsAfter(pathname, ["src", "tree"])?.join("/");
}

// Lines to mark, from a hash like "#L3" or "#L3-L7". Undefined for
// anything else, line 0, or a range that goes backwards.
export function parseLineHash(
  hash: string,
): { from: number; to: number } | undefined {
  const m = /^#L([1-9]\d{0,8})(?:-L([1-9]\d{0,8}))?$/.exec(hash);
  if (m === null) return undefined;
  const from = Number(m[1]);
  const to = m[2] === undefined ? from : Number(m[2]);
  return to < from ? undefined : { from, to };
}

// base + rel, where base is the router base ("/<token>"). A base of "/"
// or "" gives rel as is, never "//src/...", which would be a host.
export function withBase(base: string | undefined, rel: string): string {
  return (base ?? "").replace(/\/+$/, "") + rel;
}
