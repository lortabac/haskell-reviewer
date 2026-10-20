import { basePath } from "../base";

export type Result<T> =
  | { kind: "ok"; value: T }
  | { kind: "not-found" }
  | { kind: "error"; message: string };

// GET a JSON API route and decode it. path is relative to the token,
// like "/api/files/tree". Never throws.
export async function getJson<T>(
  path: string,
  decode: (value: unknown) => T,
): Promise<Result<T>> {
  try {
    const res = await fetch(basePath(window.location.pathname) + path, {
      headers: { Accept: "application/json" },
    });
    if (res.status === 404) return { kind: "not-found" };
    if (!res.ok) return { kind: "error", message: `HTTP ${res.status}` };
    return { kind: "ok", value: decode(await res.json()) };
  } catch (e) {
    return { kind: "error", message: String(e) };
  }
}
