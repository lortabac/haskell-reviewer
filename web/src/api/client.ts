import { decode as decodeCbor } from "cbor-x/decode-no-eval";
import { basePath } from "../base";
import { DecodeError } from "./decode";

export type Result<T> =
  | { kind: "ok"; value: T }
  | { kind: "not-found" }
  | { kind: "error"; message: string };

// GET a JSON API route and decode it. path is relative to the token,
// like "/api/files/tree". Never throws. signal: abort the fetch (e.g. a
// newer search makes an older one moot); an aborted fetch just comes
// back as an "error" result, so callers that pass a signal must also
// drop stale answers themselves (a sequence number, or the echoed
// query).
export async function getJson<T>(
  path: string,
  decode: (value: unknown) => T,
  opts?: { signal?: AbortSignal },
): Promise<Result<T>> {
  return get(
    path,
    "application/json",
    async (res) => decode(await res.json()),
    opts?.signal,
  );
}

// POST a JSON body to a route and decode the answer, like getJson.
// Default (cors) mode, so the browser sends our Origin (D-198): never
// "same-origin" or "no-cors", never a <form>.
export async function postJson<T>(
  path: string,
  body: unknown,
  decode: (value: unknown) => T,
): Promise<Result<T>> {
  try {
    const res = await fetch(basePath(window.location.pathname) + path, {
      method: "POST",
      headers: { "Content-Type": "application/json" },
      body: JSON.stringify(body),
    });
    if (!res.ok) return { kind: "error", message: `HTTP ${res.status}` };
    return { kind: "ok", value: decode(await res.json()) };
  } catch (e) {
    return { kind: "error", message: String(e) };
  }
}

// GET a CBOR API route (D-51) and decode it, like getJson.
export async function getCbor<T>(
  path: string,
  decode: (value: unknown) => T,
): Promise<Result<T>> {
  return get(path, "application/cbor", async (res) =>
    decode(fromCbor(new Uint8Array(await res.arrayBuffer()))),
  );
}

// One CBOR item as the plain values JSON would give: objects, arrays,
// strings, safe integers (up to 2^53 - 1), booleans, null. Anything
// else (a Map, a tag, a non-integral or too big number, bytes,
// undefined) is a DecodeError: the server never sends it. A float with
// a whole value comes out as an integer: can't tell, doesn't matter.
// See Note [CBOR bodies] in HaskellReviewer.Server.Cbor. Map keys that
// are not text come out as strings (cbor-x does that), so they are not
// caught here: the Haskell tests check the server never sends them.
export function fromCbor(bytes: Uint8Array): unknown {
  const value: unknown = decodeCbor(bytes);
  checkPlain(value, "$");
  return value;
}

function checkPlain(value: unknown, path: string): void {
  if (value === null) return;
  switch (typeof value) {
    case "string":
    case "boolean":
      return;
    case "number":
      if (!Number.isSafeInteger(value)) {
        throw new DecodeError(path, "a safe integer");
      }
      return;
    case "object":
      if (Array.isArray(value)) {
        value.forEach((v, i) => checkPlain(v, `${path}[${i}]`));
        return;
      }
      if (Object.getPrototypeOf(value) === Object.prototype) {
        for (const [k, v] of Object.entries(value)) {
          checkPlain(v, `${path}.${k}`);
        }
        return;
      }
  }
  throw new DecodeError(path, "a JSON-like value");
}

async function get<T>(
  path: string,
  accept: string,
  read: (res: Response) => Promise<T>,
  signal?: AbortSignal,
): Promise<Result<T>> {
  try {
    const res = await fetch(basePath(window.location.pathname) + path, {
      headers: { Accept: accept },
      signal,
    });
    if (res.status === 404) return { kind: "not-found" };
    if (!res.ok) return { kind: "error", message: `HTTP ${res.status}` };
    return { kind: "ok", value: await read(res) };
  } catch (e) {
    return { kind: "error", message: String(e) };
  }
}
