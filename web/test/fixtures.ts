// The API fixtures (fixtures/api, written by the Haskell tests), for the
// tests. See Note [API fixtures] in HaskellReviewer.Server.Wire.
import { readdirSync, readFileSync } from "node:fs";
import { join } from "node:path";
import { encode } from "cbor-x";
import { vi } from "vitest";

const dir = join(import.meta.dirname, "../../fixtures/api");
const names = readdirSync(dir);

function read(file: string): Uint8Array<ArrayBuffer> {
  return new Uint8Array(readFileSync(join(dir, file)));
}

// <name>.cbor: the bytes.
export const cborFixtures: ReadonlyMap<
  string,
  Uint8Array<ArrayBuffer>
> = new Map(
  names
    .filter((f) => f.endsWith(".cbor"))
    .map((f) => [f.replace(/\.cbor$/, ""), read(f)]),
);

// <name>.json, and the dump <name>.cbor.json of each CBOR fixture: the
// value as JSON.
export const fixtures: ReadonlyMap<string, unknown> = new Map(
  names
    .filter((f) => f.endsWith(".json"))
    .map((f) => [
      f.replace(/(\.cbor)?\.json$/, ""),
      JSON.parse(new TextDecoder().decode(read(f))) as unknown,
    ]),
);

export function fixture(name: string): unknown {
  if (!fixtures.has(name)) throw new Error(`no fixture ${name}`);
  return fixtures.get(name);
}

export function cborFixture(name: string): Uint8Array<ArrayBuffer> {
  const bytes = cborFixtures.get(name);
  if (bytes === undefined) throw new Error(`no CBOR fixture ${name}`);
  return bytes;
}

// A value as CBOR, for hand-made answers.
export function cbor(value: unknown): Uint8Array<ArrayBuffer> {
  return new Uint8Array(encode(value));
}

// A fake fetch: URL -> [status, body]. A Uint8Array body goes out as
// CBOR, anything else as JSON. Unknown URLs get a 404. Returns the
// mock, to check the calls.
export function stubFetch(routes: Record<string, [number, unknown]>) {
  const mock = vi.fn((input: RequestInfo | URL): Promise<Response> => {
    const url =
      typeof input === "string"
        ? input
        : input instanceof URL
          ? input.pathname
          : input.url;
    const [status, body] = routes[url] ?? [404, "not found"];
    return Promise.resolve(
      body instanceof Uint8Array
        ? new Response(new Uint8Array(body), {
            status,
            headers: { "Content-Type": "application/cbor" },
          })
        : new Response(JSON.stringify(body), {
            status,
            headers: { "Content-Type": "application/json" },
          }),
    );
  });
  vi.stubGlobal("fetch", mock);
  return mock;
}
