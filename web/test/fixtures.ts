// The API fixtures (fixtures/api, written by the Haskell tests), for the
// tests. See Note [API fixtures] in HaskellReviewer.Server.Wire.
import { vi } from "vitest";

const files = import.meta.glob<unknown>("../../fixtures/api/*.json", {
  eager: true,
  import: "default",
});

export const fixtures: ReadonlyMap<string, unknown> = new Map(
  Object.entries(files).map(([file, json]) => [
    file.replace(/^.*\//, "").replace(/\.json$/, ""),
    json,
  ]),
);

export function fixture(name: string): unknown {
  if (!fixtures.has(name)) throw new Error(`no fixture ${name}`);
  return fixtures.get(name);
}

// A fake fetch: URL -> [status, JSON body]. Unknown URLs get a 404.
// Returns the mock, to check the calls.
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
      new Response(JSON.stringify(body), {
        status,
        headers: { "Content-Type": "application/json" },
      }),
    );
  });
  vi.stubGlobal("fetch", mock);
  return mock;
}
