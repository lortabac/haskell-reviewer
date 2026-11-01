// The search box: server search (D-207), debounced, keyboard-usable
// (D-209), refreshed while partial (D-210). Most tests mount SearchBox
// directly behind a minimal Router (it needs no Prefs/Modules
// context); a few mount the whole app to check the NotFound page
// actually uses it (placement="page").
import { Route, Router } from "@solidjs/router";
import { createSignal } from "solid-js";
import { render } from "solid-js/web";
import { afterEach, describe, expect, it, vi } from "vitest";
import { cborFixture, fixture, stubFetch } from "../../test/fixtures";
import { mountAppAt, unmountApp } from "../../test/mount";
import type { Indexing } from "../api/events";
import type { Search } from "../api/search";
import { IndexingContext } from "../events/indexing";
import { SearchBox } from "./SearchBox";

let dispose: (() => void) | undefined;

function mount(
  props: { initialQuery?: string; placement: "header" | "page" },
  path = "/tok/x",
): { root: HTMLElement; setIndexing: (i: Indexing | undefined) => void } {
  window.history.replaceState(null, "", path);
  const root = document.createElement("div");
  document.body.append(root);
  const [indexing, setIndexing] = createSignal<Indexing>();
  dispose = render(
    () => (
      <Router
        base="/tok"
        root={(p) => (
          <IndexingContext.Provider value={{ indexing }}>
            <SearchBox {...props} />
            {p.children}
          </IndexingContext.Provider>
        )}
      >
        <Route path="*" component={() => <></>} />
      </Router>
    ),
    root,
  );
  return { root, setIndexing };
}

afterEach(() => {
  dispose?.();
  dispose = undefined;
  unmountApp();
  document.body.replaceChildren();
  vi.unstubAllGlobals();
  vi.useRealTimers();
});

function input(c: ParentNode): HTMLInputElement {
  const i = c.querySelector<HTMLInputElement>("input[role=combobox]");
  if (i === null) throw new Error("no input");
  return i;
}

function type(c: ParentNode, text: string): void {
  const i = input(c);
  i.value = text;
  i.dispatchEvent(new Event("input", { bubbles: true }));
}

function key(k: string): KeyboardEvent {
  return new KeyboardEvent("keydown", {
    key: k,
    bubbles: true,
    cancelable: true,
  });
}

function options(c: ParentNode): HTMLAnchorElement[] {
  return [...c.querySelectorAll<HTMLAnchorElement>('a[role="option"]')];
}

function status(c: ParentNode): string {
  return c.querySelector(".search-status")?.textContent ?? "";
}

function jsonResponse(status: number, body: unknown): Response {
  return new Response(JSON.stringify(body), {
    status,
    headers: { "Content-Type": "application/json" },
  });
}

function urlOf(req: RequestInfo | URL): string {
  return typeof req === "string"
    ? req
    : req instanceof URL
      ? req.href
      : req.url;
}

// A fake search endpoint that always echoes the query it was asked
// (as the real one does), as a single module result named after it.
function echoModule(req: RequestInfo | URL): Promise<Response> {
  const u = urlOf(req);
  const q = decodeURIComponent(u.split("q=")[1] ?? "");
  return Promise.resolve(
    jsonResponse(200, {
      generation: 1,
      query: q,
      partial: false,
      matches: 1,
      results: [{ kind: "module", name: q, ref: q }],
    }),
  );
}

describe("SearchBox", () => {
  it("a combobox, closed at mount: no popup attributes, sr-only label in the header", () => {
    const { root } = mount({ placement: "header" });
    const i = input(root);
    expect(i.getAttribute("role")).toBe("combobox");
    expect(i.getAttribute("aria-autocomplete")).toBe("list");
    expect(i.getAttribute("aria-expanded")).toBe("false");
    expect(i.hasAttribute("aria-controls")).toBe(false);
    expect(i.hasAttribute("aria-activedescendant")).toBe(false);
    expect(i.maxLength).toBe(128);
    expect(i.closest("form")).toBeNull();
    const label = root.querySelector(`label[for="${i.id}"]`);
    expect(label?.textContent).toBe("Search modules and names");
    expect(label?.className).toContain("sr-only");
  });

  it("the page placement shows a visible label and the list whenever there's text", () => {
    const { root } = mount({ placement: "page" });
    const label = root.querySelector("label");
    expect(label?.className ?? "").not.toContain("sr-only");
    expect(input(root).getAttribute("aria-expanded")).toBe("false");
  });

  it("debounces: one fetch after 120 ms of quiet; empty never fetches", async () => {
    vi.useFakeTimers();
    const fetchMock = stubFetch({
      "/tok/api/search/tree?q=m": [
        200,
        { generation: 1, query: "m", partial: false, matches: 0, results: [] },
      ],
      "/tok/api/search/tree?q=ma": [
        200,
        { generation: 1, query: "ma", partial: false, matches: 0, results: [] },
      ],
      "/tok/api/search/tree?q=map": [200, fixture("search")],
    });
    const { root } = mount({ placement: "header" });
    const searchCalls = () =>
      fetchMock.mock.calls
        .map(([u]) =>
          typeof u === "string" ? u : u instanceof URL ? u.href : u.url,
        )
        .filter((u) => u.includes("api/search"));
    type(root, "m");
    await vi.advanceTimersByTimeAsync(50);
    type(root, "ma");
    await vi.advanceTimersByTimeAsync(50);
    type(root, "map");
    expect(searchCalls()).toEqual([]);
    await vi.advanceTimersByTimeAsync(120);
    expect(searchCalls()).toEqual(["/tok/api/search/tree?q=map"]);
    const search = fixture("search") as Search;
    expect(options(root)).toHaveLength(search.results.length);
    type(root, "");
    await vi.advanceTimersByTimeAsync(200);
    expect(searchCalls()).toEqual(["/tok/api/search/tree?q=map"]);
    expect(options(root)).toEqual([]);
  });

  it("a prefilled query (a page notice) fetches at once, no debounce", async () => {
    stubFetch({
      "/tok/api/search/tree?q=Nope": [
        200,
        {
          generation: 1,
          query: "Nope",
          partial: false,
          matches: 0,
          results: [],
        },
      ],
    });
    mount({ placement: "page", initialQuery: "Nope" });
    await vi.waitFor(() => {
      expect(status(document.body)).toBe("No results.");
    });
  });

  it("a stale answer (out of order) never overwrites a newer query's results", async () => {
    vi.useFakeTimers();
    const search = fixture("search") as Search;
    const fetchMock = vi.fn((req: RequestInfo | URL): Promise<Response> => {
      const url =
        typeof req === "string"
          ? req
          : req instanceof URL
            ? req.pathname
            : req.url;
      if (url.endsWith("q=ma")) {
        return new Promise((resolve) => {
          setTimeout(
            () =>
              resolve(
                jsonResponse(200, {
                  generation: 1,
                  query: "ma",
                  partial: false,
                  matches: 0,
                  results: [],
                }),
              ),
            1000,
          );
        });
      }
      if (url.endsWith("q=map"))
        return Promise.resolve(jsonResponse(200, search));
      return Promise.resolve(jsonResponse(404, "not found"));
    });
    vi.stubGlobal("fetch", fetchMock);
    const { root } = mount({ placement: "header" });
    type(root, "ma");
    await vi.advanceTimersByTimeAsync(120);
    type(root, "map");
    await vi.advanceTimersByTimeAsync(120);
    expect(options(root)).toHaveLength(search.results.length);
    // The slow "ma" answer finally arrives: too late, dropped.
    await vi.advanceTimersByTimeAsync(1000);
    expect(options(root)).toHaveLength(search.results.length);
  });

  it("ArrowDown/Up move the active option, no wrap; Escape closes, then clears", async () => {
    vi.useFakeTimers();
    stubFetch({ "/tok/api/search/tree?q=map": [200, fixture("search")] });
    const { root } = mount({ placement: "header" });
    const i = input(root);
    type(root, "map");
    await vi.advanceTimersByTimeAsync(120);
    expect(i.getAttribute("aria-expanded")).toBe("true");
    expect(i.getAttribute("aria-controls")).not.toBeNull();
    const opts = options(root);
    expect(opts.length).toBeGreaterThan(2);

    i.dispatchEvent(key("ArrowDown"));
    expect(i.getAttribute("aria-activedescendant")).toBe(opts[0]?.id);
    i.dispatchEvent(key("ArrowDown"));
    expect(i.getAttribute("aria-activedescendant")).toBe(opts[1]?.id);
    i.dispatchEvent(key("ArrowUp"));
    expect(i.getAttribute("aria-activedescendant")).toBe(opts[0]?.id);
    // No wrap: one more Up clears the active option (back to the input).
    i.dispatchEvent(key("ArrowUp"));
    expect(i.hasAttribute("aria-activedescendant")).toBe(false);

    // Escape closes the popup but keeps the text.
    i.dispatchEvent(key("Escape"));
    expect(i.getAttribute("aria-expanded")).toBe("false");
    expect(i.value).toBe("map");
    // Escape again (already closed): clears the text.
    i.dispatchEvent(key("Escape"));
    expect(input(root).value).toBe("");
  });

  it("Enter with nothing active opens the first result, closes the popup, keeps the text", async () => {
    vi.useFakeTimers();
    stubFetch({ "/tok/api/search/tree?q=map": [200, fixture("search")] });
    const { root } = mount({ placement: "header" });
    const i = input(root);
    type(root, "map");
    await vi.advanceTimersByTimeAsync(120);
    const search = fixture("search") as Search;
    const first = search.results[0];
    if (first === undefined || first.kind !== "decl") {
      throw new Error("expected a decl first");
    }
    i.dispatchEvent(key("Enter"));
    // navigate() (solid-router) commits asynchronously.
    await vi.advanceTimersByTimeAsync(0);
    expect(window.location.pathname).toBe(`/tok/docs/tree/${first.ref}`);
    expect(window.location.hash).toBe(`#${encodeURIComponent(first.anchor)}`);
    expect(i.getAttribute("aria-expanded")).toBe("false");
    expect(i.value).toBe("map");
  });

  it('caps at 50, "Showing 50 of N"', async () => {
    vi.useFakeTimers();
    const results = Array.from({ length: 50 }, (_, n) => ({
      kind: "module" as const,
      name: `M${n}`,
      ref: `M${n}`,
    }));
    stubFetch({
      "/tok/api/search/tree?q=m": [
        200,
        { generation: 1, query: "m", partial: false, matches: 60, results },
      ],
    });
    const { root } = mount({ placement: "page" });
    type(root, "m");
    await vi.advanceTimersByTimeAsync(120);
    expect(status(root)).toBe("Showing 50 of 60.");
    expect(options(root)).toHaveLength(50);
  });

  it("partial footer; refetch on indexing change is throttled (even the first one, C6), and always happens at done", async () => {
    vi.useFakeTimers();
    const partial = {
      generation: 1,
      query: "map",
      partial: true,
      matches: 1,
      results: [{ kind: "module", name: "Data.Map", ref: "Data.Map" }],
    };
    const full = fixture("search");
    let n = 0;
    const fetchMock = vi.fn(() => {
      n++;
      return Promise.resolve(jsonResponse(200, n < 3 ? partial : full));
    });
    vi.stubGlobal("fetch", fetchMock);
    const { root, setIndexing } = mount({ placement: "page" });
    type(root, "map");
    await vi.advanceTimersByTimeAsync(120);
    expect(fetchMock).toHaveBeenCalledTimes(1);
    expect(root.querySelector(".search-partial")?.textContent).toContain(
      "Partial results, still indexing",
    );

    // Right after the search's own first answer: throttled (C6), even
    // though this is the very first indexing-triggered attempt (the
    // throttle used to start counting from 0, so this one always
    // fired at once).
    setIndexing({ generation: 1, total: 10, ready: 5, failed: 0 });
    await vi.advanceTimersByTimeAsync(10);
    expect(fetchMock).toHaveBeenCalledTimes(1);

    // Once the throttle window has passed: refetches.
    await vi.advanceTimersByTimeAsync(1000);
    setIndexing({ generation: 1, total: 10, ready: 6, failed: 0 });
    await vi.advanceTimersByTimeAsync(10);
    expect(fetchMock).toHaveBeenCalledTimes(2);

    // Right after that one: throttled again.
    setIndexing({ generation: 1, total: 10, ready: 7, failed: 0 });
    await vi.advanceTimersByTimeAsync(10);
    expect(fetchMock).toHaveBeenCalledTimes(2);

    // Done: always refetches, throttle or not.
    setIndexing({ generation: 1, total: 10, ready: 10, failed: 0 });
    await vi.advanceTimersByTimeAsync(10);
    expect(fetchMock).toHaveBeenCalledTimes(3);
    expect(root.querySelector(".search-partial")).toBeNull();
  });

  it("the status announces on a new query and on partial -> full, not every throttled refresh", async () => {
    vi.useFakeTimers();
    const partial = {
      generation: 1,
      query: "map",
      partial: true,
      matches: 1,
      results: [{ kind: "module", name: "Data.Map", ref: "Data.Map" }],
    };
    const full = {
      generation: 1,
      query: "map",
      partial: false,
      matches: 2,
      results: [
        { kind: "module", name: "Data.Map", ref: "Data.Map" },
        { kind: "module", name: "Data.IntMap", ref: "Data.IntMap" },
      ],
    };
    let n = 0;
    const fetchMock = vi.fn(() => {
      n++;
      return Promise.resolve(jsonResponse(200, n === 1 ? partial : full));
    });
    vi.stubGlobal("fetch", fetchMock);
    const { root, setIndexing } = mount({ placement: "page" });
    type(root, "map");
    await vi.advanceTimersByTimeAsync(120);
    expect(status(root)).toBe("1 result. Partial: still indexing.");
    setIndexing({ generation: 1, total: 1, ready: 1, failed: 0 });
    await vi.advanceTimersByTimeAsync(10);
    expect(fetchMock).toHaveBeenCalledTimes(2);
    expect(status(root)).toBe("2 results.");
  });

  it("keeps the active option across a partial refresh, by key not index", async () => {
    vi.useFakeTimers();
    const a = {
      kind: "module" as const,
      name: "Data.IntMap",
      ref: "Data.IntMap",
    };
    const b = { kind: "module" as const, name: "Data.Map", ref: "Data.Map" };
    let n = 0;
    const fetchMock = vi.fn(() => {
      n++;
      const results = n === 1 ? [a, b] : [b, a];
      return Promise.resolve(
        jsonResponse(200, {
          generation: 1,
          query: "map",
          partial: n === 1,
          matches: 2,
          results,
        }),
      );
    });
    vi.stubGlobal("fetch", fetchMock);
    const { root, setIndexing } = mount({ placement: "page" });
    const i = input(root);
    type(root, "map");
    await vi.advanceTimersByTimeAsync(120);
    i.dispatchEvent(key("ArrowDown"));
    i.dispatchEvent(key("ArrowDown"));
    expect(options(root)[1]?.textContent).toContain("Data.Map");
    expect(i.getAttribute("aria-activedescendant")).toBe(options(root)[1]?.id);

    setIndexing({ generation: 1, total: 2, ready: 2, failed: 0 });
    await vi.advanceTimersByTimeAsync(10);
    expect(fetchMock).toHaveBeenCalledTimes(2);
    expect(options(root)[0]?.textContent).toContain("Data.Map");
    expect(i.getAttribute("aria-activedescendant")).toBe(options(root)[0]?.id);
  });
});

describe("SearchBox — review round 1 fixes", () => {
  it("C1: Enter right after typing waits for its own query's answer, not the previous one's", async () => {
    vi.useFakeTimers();
    vi.stubGlobal("fetch", vi.fn(echoModule));
    const { root } = mount({ placement: "header" });
    const i = input(root);
    type(root, "Other");
    await vi.advanceTimersByTimeAsync(120);
    expect(options(root)).toHaveLength(1);
    // Type a different query and press Enter at once: the debounce
    // for "Perimeter" hasn't even started fetching yet, so "Other" is
    // still all that's on screen.
    type(root, "Perimeter");
    i.dispatchEvent(key("Enter"));
    await vi.advanceTimersByTimeAsync(130);
    expect(window.location.pathname).toBe("/tok/docs/tree/Perimeter");
  });

  it("C1: Enter while the new query's fetch is already in flight also waits for it", async () => {
    vi.useFakeTimers();
    let resolveSecond: ((r: Response) => void) | undefined;
    const fetchMock = vi.fn((req: RequestInfo | URL) => {
      const u = urlOf(req);
      if (u.includes("q=Other")) return echoModule(req);
      return new Promise<Response>((resolve) => {
        resolveSecond = resolve;
      }).then(() => echoModule(req));
    });
    vi.stubGlobal("fetch", fetchMock);
    const { root } = mount({ placement: "header" });
    const i = input(root);
    type(root, "Other");
    await vi.advanceTimersByTimeAsync(120);
    expect(options(root)).toHaveLength(1);
    type(root, "Perimeter");
    await vi.advanceTimersByTimeAsync(120); // the fetch for "Perimeter" has started…
    i.dispatchEvent(key("Enter")); // …but hasn't resolved yet.
    await vi.advanceTimersByTimeAsync(0);
    expect(window.location.pathname).toBe("/tok/x"); // not yet
    resolveSecond?.(jsonResponse(200, {})); // the mock's .then(echoModule) ignores this value
    await vi.advanceTimersByTimeAsync(10);
    expect(window.location.pathname).toBe("/tok/docs/tree/Perimeter");
  });

  it("C1: typing again cancels the pending Enter; it never fires late", async () => {
    vi.useFakeTimers();
    vi.stubGlobal("fetch", vi.fn(echoModule));
    const { root } = mount({ placement: "header" });
    const i = input(root);
    type(root, "Other");
    await vi.advanceTimersByTimeAsync(120);
    type(root, "Perimeter");
    i.dispatchEvent(key("Enter")); // pending on "Perimeter"
    type(root, "PerimeterX"); // changed their mind before it landed
    await vi.advanceTimersByTimeAsync(130);
    // Never silently opened "Perimeter": still here, showing its own
    // (also fresh) answer.
    expect(window.location.pathname).toBe("/tok/x");
    expect(options(root)).toHaveLength(1);
    expect(options(root)[0]?.textContent).toContain("PerimeterX");
  });

  it("C2: Enter does nothing while the header popup is closed", async () => {
    vi.useFakeTimers();
    vi.stubGlobal("fetch", vi.fn(echoModule));
    const { root } = mount({ placement: "header" });
    const i = input(root);
    type(root, "Other");
    await vi.advanceTimersByTimeAsync(120);
    i.dispatchEvent(key("Escape")); // closes the popup, keeps the text
    expect(i.getAttribute("aria-expanded")).toBe("false");
    i.dispatchEvent(key("Enter"));
    await vi.advanceTimersByTimeAsync(130);
    expect(window.location.pathname).toBe("/tok/x");

    // Choosing closes the popup too; Enter again does nothing either.
    type(root, "Other");
    await vi.advanceTimersByTimeAsync(130);
    i.dispatchEvent(key("Enter"));
    await vi.advanceTimersByTimeAsync(0);
    expect(window.location.pathname).toBe("/tok/docs/tree/Other");
    i.dispatchEvent(key("Enter"));
    await vi.advanceTimersByTimeAsync(130);
    expect(window.location.pathname).toBe("/tok/docs/tree/Other"); // unchanged
  });

  it('C4: an error clears stale results and shows "Search failed."', async () => {
    vi.useFakeTimers();
    const fetchMock = vi.fn((req: RequestInfo | URL) =>
      urlOf(req).endsWith("q=map")
        ? Promise.resolve(jsonResponse(200, fixture("search")))
        : Promise.resolve(new Response("boom", { status: 500 })),
    );
    vi.stubGlobal("fetch", fetchMock);
    const { root } = mount({ placement: "page" });
    type(root, "map");
    await vi.advanceTimersByTimeAsync(130);
    expect(options(root).length).toBeGreaterThan(0);
    type(root, "mapz");
    await vi.advanceTimersByTimeAsync(130);
    expect(options(root)).toEqual([]);
    expect(status(root)).toBe("Search failed.");
  });

  it("C4: a too-long prefilled query is capped, not sent as-is (would 400)", async () => {
    const fetchMock = vi.fn(echoModule);
    vi.stubGlobal("fetch", fetchMock);
    const long = "x".repeat(200);
    mount({ placement: "page", initialQuery: long });
    await vi.waitFor(() => {
      expect(fetchMock).toHaveBeenCalled();
    });
    const u = urlOf(fetchMock.mock.calls[0]?.[0] as RequestInfo | URL);
    const sent = decodeURIComponent(u.split("q=")[1] ?? "");
    expect(sent).toHaveLength(128);
    expect(sent).toBe("x".repeat(128));
  });

  it("Escape on the page box clears the text (there's no popup to close first)", async () => {
    vi.useFakeTimers();
    stubFetch({ "/tok/api/search/tree?q=map": [200, fixture("search")] });
    const { root } = mount({ placement: "page" });
    const i = input(root);
    type(root, "map");
    await vi.advanceTimersByTimeAsync(120);
    expect(options(root).length).toBeGreaterThan(0);
    i.dispatchEvent(key("Escape"));
    expect(input(root).value).toBe("");
    expect(options(root)).toEqual([]);
  });

  it("two boxes on one page get distinct ids (no clash)", () => {
    const root = document.createElement("div");
    document.body.append(root);
    window.history.replaceState(null, "", "/tok/x");
    dispose = render(
      () => (
        <Router
          base="/tok"
          root={() => (
            <>
              <SearchBox placement="header" />
              <SearchBox placement="page" />
            </>
          )}
        >
          <Route path="*" component={() => <></>} />
        </Router>
      ),
      root,
    );
    const inputs = [
      ...root.querySelectorAll<HTMLInputElement>("input[role=combobox]"),
    ];
    expect(inputs).toHaveLength(2);
    expect(inputs[0]?.id).not.toBe(inputs[1]?.id);
    expect(new Set(inputs.map((i) => i.id)).size).toBe(2);
  });

  it("focusout to outside the box closes the header popup", async () => {
    vi.useFakeTimers();
    stubFetch({ "/tok/api/search/tree?q=map": [200, fixture("search")] });
    const { root } = mount({ placement: "header" });
    const i = input(root);
    type(root, "map");
    await vi.advanceTimersByTimeAsync(120);
    expect(i.getAttribute("aria-expanded")).toBe("true");
    const outside = document.createElement("button");
    document.body.append(outside);
    i.dispatchEvent(
      new FocusEvent("focusout", { bubbles: true, relatedTarget: outside }),
    );
    expect(i.getAttribute("aria-expanded")).toBe("false");
  });

  it("a focusout to a result link inside the popup keeps it open", async () => {
    vi.useFakeTimers();
    stubFetch({ "/tok/api/search/tree?q=map": [200, fixture("search")] });
    const { root } = mount({ placement: "header" });
    const i = input(root);
    type(root, "map");
    await vi.advanceTimersByTimeAsync(120);
    const a = options(root)[0];
    if (a === undefined) throw new Error("no option");
    i.dispatchEvent(
      new FocusEvent("focusout", { bubbles: true, relatedTarget: a }),
    );
    expect(i.getAttribute("aria-expanded")).toBe("true");
  });

  it("a route change closes the header popup", async () => {
    vi.useFakeTimers();
    stubFetch({ "/tok/api/search/tree?q=map": [200, fixture("search")] });
    const { root } = mount({ placement: "header" });
    type(root, "map");
    await vi.advanceTimersByTimeAsync(120);
    expect(input(root).getAttribute("aria-expanded")).toBe("true");
    window.history.pushState(null, "", "/tok/elsewhere");
    window.dispatchEvent(new PopStateEvent("popstate"));
    await vi.advanceTimersByTimeAsync(0);
    expect(input(root).getAttribute("aria-expanded")).toBe("false");
  });

  it('labels: "field of T" and "private"', async () => {
    vi.useFakeTimers();
    stubFetch({ "/tok/api/search/tree?q=map": [200, fixture("search")] });
    const { root } = mount({ placement: "page" });
    type(root, "map");
    await vi.advanceTimersByTimeAsync(120);
    const search = fixture("search") as Search;
    const field = search.results.find(
      (r) => r.kind === "decl" && r.ns === "field",
    );
    const priv = search.results.find((r) => r.kind === "decl" && !r.exported);
    if (field === undefined || field.kind !== "decl" || field.parent === null) {
      throw new Error("fixture has no field result");
    }
    if (priv === undefined) throw new Error("fixture has no private result");
    const fieldRow = options(root).find((a) =>
      a.textContent?.includes(field.name),
    );
    expect(fieldRow?.textContent).toContain(`field of ${field.parent}`);
    const privRow = options(root).find((a) =>
      a.textContent?.includes(priv.name),
    );
    expect(privRow?.textContent).toContain("private");
  });

  it("C7: choosing the result for the page already showing re-runs the anchor instead of doing nothing", async () => {
    vi.useFakeTimers();
    stubFetch({
      "/tok/api/search/tree?q=val%3Ax": [
        200,
        {
          generation: 1,
          query: "val:x",
          partial: false,
          matches: 1,
          results: [
            {
              kind: "decl",
              ns: "val",
              name: "x",
              parent: null,
              anchor: "val:x",
              module: "M",
              ref: "M",
              exported: true,
            },
          ],
        },
      ],
    });
    const seen: string[] = [];
    const onReselect = (e: Event): void => {
      seen.push((e as CustomEvent<string>).detail);
    };
    window.addEventListener("hr:reselect-anchor", onReselect);
    try {
      const { root } = mount(
        { placement: "header" },
        "/tok/docs/tree/M#val%3Ax",
      );
      type(root, "val:x");
      await vi.advanceTimersByTimeAsync(120);
      input(root).dispatchEvent(key("Enter"));
      await vi.advanceTimersByTimeAsync(0);
      // Still on the same URL: no navigation happened (the router
      // would ignore it anyway), but the reselect event fired so a
      // mounted view can scroll/focus the declaration again.
      expect(window.location.pathname).toBe("/tok/docs/tree/M");
      expect(window.location.hash).toBe("#val%3Ax");
      expect(seen).toEqual(["#val%3Ax"]);
    } finally {
      window.removeEventListener("hr:reselect-anchor", onReselect);
    }
  });

  it("C8: a mousedown on the popup (not a result) doesn't blur the input or close it", async () => {
    vi.useFakeTimers();
    stubFetch({ "/tok/api/search/tree?q=map": [200, fixture("search")] });
    const { root } = mount({ placement: "header" });
    const i = input(root);
    i.focus();
    type(root, "map");
    await vi.advanceTimersByTimeAsync(120);
    const popup = root.querySelector(".search-popup");
    if (popup === null) throw new Error("no popup");
    const cancelled = !popup.dispatchEvent(
      new MouseEvent("mousedown", { bubbles: true, cancelable: true }),
    );
    expect(cancelled).toBe(true); // preventDefault() was called
    expect(i.getAttribute("aria-expanded")).toBe("true");
  });

  it("C10: an older indexing-event generation is ignored; a newer one refetches at once", async () => {
    vi.useFakeTimers();
    const partial = {
      generation: 2,
      query: "map",
      partial: true,
      matches: 1,
      results: [{ kind: "module", name: "Data.Map", ref: "Data.Map" }],
    };
    const fetchMock = vi.fn(() => Promise.resolve(jsonResponse(200, partial)));
    vi.stubGlobal("fetch", fetchMock);
    const { root, setIndexing } = mount({ placement: "page" });
    type(root, "map");
    await vi.advanceTimersByTimeAsync(120);
    expect(fetchMock).toHaveBeenCalledTimes(1);

    // Older than the answer's own snapshot: stale, ignored.
    setIndexing({ generation: 1, total: 10, ready: 10, failed: 0 });
    await vi.advanceTimersByTimeAsync(10);
    expect(fetchMock).toHaveBeenCalledTimes(1);

    // Newer (a Reload happened): our answer is for a gone snapshot,
    // so it refetches at once, throttle or not.
    setIndexing({ generation: 3, total: 10, ready: 1, failed: 0 });
    await vi.advanceTimersByTimeAsync(10);
    expect(fetchMock).toHaveBeenCalledTimes(2);
  });

  it('C10: the partial footer never shows a contradictory "(n of n)" right at done', async () => {
    vi.useFakeTimers();
    const partial = {
      generation: 1,
      query: "map",
      partial: true,
      matches: 1,
      results: [{ kind: "module", name: "Data.Map", ref: "Data.Map" }],
    };
    stubFetch({ "/tok/api/search/tree?q=map": [200, partial] });
    const { root, setIndexing } = mount({ placement: "page" });
    type(root, "map");
    await vi.advanceTimersByTimeAsync(120);
    setIndexing({ generation: 1, total: 10, ready: 10, failed: 0 }); // done
    // Before the search's own refetch for "done" lands, the footer
    // (still showing, partial() hasn't flipped yet) must not claim
    // "(10 of 10)" next to "still indexing".
    expect(root.querySelector(".search-partial")?.textContent).toBe(
      "Partial results, still indexing.",
    );
  });

  it("C5: the header never shows an empty popup strip for an empty query", async () => {
    vi.useFakeTimers();
    stubFetch({ "/tok/api/search/tree?q=map": [200, fixture("search")] });
    const { root } = mount({ placement: "header" });
    const i = input(root);
    expect(root.querySelector(".search-popup")).toBeNull();
    i.focus();
    expect(root.querySelector(".search-popup")).toBeNull(); // focus alone, still empty
    type(root, "map");
    await vi.advanceTimersByTimeAsync(120);
    expect(root.querySelector(".search-popup")).not.toBeNull();
    type(root, "");
    expect(root.querySelector(".search-popup")).toBeNull(); // cleared: closes at once
  });

  it("C5/C3: the header shows the live count inside the popup (sighted users, not just aria-live)", async () => {
    vi.useFakeTimers();
    stubFetch({
      "/tok/api/search/tree?q=zzzzqqq": [
        200,
        {
          generation: 1,
          query: "zzzzqqq",
          partial: false,
          matches: 0,
          results: [],
        },
      ],
    });
    const { root } = mount({ placement: "header" });
    type(root, "zzzzqqq");
    await vi.advanceTimersByTimeAsync(120);
    const popup = root.querySelector(".search-popup");
    expect(popup?.querySelector(".search-status")?.textContent).toBe(
      "No results.",
    );
  });
});

describe("SearchBox — review round 2 fixes", () => {
  // A fetch that resolves at once for anything except a query starting
  // with "Slow", which waits for release() — our own stand-in for
  // Probe2's slowFetch().
  function slowFetch(): {
    release: () => void;
    fetchMock: ReturnType<typeof vi.fn>;
  } {
    const pending: (() => void)[] = [];
    const fetchMock = vi.fn((req: RequestInfo | URL) => {
      const u = urlOf(req);
      if (u.includes("q=Slow")) {
        return new Promise<void>((resolve) => {
          pending.push(resolve);
        }).then(() => echoModule(req));
      }
      return echoModule(req);
    });
    vi.stubGlobal("fetch", fetchMock);
    return { release: () => pending.splice(0).forEach((f) => f()), fetchMock };
  }

  it("1: a pending Enter is cancelled by focus leaving the box, so a late answer never navigates", async () => {
    vi.useFakeTimers();
    const s = slowFetch();
    const { root } = mount({ placement: "header" });
    const i = input(root);
    type(root, "Other");
    await vi.advanceTimersByTimeAsync(120);
    type(root, "Slow");
    i.dispatchEvent(key("Enter")); // pending: "Slow" hasn't answered yet
    await vi.advanceTimersByTimeAsync(10);
    const outside = document.createElement("button");
    document.body.append(outside);
    i.dispatchEvent(
      new FocusEvent("focusout", { bubbles: true, relatedTarget: outside }),
    );
    s.release();
    await vi.advanceTimersByTimeAsync(10);
    expect(window.location.pathname).toBe("/tok/x"); // never navigated
  });

  it("1: a pending Enter is cancelled by a route change (e.g. a sidebar link)", async () => {
    vi.useFakeTimers();
    const s = slowFetch();
    const { root } = mount({ placement: "header" });
    const i = input(root);
    type(root, "Other");
    await vi.advanceTimersByTimeAsync(120);
    type(root, "Slow");
    i.dispatchEvent(key("Enter"));
    await vi.advanceTimersByTimeAsync(10);
    window.history.pushState(null, "", "/tok/elsewhere");
    window.dispatchEvent(new PopStateEvent("popstate"));
    await vi.advanceTimersByTimeAsync(0);
    s.release();
    await vi.advanceTimersByTimeAsync(10);
    expect(window.location.pathname).toBe("/tok/elsewhere"); // unchanged
  });

  it("1: a pending Enter is cancelled by clicking a different, already-visible result", async () => {
    vi.useFakeTimers();
    const s = slowFetch();
    const { root } = mount({ placement: "header" });
    const i = input(root);
    type(root, "Other");
    await vi.advanceTimersByTimeAsync(120);
    type(root, "Slow");
    i.dispatchEvent(key("Enter"));
    await vi.advanceTimersByTimeAsync(10);
    // The stale "Other" option is still on screen (results haven't
    // been replaced yet): clicking it is a deliberate, different
    // choice.
    options(root)[0]?.click();
    await vi.advanceTimersByTimeAsync(0);
    expect(window.location.pathname).toBe("/tok/docs/tree/Other");
    s.release();
    await vi.advanceTimersByTimeAsync(10);
    expect(window.location.pathname).toBe("/tok/docs/tree/Other"); // unchanged
  });

  it("3: a stale error from an earlier query doesn't cancel a newer pending Enter", async () => {
    vi.useFakeTimers();
    let resolveGood: (() => void) | undefined;
    const fetchMock = vi.fn((req: RequestInfo | URL) => {
      const u = urlOf(req);
      if (u.includes("q=Err")) {
        return Promise.resolve(new Response("boom", { status: 500 }));
      }
      if (u.includes("q=Good")) {
        return new Promise<void>((resolve) => {
          resolveGood = resolve;
        }).then(() => echoModule(req));
      }
      return echoModule(req);
    });
    vi.stubGlobal("fetch", fetchMock);
    const { root } = mount({ placement: "header" });
    const i = input(root);
    type(root, "Err");
    await vi.advanceTimersByTimeAsync(120);
    expect(status(root)).toBe("Search failed.");
    type(root, "Good");
    i.dispatchEvent(key("Enter")); // pending on "Good"; error() is
    // still "Err"'s (stale), but settledQuery() isn't "Good" yet.
    await vi.advanceTimersByTimeAsync(10);
    resolveGood?.();
    await vi.advanceTimersByTimeAsync(10);
    expect(window.location.pathname).toBe("/tok/docs/tree/Good");
  });

  it("6: no refetch while partial but the header popup is closed", async () => {
    vi.useFakeTimers();
    const partial = {
      generation: 1,
      query: "map",
      partial: true,
      matches: 1,
      results: [{ kind: "module", name: "Data.Map", ref: "Data.Map" }],
    };
    const fetchMock = vi.fn(() => Promise.resolve(jsonResponse(200, partial)));
    vi.stubGlobal("fetch", fetchMock);
    const { root, setIndexing } = mount({ placement: "header" });
    const i = input(root);
    type(root, "map");
    await vi.advanceTimersByTimeAsync(120);
    expect(fetchMock).toHaveBeenCalledTimes(1);
    i.dispatchEvent(key("Escape")); // closes the popup, keeps the text
    expect(i.getAttribute("aria-expanded")).toBe("false");
    // "Done" normally bypasses the throttle, but nothing is shown.
    setIndexing({ generation: 1, total: 10, ready: 10, failed: 0 });
    await vi.advanceTimersByTimeAsync(10);
    expect(fetchMock).toHaveBeenCalledTimes(1);
  });

  it("6: reopening the popup refetches once if its answer is still partial", async () => {
    vi.useFakeTimers();
    const partial = {
      generation: 1,
      query: "map",
      partial: true,
      matches: 1,
      results: [{ kind: "module", name: "Data.Map", ref: "Data.Map" }],
    };
    const fetchMock = vi.fn(() => Promise.resolve(jsonResponse(200, partial)));
    vi.stubGlobal("fetch", fetchMock);
    const { root } = mount({ placement: "header" });
    const i = input(root);
    type(root, "map");
    await vi.advanceTimersByTimeAsync(120);
    expect(fetchMock).toHaveBeenCalledTimes(1);
    i.dispatchEvent(key("Escape"));
    i.focus();
    await vi.advanceTimersByTimeAsync(10);
    expect(fetchMock).toHaveBeenCalledTimes(2); // caught up once
  });

  it("6: reopening doesn't refetch when the shown answer is already full", async () => {
    vi.useFakeTimers();
    const fetchMock = stubFetch({
      "/tok/api/search/tree?q=map": [200, fixture("search")], // partial: false
    });
    const { root } = mount({ placement: "header" });
    const i = input(root);
    type(root, "map");
    await vi.advanceTimersByTimeAsync(120);
    const before = fetchMock.mock.calls.length;
    i.dispatchEvent(key("Escape"));
    i.focus();
    await vi.advanceTimersByTimeAsync(10);
    expect(fetchMock.mock.calls.length).toBe(before);
  });
});

describe("DocsView decl-notice box really searches", () => {
  afterEach(() => {
    unmountApp();
    vi.unstubAllGlobals();
  });

  it("a missing anchor's notice box fetches and shows server results", async () => {
    stubFetch({
      "/tok/api/files/tree": [200, fixture("files")],
      "/tok/api/modules/tree": [200, fixture("modules")],
      "/tok/api/module/tree/Page": [200, cborFixture("module-page")],
      "/tok/api/search/tree?q=Nope": [
        200,
        {
          generation: 1,
          query: "Nope",
          partial: false,
          matches: 1,
          results: [{ kind: "module", name: "Other", ref: "Other" }],
        },
      ],
    });
    const main = mountAppAt("/tok/docs/tree/Page#type%3ANope").querySelector(
      "main",
    );
    if (main === null) throw new Error("no main");
    await vi.waitFor(() => {
      expect(main.querySelector(".decl-missing")).not.toBeNull();
    });
    await vi.waitFor(() => {
      expect(options(main)).toHaveLength(1);
    });
    expect(options(main)[0]?.textContent).toContain("Other");
  });
});

describe("NotFound page: uses the server search", () => {
  afterEach(() => {
    unmountApp();
    vi.unstubAllGlobals();
  });

  it("prefilled from the URL, server search, mouse-selects and navigates", async () => {
    stubFetch({
      "/tok/api/files/tree": [200, fixture("files")],
      "/tok/api/modules/tree": [200, fixture("modules")],
      "/tok/api/search/tree?q=No.Such": [
        200,
        {
          generation: 1,
          query: "No.Such",
          partial: false,
          matches: 0,
          results: [],
        },
      ],
      "/tok/api/search/tree?q=Shapes": [
        200,
        {
          generation: 1,
          query: "Shapes",
          partial: false,
          matches: 1,
          results: [{ kind: "module", name: "Shapes", ref: "Shapes" }],
        },
      ],
    });
    const main = mountAppAt("/tok/docs/tree/No.Such").querySelector("main");
    if (main === null) throw new Error("no main");
    await vi.waitFor(() => {
      expect(input(main).value).toBe("No.Such");
      expect(status(main)).toBe("No results.");
    });
    type(main, "Shapes");
    await vi.waitFor(() => {
      expect(options(main)).toHaveLength(1);
    });
    options(main)[0]?.click();
    await vi.waitFor(() => {
      expect(window.location.pathname).toBe("/tok/docs/tree/Shapes");
    });
  });
});
