import { createSignal } from "solid-js";
import { afterEach, beforeEach, describe, expect, it, vi } from "vitest";
import type { Indexing } from "../api/events";
import { fixture, stubFetch } from "../../test/fixtures";
import { mountAppAt, unmountApp } from "../../test/mount";
import { clearRefsCache } from "./RefsView";

const api = "/tok/api/refs/tree?module=R.Shape&decl=val%3Aarea";
const page = "/tok/refs/tree/R.Shape?decl=val%3Aarea";
const base = {
  "/tok/api/files/tree": [200, fixture("files")] as [number, unknown],
  "/tok/api/modules/tree": [200, fixture("modules")] as [number, unknown],
};
const settle = () => new Promise((r) => setTimeout(r, 20));
const at = (generation: number, ready: number): Indexing => ({
  generation,
  total: 2,
  ready,
  failed: 0,
});

beforeEach(() => clearRefsCache());
afterEach(() => {
  unmountApp();
  vi.unstubAllGlobals();
  vi.restoreAllMocks();
});

function open(
  routes: Record<string, [number, unknown]>,
  url = page,
  state: unknown = null,
) {
  const fetchMock = stubFetch({ ...base, ...routes });
  const [ix, setIx] = createSignal<Indexing>();
  // eslint-disable-next-line solid/reactivity -- read by the page
  const root = mountAppAt(url, undefined, ix, state);
  const main = root.querySelector("main")!;
  return { main, setIx, fetchMock };
}

describe("RefsView", () => {
  it("groups, links, badges", async () => {
    const { main } = open({ [api]: [200, fixture("refs")] });
    await vi.waitFor(() =>
      expect(main.querySelector(".refs-group")).not.toBeNull(),
    );
    expect(main.querySelector("h1")?.textContent).toBe(
      "References to area (value) in R.Shape",
    );
    expect(main.querySelector(".refs-summary")?.textContent).toBe(
      "10 references in 3 modules. 1 is a guess.",
    );
    expect(main.textContent).toContain("Syntactic: found by name and imports");
    expect(main.querySelector(".refs-partial")).toBeNull();
    const groups = main.querySelectorAll(".refs-group");
    expect(groups.length).toBe(3);
    expect(groups[0]!.querySelector("h2")?.textContent).toBe(
      "R.Shape (declared here)",
    );
    const a = groups[1]!.querySelector<HTMLAnchorElement>("li a")!;
    expect(a.getAttribute("href")).toBe("/tok/src/tree/r/src/R/Guess.hs#L7");
    expect(a.textContent).toBe("Line 7 guess = area");
    const li = groups[1]!.querySelector("li")!;
    expect(li.textContent).toContain("guess");
    expect(li.textContent).toContain("Resolved heuristically: One of several");
    expect(li.querySelector("mark")?.textContent).toBe("area");
    expect(main.querySelector("[role=status]")?.textContent).toBe(
      "10 references in 3 modules.",
    );
  });

  it("partial: asked again, rows kept, announced once at done", async () => {
    const partial = { ...(fixture("refs") as object), partial: true };
    const answers: unknown[] = [partial, fixture("refs")];
    const others = stubFetch(base).getMockImplementation()!;
    let calls = 0;
    vi.stubGlobal("fetch", (input: RequestInfo | URL) => {
      if (input !== api) return others(input);
      const body = answers[Math.min(calls++, answers.length - 1)];
      return Promise.resolve(new Response(JSON.stringify(body)));
    });
    const [ix, setIx] = createSignal<Indexing>();
    // eslint-disable-next-line solid/reactivity -- read by the page
    const main = mountAppAt(page, undefined, ix).querySelector("main")!;
    await vi.waitFor(() =>
      expect(main.querySelector(".refs-partial")).not.toBeNull(),
    );
    expect(main.querySelector("[role=status]")?.textContent).toBe("");
    const first = main.querySelector("li a");
    setIx(at(1, 1));
    await vi.waitFor(() => expect(calls).toBe(1));
    setIx(at(1, 2));
    await vi.waitFor(() => expect(calls).toBe(2));
    await settle();
    expect(main.querySelector(".refs-partial")).toBeNull();
    expect(main.querySelector("li a")).toBe(first);
    expect(main.querySelector("[role=status]")?.textContent).toBe(
      "10 references in 3 modules.",
    );
  });

  it("a complete answer adds a row before the focused one and drops one", async () => {
    const full = fixture("refs") as { results: unknown[]; total: number };
    const partial = {
      ...full,
      partial: true,
      total: 9,
      results: full.results.slice(1),
    };
    const done = {
      ...full,
      total: 9,
      results: full.results.slice(0, 9),
    };
    const answers: unknown[] = [partial, done];
    const others = stubFetch(base).getMockImplementation()!;
    let calls = 0;
    vi.stubGlobal("fetch", (input: RequestInfo | URL) => {
      if (input !== api) return others(input);
      const body = answers[Math.min(calls++, answers.length - 1)];
      return Promise.resolve(new Response(JSON.stringify(body)));
    });
    const [ix, setIx] = createSignal<Indexing>();
    // eslint-disable-next-line solid/reactivity -- read by the page
    const main = mountAppAt(page, undefined, ix).querySelector("main")!;
    await vi.waitFor(() =>
      expect(main.querySelector(".refs-partial")).not.toBeNull(),
    );
    expect(main.querySelectorAll(".refs-item").length).toBe(9);
    const focused = main.querySelectorAll<HTMLElement>(".refs-item a")[1]!;
    focused.focus();
    const text = focused.textContent;
    setIx(at(1, 2));
    await vi.waitFor(() => expect(calls).toBe(2));
    await vi.waitFor(() =>
      expect(main.querySelector(".refs-partial")).toBeNull(),
    );
    const items = main.querySelectorAll(".refs-item");
    expect(items.length).toBe(9);
    // One row now before it; same element, same focus.
    expect(main.querySelectorAll<HTMLElement>(".refs-item a")[2]).toBe(focused);
    expect(focused.textContent).toBe(text);
    expect(document.activeElement).toBe(focused);
    // The last one is gone.
    expect(
      main.querySelectorAll(".refs-group")[2]!.querySelectorAll("li").length,
    ).toBe(4);
  });

  it("capped: the guess sentence is about the shown ones", async () => {
    const full = fixture("refs") as object;
    const { main } = open({ [api]: [200, { ...full, total: 4145 }] });
    await vi.waitFor(() =>
      expect(main.querySelector(".refs-summary")).not.toBeNull(),
    );
    expect(main.querySelector(".refs-summary")?.textContent).toBe(
      "Showing the first 10 of 4,145 references. 1 of these is a guess.",
    );
  });

  it("Back: the followed row is focused again; without a marker, the h1", async () => {
    const key = "R.Guess|r/src/R/Guess.hs|7,9,7,13|use";
    const { main } = open({ [api]: [200, fixture("refs")] }, page, {
      _depth: 2,
      refsFollowed: key,
    });
    await vi.waitFor(() =>
      expect(document.activeElement).toBe(
        main.querySelector(`[data-ref-key="${key}"]`),
      ),
    );
    unmountApp();
    clearRefsCache();
    const m2 = open({ [api]: [200, fixture("refs")] }).main;
    await vi.waitFor(() =>
      expect(m2.querySelector(".refs-group")).not.toBeNull(),
    );
    await new Promise((r) => setTimeout(r, 50));
    expect(m2.contains(document.activeElement)).toBe(false);
  });

  it("a click on a row marks the entry; the state keeps its other keys", async () => {
    const { main } = open({ [api]: [200, fixture("refs")] }, page, {
      _depth: 3,
    });
    await vi.waitFor(() =>
      expect(main.querySelector(".refs-item a")).not.toBeNull(),
    );
    const a = main.querySelectorAll<HTMLElement>(".refs-item a")[1]!;
    a.click();
    const st = window.history.state as Record<string, unknown>;
    expect(st.refsFollowed).toBe(a.dataset.refKey);
    expect(st._depth).toBeDefined();
  });

  it("404 and error", async () => {
    const { main } = open({
      [api]: [404, fixture("refs-not-found")],
    });
    await vi.waitFor(() =>
      expect(main.textContent).toContain("No such declaration"),
    );
    unmountApp();
    const e = open({ [api]: [500, "boom"] });
    await vi.waitFor(() =>
      expect(e.main.textContent).toContain("Couldn't load the references."),
    );
    expect(e.main.querySelector("h1")?.textContent).toContain("area");
  });

  it("404 reached by an in-app link: focus goes to the new heading", async () => {
    const gone = "/tok/api/refs/tree?module=R.Shape&decl=val%3Agone";
    const { main } = open({
      [api]: [200, fixture("refs")],
      [gone]: [404, fixture("refs-not-found")],
    });
    await vi.waitFor(() =>
      expect(main.querySelector(".refs-group")).not.toBeNull(),
    );
    // A link inside the page: it goes away with it.
    const a = document.createElement("a");
    a.href = "/tok/refs/tree/R.Shape?decl=val%3Agone";
    a.textContent = "gone";
    main.querySelector(".refs-list")!.append(a);
    a.focus();
    a.click();
    await vi.waitFor(() =>
      expect(main.textContent).toContain("No such declaration"),
    );
    await vi.waitFor(() =>
      expect(document.activeElement).toBe(main.querySelector("h1")),
    );
  });

  it("a bad URL is not found", () => {
    const { main } = open({}, "/tok/refs/tree/R.Shape");
    expect(main.textContent).toContain("Not found");
  });

  it("Back: a complete answer is used with no fetch", async () => {
    const { main, fetchMock } = open({ [api]: [200, fixture("refs")] });
    await vi.waitFor(() =>
      expect(main.querySelector(".refs-group")).not.toBeNull(),
    );
    unmountApp();
    fetchMock.mockClear();
    const [ix] = createSignal<Indexing>(at(1, 2));
    // eslint-disable-next-line solid/reactivity -- read by the page
    const m2 = mountAppAt(page, undefined, ix).querySelector("main")!;
    await vi.waitFor(() =>
      expect(m2.querySelector(".refs-group")).not.toBeNull(),
    );
    expect(fetchMock.mock.calls.some((c) => c[0] === api)).toBe(false);
  });
});
