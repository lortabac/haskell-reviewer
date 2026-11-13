import { createSignal } from "solid-js";
import { afterEach, beforeEach, describe, expect, it, vi } from "vitest";
import type { Indexing } from "../api/events";
import { cbor, fixture, stubFetch } from "../../test/fixtures";
import { mountAppAt, unmountApp } from "../../test/mount";
import { clearSourceCache } from "./pageSource";
import { relink } from "./relink";

// The real one, counted.
vi.mock("./relink", async (orig) => {
  const m = await orig<typeof import("./relink")>();
  return { ...m, relink: vi.fn(m.relink) };
});

// The docs page asks again while partial (D-293). Page fixture: area's
// signature "area, perimeter :: Shape -> Double", Double at token 11
// (target 1); Circle's Double (declaration 0) the same target.
type Json = Record<string, unknown>;
type Raw = Json & { parse: Json & { targets: unknown[] } };

const raw = (over: (r: Raw) => void = () => undefined): Raw => {
  const r = structuredClone(fixture("module-page")) as Raw;
  r.partial = true;
  over(r);
  return r;
};
const decls = (r: Raw) => r.parse.declarations as Json[];
const areaSig = (r: Raw) =>
  (decls(r)[5]!.signature as { code: unknown[] }).code;
// area's Double, plain.
const plain = (r: Raw) => {
  areaSig(r)[11] = ["con", "Double"];
};

const at = (generation: number, ready: number): Indexing => ({
  generation,
  total: 2,
  ready,
  failed: 0,
});
const settle = () => new Promise((r) => setTimeout(r, 20));

let now = 1000;
let frames: FrameRequestCallback[] = [];

beforeEach(() => {
  clearSourceCache();
  now = 1000;
  vi.spyOn(Date, "now").mockImplementation(() => now);
  frames = [];
  vi.stubGlobal("requestAnimationFrame", (f: FrameRequestCallback) => {
    frames.push(f);
    return frames.length;
  });
  vi.stubGlobal("cancelAnimationFrame", () => undefined);
});

afterEach(() => {
  unmountApp();
  vi.unstubAllGlobals();
  vi.restoreAllMocks();
});

// Mounts /tok/docs/tree/Page; api/module answers with the next of
// answers (the last one again when they run out).
async function openWith(
  answers: ([number, unknown] | (() => Promise<Response>))[],
) {
  const others = stubFetch({
    "/tok/api/files/tree": [200, fixture("files")],
    "/tok/api/modules/tree": [200, fixture("modules")],
  }).getMockImplementation()!;
  let calls = 0;
  const signals: (AbortSignal | undefined)[] = [];
  vi.stubGlobal(
    "fetch",
    vi.fn((input: RequestInfo | URL, init?: RequestInit) => {
      if (input !== "/tok/api/module/tree/Page") return others(input);
      signals.push(init?.signal ?? undefined);
      calls++;
      const a = answers[Math.min(calls - 1, answers.length - 1)]!;
      if (typeof a === "function") return a();
      const [status, body] = a;
      return Promise.resolve(
        new Response(new Uint8Array(cbor(body)), {
          status,
          headers: { "Content-Type": "application/cbor" },
        }),
      );
    }),
  );
  const [ix, setIx] = createSignal<Indexing>();
  // eslint-disable-next-line solid/reactivity -- read by the page
  const main = mountAppAt("/tok/docs/tree/Page", undefined, ix).querySelector(
    "main",
  );
  if (main === null) throw new Error("no main");
  await vi.waitFor(() => expect(main.querySelector(".decl")).not.toBeNull());
  return { main, calls: () => calls, signals, setIx };
}

const section = (main: HTMLElement, head: string): HTMLElement => {
  const s = [...main.querySelectorAll<HTMLElement>("section.decl")].find((el) =>
    el.querySelector(":scope > .decl-head .sig")?.textContent.startsWith(head),
  );
  if (s === undefined) throw new Error(`no declaration ${head}`);
  return s;
};
// The Double in a declaration's head, link or not.
const double = (main: HTMLElement, head: string): HTMLElement => {
  const t = [
    ...section(main, head).querySelectorAll<HTMLElement>(
      ":scope > .decl-head .sig .tok-con",
    ),
  ].find((x) => x.textContent === "Double");
  if (t === undefined) throw new Error("no Double");
  return t;
};
const circleDouble = (main: HTMLElement): HTMLElement => {
  const t = [
    ...section(main, "data Shape").querySelectorAll<HTMLElement>("a.tok-con"),
  ].find((x) => x.textContent === "Double");
  if (t === undefined) throw new Error("no Double");
  return t;
};

describe("docs page refetch while partial", () => {
  it("not partial: no refetch", async () => {
    const { calls, setIx } = await openWith([
      [200, raw((r) => (r.partial = false))],
    ]);
    now = 5000;
    setIx(at(1, 1));
    setIx(at(1, 2));
    await settle();
    expect(calls()).toBe(1);
  });

  it("throttled, due, once at done, then none; other snapshots", async () => {
    const { calls, setIx } = await openWith([[200, raw()]]);
    now = 1500;
    setIx(at(1, 1));
    await settle();
    expect(calls()).toBe(1);
    now = 2100;
    setIx(at(1, 1));
    await vi.waitFor(() => expect(calls()).toBe(2));
    // An older snapshot: ignored.
    now = 4000;
    setIx(at(0, 1));
    await settle();
    expect(calls()).toBe(2);
    now = 4100;
    setIx(at(1, 2));
    await vi.waitFor(() => expect(calls()).toBe(3));
    await settle();
    now = 9000;
    setIx({ ...at(1, 2) });
    await settle();
    expect(calls()).toBe(3);
    // A newer one: stops.
    setIx(at(2, 1));
    now = 12000;
    setIx(at(2, 1));
    await settle();
    expect(calls()).toBe(3);
  });

  it("done answer not partial: no more asks", async () => {
    const { calls, setIx } = await openWith([
      [200, raw()],
      [200, raw((r) => (r.partial = false))],
    ]);
    now = 2000;
    setIx(at(1, 2));
    await vi.waitFor(() => expect(calls()).toBe(2));
    await settle();
    now = 5000;
    setIx(at(1, 1));
    await settle();
    expect(calls()).toBe(2);
  });

  it("a failure or an equal answer: the same elements", async () => {
    const { main, calls, setIx } = await openWith([
      [200, raw(plain)],
      [500, "boom"],
      [200, raw(plain)],
    ]);
    const tok = double(main, "area");
    const link = circleDouble(main);
    for (const [i, t] of [2000, 3100].entries()) {
      now = t;
      setIx(at(1, 1));
      await vi.waitFor(() => expect(calls()).toBe(i + 2));
      await settle();
      expect(double(main, "area")).toBe(tok);
      expect(circleDouble(main)).toBe(link);
    }
  });

  it("a changed link: only that token is new; open things stay", async () => {
    const { main, calls, setIx } = await openWith([
      [200, raw(plain)],
      [200, raw()],
    ]);
    const area = section(main, "area");
    const tok = double(main, "area");
    expect(tok.tagName).toBe("SPAN");
    // "->" before Double, in the same block.
    const arrow = tok.parentElement!.firstElementChild!;
    expect(arrow.textContent).toBe("->");
    const link = circleDouble(main);
    // Open: Shape's instances, the extensions, area's source.
    const shape = section(main, "data Shape");
    const inst = [
      ...shape.querySelectorAll<HTMLDetailsElement>("details.decl-instances"),
    ].find((d) => d.parentElement === shape)!;
    inst.open = true;
    inst.dispatchEvent(new Event("toggle"));
    const ext = main.querySelector<HTMLDetailsElement>(".docs-extensions")!;
    ext.open = true;
    [...area.querySelectorAll<HTMLButtonElement>("button.decl-source-toggle")]
      .find((b) => b.textContent.includes("of area"))!
      .click();
    await vi.waitFor(() => expect(area.querySelector(".src")).not.toBeNull());
    const src = area.querySelector(".src");
    const instItems = inst.querySelector("ul");
    now = 2000;
    setIx(at(1, 1));
    await vi.waitFor(() => expect(calls()).toBe(2));
    await vi.waitFor(() => expect(double(main, "area").tagName).toBe("A"));
    const a = double(main, "area");
    expect(a.getAttribute("href")).toBe(
      "https://hoogle.haskell.org/?hoogle=Double%20%2Bmodule%3APrelude",
    );
    expect(tok.isConnected).toBe(false);
    expect(a.parentElement!.firstElementChild).toBe(arrow);
    expect(section(main, "area")).toBe(area);
    expect(circleDouble(main)).toBe(link);
    expect(area.querySelector(".src")).toBe(src);
    expect(inst.open).toBe(true);
    expect(inst.querySelector("ul")).toBe(instItems);
    expect(ext.open).toBe(true);
    expect(main.querySelector(".docs-extensions")).toBe(ext);
  });

  it("focus: an unchanged link keeps it; a changed one moves to the new link", async () => {
    const other = raw((r) => {
      areaSig(r)[11] = ["con", "Double", null, 8];
    });
    const { main, calls, setIx } = await openWith([
      [200, raw()],
      [200, other],
    ]);
    const link = circleDouble(main);
    link.focus();
    now = 2000;
    setIx(at(1, 1));
    await vi.waitFor(() => expect(calls()).toBe(2));
    await vi.waitFor(() =>
      expect(double(main, "area").getAttribute("href")).toContain("Int"),
    );
    expect(document.activeElement).toBe(link);
    // Now the changed one, focused.
    const a = double(main, "area");
    a.focus();
    const back = raw();
    vi.stubGlobal(
      "fetch",
      vi.fn(() =>
        Promise.resolve(
          new Response(new Uint8Array(cbor(back)), {
            status: 200,
            headers: { "Content-Type": "application/cbor" },
          }),
        ),
      ),
    );
    now = 3100;
    setIx(at(1, 1));
    await vi.waitFor(() =>
      expect(double(main, "area").getAttribute("href")).toContain("Double"),
    );
    expect(a.isConnected).toBe(false);
    expect(document.activeElement).toBe(double(main, "area"));
  });

  it("focus: a link that becomes plain hands it to its declaration", async () => {
    const { main, calls, setIx } = await openWith([
      [200, raw()],
      [200, raw(plain)],
    ]);
    const a = double(main, "area");
    a.focus();
    now = 2000;
    setIx(at(1, 1));
    await vi.waitFor(() => expect(calls()).toBe(2));
    await vi.waitFor(() => expect(double(main, "area").tagName).toBe("SPAN"));
    expect(document.activeElement).toBe(section(main, "area"));
  });

  it("an answer that differs beyond links: old page kept, no more asks", async () => {
    const { main, calls, setIx } = await openWith([
      [200, raw(plain)],
      [200, raw((r) => (decls(r)[5]!.line = 99))],
      [200, raw()],
    ]);
    const tok = double(main, "area");
    now = 2000;
    setIx(at(1, 1));
    await vi.waitFor(() => expect(calls()).toBe(2));
    await settle();
    now = 4000;
    setIx(at(1, 2));
    await settle();
    expect(calls()).toBe(2);
    expect(double(main, "area")).toBe(tok);
  });

  it("a late answer is dropped; unmount aborts", async () => {
    let late: ((r: Response) => void) | undefined;
    const { main, calls, signals, setIx } = await openWith([
      [200, raw(plain)],
      () => new Promise((done) => (late = done)),
      [200, raw(plain)],
    ]);
    now = 2000;
    setIx(at(1, 1));
    await vi.waitFor(() => expect(late).toBeDefined());
    now = 3100;
    setIx(at(1, 1));
    await vi.waitFor(() => expect(calls()).toBe(3));
    expect(signals[1]?.aborted).toBe(true);
    late?.(
      new Response(new Uint8Array(cbor(raw())), {
        status: 200,
        headers: { "Content-Type": "application/cbor" },
      }),
    );
    await settle();
    expect(double(main, "area").tagName).toBe("SPAN");
    expect(signals[2]?.aborted).toBe(false);
    unmountApp();
    expect(signals[2]?.aborted).toBe(true);
  });

  it("the same bytes as the last answer: not decoded or walked", async () => {
    const { main, calls, setIx } = await openWith([
      [200, raw(plain)],
      [200, raw(plain)],
      [200, raw()],
      [200, raw()],
    ]);
    vi.mocked(relink).mockClear();
    for (const [i, t] of [2000, 3100, 4200].entries()) {
      now = t;
      setIx(at(1, 1));
      await vi.waitFor(() => expect(calls()).toBe(i + 2));
      await settle();
    }
    expect(double(main, "area").tagName).toBe("A");
    // Only the changed answer.
    expect(vi.mocked(relink)).toHaveBeenCalledTimes(1);
  });

  it("a new target, not in the first table: the link goes to it", async () => {
    const foo = [
      "decl",
      "Other.Mod",
      "Other.hs",
      10,
      "val:foo",
      [],
      1,
      "foo :: Int",
      false,
    ];
    const { main, calls, setIx } = await openWith([
      [200, raw(plain)],
      [
        200,
        raw((r) => {
          r.parse.targets.push(foo);
          areaSig(r)[11] = ["con", "Double", null, r.parse.targets.length];
        }),
      ],
    ]);
    const link = circleDouble(main);
    now = 2000;
    setIx(at(1, 1));
    await vi.waitFor(() => expect(calls()).toBe(2));
    await vi.waitFor(() => expect(double(main, "area").tagName).toBe("A"));
    expect(double(main, "area").getAttribute("href")).toBe(
      "/tok/docs/tree/Other.Mod#val%3Afoo",
    );
    // The old targets keep their links.
    expect(circleDouble(main)).toBe(link);
    expect(link.getAttribute("href")).toContain("hoogle");
  });

  it("focus on a Haddock name whose link changes: the new link", async () => {
    // The header doc: "Missing" links to target 10 (Hoogle).
    const header = (r: Raw) =>
      (
        (r.parse.header as Json).haddock as {
          doc: { blocks: { inlines: Json[] }[] };
        }
      ).doc.blocks[0]!.inlines[3]!;
    const { main, calls, setIx } = await openWith([
      [200, raw()],
      [200, raw((r) => (header(r).link = 1))],
    ]);
    const doc = main.querySelector<HTMLElement>(".docs-page > .doc")!;
    const name = () =>
      [...doc.querySelectorAll<HTMLAnchorElement>("a")].find(
        (a) => a.textContent === "Missing",
      )!;
    const a = name();
    expect(a.getAttribute("href")).toContain("Missing");
    a.focus();
    now = 2000;
    setIx(at(1, 1));
    await vi.waitFor(() => expect(calls()).toBe(2));
    await vi.waitFor(() =>
      expect(name().getAttribute("href")).toContain("Double"),
    );
    expect(a.isConnected).toBe(false);
    expect(document.activeElement).toBe(name());
  });
});
