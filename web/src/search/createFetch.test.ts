import { createRoot, createSignal } from "solid-js";
import { describe, expect, it } from "vitest";
import type { Result } from "../api/client";
import type { Side } from "../sides";
import { createFetch } from "./createFetch";

// A promise and its resolve.
function deferred<T>(): { promise: Promise<T>; resolve: (v: T) => void } {
  let resolve!: (v: T) => void;
  const promise = new Promise<T>((r) => (resolve = r));
  return { promise, resolve };
}

const settle = () => new Promise((r) => setTimeout(r, 0));

describe("createFetch, sides (D-332)", () => {
  it("a slow answer of the old side, after a side change, is dropped", async () => {
    const asks = new Map<Side, ReturnType<typeof deferred<Result<string>>>>();
    const [side, setSide] = createSignal<Side>("head");
    const f = createRoot(() =>
      createFetch(
        () => "x",
        side,
        (_q, s) => {
          const d = deferred<Result<string>>();
          asks.set(s, d);
          return d.promise;
        },
        () => "x",
      ),
    );
    expect([...asks.keys()]).toEqual(["head"]);
    setSide("base");
    expect([...asks.keys()]).toEqual(["head", "base"]);
    asks.get("base")?.resolve({ kind: "ok", value: "from base" });
    await settle();
    expect(f.value()).toBe("from base");
    expect(f.answeredQuery()).toBe("x");
    asks.get("head")?.resolve({ kind: "ok", value: "from head" });
    await settle();
    expect(f.value()).toBe("from base");
    expect(f.loading()).toBe(false);
  });

  it("nothing of the old side shows while the new side loads", async () => {
    const [side, setSide] = createSignal<Side>("head");
    let pending: ((r: Result<string>) => void) | undefined;
    const f = createRoot(() =>
      createFetch(
        () => "x",
        side,
        (_q, s) =>
          s === "head"
            ? Promise.resolve({ kind: "ok", value: "from head" } as const)
            : new Promise<Result<string>>((r) => (pending = r)),
        () => "x",
      ),
    );
    await settle();
    expect(f.value()).toBe("from head");
    setSide("base");
    expect(f.value()).toBeUndefined();
    expect(f.answeredQuery()).toBeUndefined();
    expect(f.settledQuery()).toBeUndefined();
    pending?.({ kind: "ok", value: "from base" });
    await settle();
    expect(f.value()).toBe("from base");
  });
});
