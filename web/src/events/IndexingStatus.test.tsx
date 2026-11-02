import { createSignal } from "solid-js";
import { render } from "solid-js/web";
import { afterEach, beforeEach, describe, expect, it, vi } from "vitest";
import type { Indexing } from "../api/events";
import { ConnectionContext } from "./ConnectionNotice";
import { IndexingContext } from "./indexing";
import { IndexingStatus } from "./IndexingStatus";
import type { Status } from "./protocol";

let dispose: (() => void) | undefined;

function mount(): {
  root: HTMLElement;
  setIndexing: (ix: Indexing | undefined) => void;
  setStatus: (s: Status) => void;
} {
  const [indexing, setIndexing] = createSignal<Indexing | undefined>();
  const [status, setStatus] = createSignal<Status>("open");
  const root = document.createElement("div");
  document.body.append(root);
  dispose = render(
    () => (
      <ConnectionContext.Provider value={{ status, fallback: () => false }}>
        <IndexingContext.Provider value={{ indexing }}>
          <IndexingStatus />
        </IndexingContext.Provider>
      </ConnectionContext.Provider>
    ),
    root,
  );
  return { root, setIndexing, setStatus };
}

function visible(root: HTMLElement): string {
  return root.querySelector(".indexing-status")?.textContent ?? "";
}

function live(root: HTMLElement): Element {
  const r = root.querySelector('[role="status"]');
  if (r === null) throw new Error("no live region");
  return r;
}

beforeEach(() => {
  vi.useFakeTimers();
});

afterEach(() => {
  dispose?.();
  document.body.replaceChildren();
  vi.useRealTimers();
});

describe("IndexingStatus", () => {
  it("undefined: nothing", () => {
    const { root } = mount();
    expect(visible(root)).toBe("");
    expect(live(root).textContent).toBe("");
  });

  it("not done, under 1 s: nothing", async () => {
    const { root, setIndexing } = mount();
    setIndexing({ generation: 1, total: 10, ready: 3, failed: 0 });
    await vi.advanceTimersByTimeAsync(900);
    expect(visible(root)).toBe("");
  });

  it("not done, 1 s or more: the count", async () => {
    const { root, setIndexing } = mount();
    setIndexing({ generation: 1, total: 10, ready: 3, failed: 0 });
    await vi.advanceTimersByTimeAsync(1000);
    expect(visible(root)).toBe("Indexing 3 of 10 modules");
  });

  it("the failed suffix", async () => {
    const { root, setIndexing } = mount();
    setIndexing({ generation: 1, total: 10, ready: 3, failed: 1 });
    await vi.advanceTimersByTimeAsync(1000);
    expect(visible(root)).toBe("Indexing 4 of 10 modules (1 failed)");
  });

  it("singular: 1 module total", async () => {
    const { root, setIndexing } = mount();
    setIndexing({ generation: 1, total: 1, ready: 0, failed: 0 });
    await vi.advanceTimersByTimeAsync(1000);
    expect(visible(root)).toBe("Indexing 0 of 1 module");
  });

  it("done, no failures, was shown: hides, announces once", async () => {
    const { root, setIndexing } = mount();
    setIndexing({ generation: 1, total: 10, ready: 3, failed: 0 });
    await vi.advanceTimersByTimeAsync(1000);
    expect(visible(root)).toBe("Indexing 3 of 10 modules");
    setIndexing({ generation: 1, total: 10, ready: 10, failed: 0 });
    await vi.advanceTimersByTimeAsync(0);
    expect(visible(root)).toBe("");
    expect(live(root).textContent).toBe("Indexing done.");
  });

  it("done with failures, was shown: persistent line, one announcement", async () => {
    const { root, setIndexing } = mount();
    setIndexing({ generation: 1, total: 10, ready: 3, failed: 1 });
    await vi.advanceTimersByTimeAsync(1000);
    setIndexing({ generation: 1, total: 10, ready: 9, failed: 1 });
    await vi.advanceTimersByTimeAsync(0);
    expect(visible(root)).toBe("1 module couldn't be indexed");
    expect(live(root).textContent).toBe(
      "Indexing done; 1 module couldn't be indexed.",
    );
    // Settling again (e.g. a late replay) says nothing new.
    setIndexing({ generation: 1, total: 10, ready: 9, failed: 1 });
    await vi.advanceTimersByTimeAsync(0);
    expect(live(root).textContent).toBe(
      "Indexing done; 1 module couldn't be indexed.",
    );
  });

  it("done with failures, never shown (a fast run): the failure line still shows, no announcement (C2)", async () => {
    const { root, setIndexing } = mount();
    setIndexing({ generation: 1, total: 10, ready: 5, failed: 2 });
    await vi.advanceTimersByTimeAsync(200); // under the 1 s delay
    setIndexing({ generation: 1, total: 10, ready: 8, failed: 2 });
    await vi.advanceTimersByTimeAsync(0);
    expect(visible(root)).toBe("2 modules couldn't be indexed");
    expect(live(root).textContent).toBe("");
  });

  it("done with no failures, never shown (a fast run): nothing", async () => {
    const { root, setIndexing } = mount();
    setIndexing({ generation: 1, total: 10, ready: 5, failed: 0 });
    await vi.advanceTimersByTimeAsync(200); // under the 1 s delay
    setIndexing({ generation: 1, total: 10, ready: 10, failed: 0 });
    await vi.advanceTimersByTimeAsync(0);
    expect(visible(root)).toBe("");
    expect(live(root).textContent).toBe("");
  });

  it("no per-tick announcement while still partial", async () => {
    const { root, setIndexing } = mount();
    setIndexing({ generation: 1, total: 10, ready: 1, failed: 0 });
    await vi.advanceTimersByTimeAsync(1000);
    setIndexing({ generation: 1, total: 10, ready: 2, failed: 0 });
    await vi.advanceTimersByTimeAsync(10);
    setIndexing({ generation: 1, total: 10, ready: 3, failed: 0 });
    await vi.advanceTimersByTimeAsync(10);
    expect(live(root).textContent).toBe("");
  });

  it("a newer generation starts over: shown again, announced again", async () => {
    const { root, setIndexing } = mount();
    setIndexing({ generation: 1, total: 10, ready: 10, failed: 0 });
    await vi.advanceTimersByTimeAsync(1000);
    expect(visible(root)).toBe(""); // already done when first seen
    setIndexing({ generation: 2, total: 5, ready: 0, failed: 0 });
    await vi.advanceTimersByTimeAsync(1000);
    expect(visible(root)).toBe("Indexing 0 of 5 modules");
    setIndexing({ generation: 2, total: 5, ready: 5, failed: 0 });
    await vi.advanceTimersByTimeAsync(0);
    expect(live(root).textContent).toBe("Indexing done.");
  });

  it("two shown runs that end the same way are both announced (C1: a Reload)", async () => {
    const { root, setIndexing } = mount();
    setIndexing({ generation: 1, total: 10, ready: 3, failed: 0 });
    await vi.advanceTimersByTimeAsync(1000);
    setIndexing({ generation: 1, total: 10, ready: 10, failed: 0 });
    await vi.advanceTimersByTimeAsync(0);
    expect(live(root).textContent).toBe("Indexing done.");

    // A second run, same final sentence: setting the live region to
    // the identical string again would be no DOM mutation at all, so
    // it has to be cleared first (loading() turning true) for a
    // screen reader to notice the second "Indexing done."
    setIndexing({ generation: 2, total: 4, ready: 0, failed: 0 });
    expect(live(root).textContent).toBe(""); // cleared as the run starts
    await vi.advanceTimersByTimeAsync(1000);
    expect(visible(root)).toBe("Indexing 0 of 4 modules");
    setIndexing({ generation: 2, total: 4, ready: 4, failed: 0 });
    await vi.advanceTimersByTimeAsync(0);
    expect(live(root).textContent).toBe("Indexing done.");
  });

  it("connection ended: nothing, even with failures shown", async () => {
    const { root, setIndexing, setStatus } = mount();
    setIndexing({ generation: 1, total: 10, ready: 3, failed: 1 });
    await vi.advanceTimersByTimeAsync(1000);
    setIndexing({ generation: 1, total: 10, ready: 9, failed: 1 });
    await vi.advanceTimersByTimeAsync(0);
    expect(visible(root)).toBe("1 module couldn't be indexed");
    setStatus("ended");
    await vi.advanceTimersByTimeAsync(0);
    expect(visible(root)).toBe("");
  });

  it("the live region is always in the DOM", () => {
    const { root } = mount();
    expect(root.querySelector('[role="status"]')).not.toBeNull();
  });

  it("no provider: nothing", () => {
    const root = document.createElement("div");
    document.body.append(root);
    dispose = render(() => <IndexingStatus />, root);
    expect(visible(root)).toBe("");
    expect(live(root).textContent).toBe("");
  });
});
