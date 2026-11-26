import { afterEach, beforeEach, describe, expect, it, vi } from "vitest";
import { FakeSource } from "./fakes";
import { createHub, giveUpAfterMs } from "./hub";
import { decodeWorkerMessage, type WorkerMessage } from "./protocol";

function listener(): {
  got: WorkerMessage[];
  post: (m: WorkerMessage) => void;
} {
  const got: WorkerMessage[] = [];
  return { got, post: (m) => got.push(m) };
}

function only(): FakeSource {
  expect(FakeSource.sources).toHaveLength(1);
  const s = FakeSource.sources[0];
  if (s === undefined) throw new Error("no source");
  return s;
}

beforeEach(() => {
  FakeSource.sources = [];
  vi.useFakeTimers();
});

afterEach(() => {
  vi.useRealTimers();
});

function lastStatus(got: WorkerMessage[]): WorkerMessage | undefined {
  return got.filter((m) => m.type === "status").at(-1);
}

describe("createHub", () => {
  it("one source for many listeners", () => {
    const hub = createHub("/tok/api/events", FakeSource);
    const a = listener();
    const b = listener();
    hub.add(a);
    hub.add(b);
    expect(only().url).toBe("/tok/api/events");
    only().fire("open", 1);
    only().hello();
    const expected = [
      { type: "status", status: "connecting" },
      { type: "status", status: "open" },
      { type: "hello" },
    ];
    expect(a.got).toEqual(expected);
    expect(b.got).toEqual(expected);
  });

  it("a hello with bad data is not passed on", () => {
    const hub = createHub("/tok/api/events", FakeSource);
    const a = listener();
    hub.add(a);
    const error = vi.spyOn(console, "error").mockImplementation(() => {});
    only().hello("[");
    only().hello("[]");
    expect(a.got).toEqual([{ type: "status", status: "connecting" }]);
    expect(error).toHaveBeenCalledTimes(2);
    error.mockRestore();
  });

  it("a late listener gets the current status", () => {
    const hub = createHub("/tok/api/events", FakeSource);
    only().fire("open", 1);
    const late = listener();
    hub.add(late);
    expect(late.got).toEqual([{ type: "status", status: "open" }]);
  });

  it("forwards indexing events (D-210)", () => {
    const hub = createHub("/tok/api/events", FakeSource);
    const a = listener();
    hub.add(a);
    only().indexing(
      JSON.stringify({
        generation: 1,
        side: "tree",
        total: 3,
        ready: 1,
        failed: 0,
      }),
    );
    expect(a.got.at(-1)).toEqual({
      type: "indexing",
      indexing: { generation: 1, side: "tree", total: 3, ready: 1, failed: 0 },
    });
  });

  it("a late listener gets the latest indexing state too", () => {
    const hub = createHub("/tok/api/events", FakeSource);
    only().indexing(
      JSON.stringify({
        generation: 1,
        side: "tree",
        total: 3,
        ready: 1,
        failed: 0,
      }),
    );
    only().indexing(
      JSON.stringify({
        generation: 1,
        side: "tree",
        total: 3,
        ready: 2,
        failed: 0,
      }),
    );
    const late = listener();
    hub.add(late);
    expect(late.got).toEqual([
      { type: "status", status: "connecting" },
      {
        type: "indexing",
        indexing: {
          generation: 1,
          side: "tree",
          total: 3,
          ready: 2,
          failed: 0,
        },
      },
    ]);
  });

  it("keeps the latest event of each side; a late listener gets both", () => {
    const hub = createHub("/tok/api/events", FakeSource);
    const ev = (side: string, ready: number) =>
      JSON.stringify({ generation: 1, side, total: 3, ready, failed: 0 });
    only().indexing(ev("head", 1));
    only().indexing(ev("base", 1));
    // A base event doesn't replace head's.
    only().indexing(ev("base", 3));
    only().indexing(ev("head", 2));
    const late = listener();
    hub.add(late);
    expect(late.got).toEqual([
      { type: "status", status: "connecting" },
      {
        type: "indexing",
        indexing: {
          generation: 1,
          side: "head",
          total: 3,
          ready: 2,
          failed: 0,
        },
      },
      {
        type: "indexing",
        indexing: {
          generation: 1,
          side: "base",
          total: 3,
          ready: 3,
          failed: 0,
        },
      },
    ]);
  });

  it("an indexing event with bad data is not passed on", () => {
    const hub = createHub("/tok/api/events", FakeSource);
    const a = listener();
    hub.add(a);
    const error = vi.spyOn(console, "error").mockImplementation(() => {});
    only().indexing("[");
    only().indexing(JSON.stringify({ generation: 1, side: "tree", total: 1 }));
    expect(a.got).toEqual([{ type: "status", status: "connecting" }]);
    expect(error).toHaveBeenCalledTimes(2);
    error.mockRestore();
  });

  it("error while CLOSED: ended", () => {
    const hub = createHub("/tok/api/events", FakeSource);
    const a = listener();
    hub.add(a);
    only().fire("error", 2);
    expect(a.got.at(-1)).toEqual({ type: "status", status: "ended" });
    expect(only().closed).toBe(true);
  });

  it("no way back in within 30 s: ended, source closed", () => {
    const hub = createHub("/tok/api/events", FakeSource);
    const a = listener();
    hub.add(a);
    only().fire("open", 1);
    only().fire("error", 0);
    vi.advanceTimersByTime(giveUpAfterMs - 100);
    // More failed tries don't restart the timer.
    only().fire("error", 0);
    expect(lastStatus(a.got)).toEqual({
      type: "status",
      status: "reconnecting",
    });
    expect(only().closed).toBe(false);
    vi.advanceTimersByTime(100);
    expect(lastStatus(a.got)).toEqual({ type: "status", status: "ended" });
    expect(only().closed).toBe(true);
    expect(giveUpAfterMs).toBe(30_000);
  });

  it("back in within 30 s: open, the timer is gone", () => {
    const hub = createHub("/tok/api/events", FakeSource);
    const a = listener();
    hub.add(a);
    only().fire("error", 0);
    vi.advanceTimersByTime(giveUpAfterMs - 100);
    only().fire("open", 1);
    vi.advanceTimersByTime(2 * giveUpAfterMs);
    expect(lastStatus(a.got)).toEqual({ type: "status", status: "open" });
    expect(only().closed).toBe(false);
  });

  it("error while CONNECTING: reconnecting, then open again", () => {
    const hub = createHub("/tok/api/events", FakeSource);
    const a = listener();
    hub.add(a);
    only().fire("open", 1);
    only().fire("error", 0);
    expect(a.got.at(-1)).toEqual({ type: "status", status: "reconnecting" });
    only().fire("open", 1);
    expect(a.got.at(-1)).toEqual({ type: "status", status: "open" });
  });
});

describe("createHub, generations (D-354)", () => {
  const ix = (generation: number, side: string, ready = 0) =>
    JSON.stringify({ generation, side, total: 2, ready, failed: 0 });
  const gen = (g: number) => JSON.stringify({ generation: g });

  it("forwards the generation event", () => {
    const hub = createHub("/tok/api/events", FakeSource);
    const a = listener();
    hub.add(a);
    only().fire("generation", undefined, gen(2));
    expect(a.got.at(-1)).toEqual({ type: "generation", generation: 2 });
  });

  it("a bad or old generation is not passed on", () => {
    const hub = createHub("/tok/api/events", FakeSource);
    const a = listener();
    hub.add(a);
    const error = vi.spyOn(console, "error").mockImplementation(() => {});
    only().fire("generation", undefined, "[");
    only().fire("generation", undefined, JSON.stringify({ generation: 0 }));
    only().fire("generation", undefined, gen(3));
    only().fire("generation", undefined, gen(3));
    only().fire("generation", undefined, gen(2));
    expect(a.got.filter((m) => m.type === "generation")).toEqual([
      { type: "generation", generation: 3 },
    ]);
    expect(error).toHaveBeenCalledTimes(2);
    error.mockRestore();
  });

  it("replays the newest generation, from indexing too (a reconnect)", () => {
    const hub = createHub("/tok/api/events", FakeSource);
    only().indexing(ix(1, "tree"));
    only().fire("generation", undefined, gen(2));
    only().indexing(ix(2, "tree"));
    // A reconnect after another reload: no generation event on open.
    only().indexing(ix(3, "tree"));
    const late = listener();
    hub.add(late);
    expect(late.got.slice(1, 2)).toEqual([
      { type: "generation", generation: 3, replay: true },
    ]);
  });

  it("replays status, then the generation, then indexing", () => {
    const hub = createHub("/tok/api/events", FakeSource);
    only().fire("open", 1);
    only().indexing(ix(1, "base"));
    only().fire("generation", undefined, gen(2));
    only().indexing(ix(2, "head", 1));
    const late = listener();
    hub.add(late);
    expect(late.got).toEqual([
      { type: "status", status: "open" },
      { type: "generation", generation: 2, replay: true },
      {
        type: "indexing",
        indexing: {
          generation: 2,
          side: "head",
          total: 2,
          ready: 1,
          failed: 0,
        },
      },
    ]);
  });

  it("drops indexing states older than the newest generation", () => {
    const hub = createHub("/tok/api/events", FakeSource);
    const a = listener();
    hub.add(a);
    only().indexing(ix(1, "base"));
    only().indexing(ix(1, "head"));
    // A newer indexing event, with no generation event (a reconnect).
    only().indexing(ix(2, "head"));
    // Late, for the old snapshot: not passed on.
    only().indexing(ix(1, "base", 1));
    expect(
      a.got.filter((m) => m.type === "indexing").map((m) => m.indexing),
    ).toHaveLength(3);
    const late = listener();
    hub.add(late);
    expect(late.got.map((m) => m.type)).toEqual(["status", "indexing"]);
  });
});

describe("decodeWorkerMessage", () => {
  it("accepts our messages", () => {
    for (const m of [
      { type: "status", status: "ended" },
      { type: "hello" },
      {
        type: "indexing",
        indexing: {
          generation: 1,
          side: "tree",
          total: 3,
          ready: 1,
          failed: 0,
        },
      },
      { type: "generation", generation: 2 },
      { type: "generation", generation: 2, replay: true },
      { type: "unsupported" },
    ]) {
      expect(decodeWorkerMessage(m)).toEqual(m);
    }
  });

  it("rejects anything else", () => {
    for (const m of [
      null,
      "hello",
      {},
      { type: "status", status: "bogus" },
      { type: "nope" },
      { type: "indexing", indexing: { generation: 1, side: "tree", total: 1 } },
      { type: "indexing", indexing: "bogus" },
      { type: "generation" },
      { type: "generation", generation: 0 },
      { type: "generation", generation: "2" },
    ]) {
      expect(decodeWorkerMessage(m)).toBeUndefined();
    }
  });
});
