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
      JSON.stringify({ generation: 1, total: 3, ready: 1, failed: 0 }),
    );
    expect(a.got.at(-1)).toEqual({
      type: "indexing",
      indexing: { generation: 1, total: 3, ready: 1, failed: 0 },
    });
  });

  it("a late listener gets the latest indexing state too", () => {
    const hub = createHub("/tok/api/events", FakeSource);
    only().indexing(
      JSON.stringify({ generation: 1, total: 3, ready: 1, failed: 0 }),
    );
    only().indexing(
      JSON.stringify({ generation: 1, total: 3, ready: 2, failed: 0 }),
    );
    const late = listener();
    hub.add(late);
    expect(late.got).toEqual([
      { type: "status", status: "connecting" },
      {
        type: "indexing",
        indexing: { generation: 1, total: 3, ready: 2, failed: 0 },
      },
    ]);
  });

  it("an indexing event with bad data is not passed on", () => {
    const hub = createHub("/tok/api/events", FakeSource);
    const a = listener();
    hub.add(a);
    const error = vi.spyOn(console, "error").mockImplementation(() => {});
    only().indexing("[");
    only().indexing(JSON.stringify({ generation: 1, total: 1 }));
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

describe("decodeWorkerMessage", () => {
  it("accepts our messages", () => {
    for (const m of [
      { type: "status", status: "ended" },
      { type: "hello" },
      {
        type: "indexing",
        indexing: { generation: 1, total: 3, ready: 1, failed: 0 },
      },
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
      { type: "indexing", indexing: { generation: 1, total: 1 } },
      { type: "indexing", indexing: "bogus" },
    ]) {
      expect(decodeWorkerMessage(m)).toBeUndefined();
    }
  });
});
