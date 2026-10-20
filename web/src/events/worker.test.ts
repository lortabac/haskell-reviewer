import { beforeEach, describe, expect, it } from "vitest";
import { FakeSource } from "./fakes";
import type { WorkerMessage } from "./protocol";
import { startWorker, type PortLike, type WorkerScope } from "./worker";

function port(): PortLike & { got: WorkerMessage[] } {
  const got: WorkerMessage[] = [];
  return { got, postMessage: (m) => got.push(m) };
}

function scope(withSource: boolean): WorkerScope {
  return {
    location: { pathname: "/tok/assets/events.worker-abc.js" },
    onconnect: null,
    EventSource: withSource ? FakeSource : undefined,
  };
}

function connect(s: WorkerScope, p: PortLike): void {
  if (s.onconnect === null) throw new Error("no onconnect");
  s.onconnect({ ports: [p] });
}

beforeEach(() => {
  FakeSource.sources = [];
});

describe("startWorker", () => {
  it("each tab gets the stream; one source for all", () => {
    const s = scope(true);
    startWorker(s);
    const a = port();
    const b = port();
    connect(s, a);
    connect(s, b);
    expect(FakeSource.sources.map((x) => x.url)).toEqual(["/tok/api/events"]);
    FakeSource.sources[0]?.fire("hello");
    expect(a.got).toContainEqual({ type: "hello" });
    expect(b.got).toContainEqual({ type: "hello" });
  });

  it("no EventSource in the worker: unsupported", () => {
    const s = scope(false);
    startWorker(s);
    const a = port();
    connect(s, a);
    expect(a.got).toEqual([{ type: "unsupported" }]);
  });
});
