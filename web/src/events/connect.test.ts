import { beforeEach, describe, expect, it } from "vitest";
import { connectEvents, type SharedWorkerLike } from "./connect";
import { FakeSource } from "./fakes";
import type { WorkerMessage } from "./protocol";

class FakeWorker implements SharedWorkerLike {
  static workers: FakeWorker[] = [];
  port: SharedWorkerLike["port"] = { onmessage: null };
  onerror: SharedWorkerLike["onerror"] = null;

  constructor(
    readonly url: URL,
    readonly options: { name: string },
  ) {
    FakeWorker.workers.push(this);
  }

  send(data: unknown): void {
    this.port.onmessage?.(new MessageEvent("message", { data }));
  }
}

const location = { pathname: "/tok/src/a.hs" };

beforeEach(() => {
  FakeSource.sources = [];
  FakeWorker.workers = [];
});

describe("connectEvents", () => {
  it("uses the SharedWorker when there is one", () => {
    const got: WorkerMessage[] = [];
    connectEvents(
      { SharedWorker: FakeWorker, EventSource: FakeSource, location },
      (m) => got.push(m),
    );
    expect(FakeWorker.workers).toHaveLength(1);
    const w = FakeWorker.workers[0];
    expect(w?.options).toEqual({ name: "haskell-reviewer-events" });
    expect(w?.url.href).toContain("events.worker");
    expect(FakeSource.sources).toEqual([]);
    w?.send({ type: "status", status: "open" });
    w?.send({ type: "junk" });
    expect(got).toEqual([{ type: "status", status: "open" }]);
  });

  it("no SharedWorker: own stream and the fallback notice", () => {
    const got: WorkerMessage[] = [];
    connectEvents({ EventSource: FakeSource, location }, (m) => got.push(m));
    expect(FakeSource.sources.map((s) => s.url)).toEqual(["/tok/api/events"]);
    expect(got).toEqual([
      { type: "unsupported" },
      { type: "status", status: "connecting" },
    ]);
  });

  it("SharedWorker throws: own stream and the fallback notice", () => {
    class Throwing extends FakeWorker {
      constructor(url: URL, options: { name: string }) {
        super(url, options);
        throw new Error("SecurityError");
      }
    }
    const got: WorkerMessage[] = [];
    connectEvents(
      { SharedWorker: Throwing, EventSource: FakeSource, location },
      (m) => got.push(m),
    );
    expect(FakeSource.sources.map((s) => s.url)).toEqual(["/tok/api/events"]);
    expect(got[0]).toEqual({ type: "unsupported" });
  });

  it("worker fails to start: own stream, once", () => {
    const got: WorkerMessage[] = [];
    connectEvents(
      { SharedWorker: FakeWorker, EventSource: FakeSource, location },
      (m) => got.push(m),
    );
    const w = FakeWorker.workers[0];
    w?.onerror?.(new ErrorEvent("error"));
    w?.onerror?.(new ErrorEvent("error"));
    w?.send({ type: "unsupported" });
    expect(FakeSource.sources.map((s) => s.url)).toEqual(["/tok/api/events"]);
    expect(got.filter((m) => m.type === "unsupported")).toHaveLength(1);
  });

  it("worker says unsupported: own stream and the fallback notice", () => {
    const got: WorkerMessage[] = [];
    connectEvents(
      { SharedWorker: FakeWorker, EventSource: FakeSource, location },
      (m) => got.push(m),
    );
    FakeWorker.workers[0]?.send({ type: "unsupported" });
    expect(FakeSource.sources.map((s) => s.url)).toEqual(["/tok/api/events"]);
    expect(got[0]).toEqual({ type: "unsupported" });
  });
});
