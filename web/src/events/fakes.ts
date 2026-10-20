// Test fakes for the browser APIs the events code uses.
import type { EventLike, SourceLike } from "./hub";

// A fake EventSource. Every instance is kept in `sources`.
export class FakeSource implements SourceLike {
  static sources: FakeSource[] = [];
  readyState = 0;
  closed = false;
  private handlers = new Map<string, ((event: EventLike) => void)[]>();

  constructor(readonly url: string) {
    FakeSource.sources.push(this);
  }

  close(): void {
    this.readyState = 2;
    this.closed = true;
  }

  addEventListener(type: string, listener: (event: EventLike) => void): void {
    this.handlers.set(type, [...(this.handlers.get(type) ?? []), listener]);
  }

  // Fire an event, after setting readyState if given.
  fire(type: string, readyState?: number, data?: string): void {
    if (readyState !== undefined) this.readyState = readyState;
    for (const l of this.handlers.get(type) ?? []) l({ data });
  }

  // The server's hello, with its real payload.
  hello(data = "{}"): void {
    this.fire("hello", undefined, data);
  }
}
