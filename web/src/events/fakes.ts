// Test fakes for the browser APIs the events code uses.
import type { SourceLike } from "./hub";

// A fake EventSource. Every instance is kept in `sources`.
export class FakeSource implements SourceLike {
  static sources: FakeSource[] = [];
  readyState = 0;
  closed = false;
  private handlers = new Map<string, (() => void)[]>();

  constructor(readonly url: string) {
    FakeSource.sources.push(this);
  }

  close(): void {
    this.readyState = 2;
    this.closed = true;
  }

  addEventListener(type: string, listener: () => void): void {
    this.handlers.set(type, [...(this.handlers.get(type) ?? []), listener]);
  }

  // Fire an event, after setting readyState if given.
  fire(type: string, readyState?: number): void {
    if (readyState !== undefined) this.readyState = readyState;
    for (const l of this.handlers.get(type) ?? []) l();
  }
}
