import { afterEach, beforeEach, describe, expect, it } from "vitest";
import { originOf, saveOrigin } from "./origin";

describe("originOf", () => {
  it("reads the column", () => {
    expect(originOf({ origin: { col: 3 }, _depth: 2 })).toEqual({ col: 3 });
  });
  it("anything else: none", () => {
    for (const s of [null, 1, {}, { origin: 1 }, { origin: { col: "1" } }]) {
      expect(originOf(s)).toBeUndefined();
    }
    expect(originOf({ origin: { col: -1 } })).toBeUndefined();
  });
});

describe("saveOrigin", () => {
  let len: number;
  beforeEach(() => {
    window.history.replaceState({ _depth: 0, x: 1 }, "", "/tok/src/tree/A.hs");
    len = window.history.length;
  });
  afterEach(() => {
    window.history.replaceState(null, "", "/");
  });

  it("another line: a new entry, with the router's depth", () => {
    saveOrigin(30, 4, undefined, `${window.location.origin}/tok/src/tree/B.hs`);
    expect(window.location.pathname + window.location.hash).toBe(
      "/tok/src/tree/A.hs#L30",
    );
    expect(window.history.length).toBe(len + 1);
    expect(window.history.state).toEqual({
      origin: { col: 4 },
      _depth: window.history.length - 1,
    });
  });

  it("the line the URL marks: no entry, the column kept", () => {
    window.history.replaceState({ _depth: 0 }, "", "/tok/src/tree/A.hs#L30");
    saveOrigin(30, 4, 30);
    expect(window.history.length).toBe(len);
    expect(window.history.state).toEqual({ _depth: 0, origin: { col: 4 } });
  });

  it("the hash is that line already (the router lags): no entry", () => {
    window.history.replaceState({ _depth: 0 }, "", "/tok/src/tree/A.hs#L30");
    saveOrigin(30, 4, 12);
    expect(window.history.length).toBe(len);
    expect(window.history.state).toEqual({ _depth: 0, origin: { col: 4 } });
  });

  it("line 1 and no hash: no entry", () => {
    saveOrigin(1, 0, undefined);
    expect(window.history.length).toBe(len);
    expect(window.location.hash).toBe("");
    expect(window.history.state).toEqual({
      _depth: 0,
      x: 1,
      origin: { col: 0 },
    });
  });

  it("a link to the very line: nothing", () => {
    const to = `${window.location.origin}/tok/src/tree/A.hs#L30`;
    saveOrigin(30, 4, undefined, to);
    expect(window.history.length).toBe(len);
    expect(window.history.state).toEqual({ _depth: 0, x: 1 });
  });
});
