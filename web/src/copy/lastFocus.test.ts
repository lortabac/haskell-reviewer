import { createRoot } from "solid-js";
import { afterEach, describe, expect, it } from "vitest";
import { lastFocus } from "./lastFocus";

afterEach(() => document.body.replaceChildren());

function setup() {
  const mk = (tag: string, id: string, parent: Element, extra = {}) => {
    const el = document.createElement(tag);
    el.id = id;
    for (const [k, v] of Object.entries(extra)) el.setAttribute(k, String(v));
    parent.append(el);
    return el;
  };
  const header = mk("header", "hd", document.body);
  mk("button", "h", header);
  const nav = mk("nav", "nv", document.body);
  mk("a", "side", nav, { href: "#x" });
  const main = mk("main", "mn", document.body);
  const sec = mk("section", "d1", main, { class: "decl", tabindex: -1 });
  mk("a", "in", sec, { href: "#y" });
  mk("p", "plain", main, { tabindex: -1 });
  const q = (id: string) => document.getElementById(id) as HTMLElement;
  let get = () => undefined as HTMLElement | undefined;
  createRoot(() => {
    const lf = lastFocus(
      () => document.querySelector("main") ?? undefined,
      "section.decl",
    );
    get = lf.get;
  });
  return { q, get: () => get() };
}

describe("lastFocus", () => {
  it("the item with focus", () => {
    const { q, get } = setup();
    q("in").focus();
    expect(get()).toBe(q("d1"));
  });

  it("focus in the header: the item it left", () => {
    const { q, get } = setup();
    q("d1").focus();
    q("h").focus();
    expect(get()).toBe(q("d1"));
  });

  it("focus went elsewhere first (sidebar): forgotten", () => {
    const { q, get } = setup();
    q("d1").focus();
    q("side").focus();
    q("h").focus();
    expect(get()).toBeUndefined();
  });

  it("focus on a non-item in the view: forgotten", () => {
    const { q, get } = setup();
    q("d1").focus();
    q("plain").focus();
    q("h").focus();
    expect(get()).toBeUndefined();
  });
});
