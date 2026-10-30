import type { JSX } from "solid-js";
import { render } from "solid-js/web";
import { afterEach, describe, expect, it } from "vitest";
import type { Code as Tokens } from "../api/code";
import type { Block, Doc, Inline } from "../api/richtext";
import { Code, Signature } from "./Code";
import { type Links, LinksContext } from "./links";
import { DocView, numberLabels } from "./RichText";

let dispose: (() => void) | undefined;

afterEach(() => {
  dispose?.();
  dispose = undefined;
  document.body.replaceChildren();
});

// Handles 1 and 2 are on the page; 9 isn't.
const links: Links = {
  decl: (n) => (n === 9 ? undefined : `/tok/docs/tree/M#h${n}`),
  module: (ref, anchor) =>
    `/tok/docs/tree/${ref}` + (anchor === null ? "" : "#" + anchor),
};

function mount(el: () => JSX.Element): HTMLElement {
  const root = document.createElement("div");
  document.body.append(root);
  dispose = render(
    () => <LinksContext.Provider value={links}>{el()}</LinksContext.Provider>,
    root,
  );
  return root;
}

const t = (text: string): Inline => ({ kind: "text", text });
const doc = (blocks: Block[], since: Doc["since"] = null): Doc => ({
  blocks,
  since,
});
const para = (...inlines: Inline[]): Block => ({ kind: "paragraph", inlines });

function show(d: Doc, level = 3): HTMLElement {
  return mount(() => <DocView doc={d} level={level} />);
}

describe("DocView", () => {
  it("markup in text stays text", () => {
    const root = show(
      doc([
        para(t('<script>window.__pwned=1</script><img src=x onerror="1">')),
      ]),
    );
    expect(root.querySelector("script, img")).toBeNull();
    expect(root.textContent).toBe(
      '<script>window.__pwned=1</script><img src=x onerror="1">',
    );
  });

  it("emphasis, bold, code, math", () => {
    const root = show(
      doc([
        para(
          { kind: "emphasis", inlines: [t("a")] },
          { kind: "bold", inlines: [t("b")] },
          { kind: "code", inlines: [t("c")] },
          { kind: "math", style: "inline", tex: "x^2" },
          { kind: "math", style: "display", tex: "\\sum" },
        ),
      ]),
    );
    expect(root.querySelector("em")?.textContent).toBe("a");
    expect(root.querySelector("strong")?.textContent).toBe("b");
    expect(root.querySelector("p > code")?.textContent).toBe("c");
    expect(root.querySelector(".doc-math")?.textContent).toBe("x^2");
    expect(root.querySelector(".doc-math-display")?.textContent).toBe("\\sum");
  });

  it("links off the app: new tab, no opener, no referrer, said so", () => {
    const root = show(
      doc([
        para(
          { kind: "link", url: "https://example.com/?a=1", label: [t("site")] },
          { kind: "link", url: "http://x.org", label: null },
        ),
      ]),
    );
    const [a, b] = [...root.querySelectorAll("a")];
    expect(a?.getAttribute("href")).toBe("https://example.com/?a=1");
    expect(a?.getAttribute("target")).toBe("_blank");
    expect(a?.getAttribute("rel")).toBe("noopener noreferrer");
    expect(a?.className).toBe("ext-link");
    expect(a?.textContent).toBe("site (opens in a new tab)");
    expect(b?.textContent).toBe("http://x.org (opens in a new tab)");
  });

  it("names: a link when the page has it, else code", () => {
    const root = show(
      doc([
        para(
          { kind: "name", written: "f", ns: "any", target: 1 },
          { kind: "name", written: "g", ns: "value", target: null },
          { kind: "name", written: "h", ns: "type", target: 9 },
        ),
      ]),
    );
    const a = root.querySelector("a");
    expect(a?.getAttribute("href")).toBe("/tok/docs/tree/M#h1");
    expect(a?.textContent).toBe("f");
    expect(root.querySelectorAll("a")).toHaveLength(1);
    expect(
      [...root.querySelectorAll("code")].map((c) => c.textContent),
    ).toEqual(["f", "g", "h"]);
  });

  it("module links: by ref, anchor as written; no ref: text", () => {
    const root = show(
      doc([
        para(
          {
            kind: "module",
            module: "Sections",
            anchor: "v:x",
            ref: "Sections",
            label: null,
          },
          {
            kind: "module",
            module: "A.B",
            anchor: null,
            ref: "pkg:lib:A.B",
            label: [t("ab")],
          },
          {
            kind: "module",
            module: "Nowhere",
            anchor: null,
            ref: null,
            label: null,
          },
        ),
      ]),
    );
    expect(
      [...root.querySelectorAll("a")].map((a) => [
        a.textContent,
        a.getAttribute("href"),
      ]),
    ).toEqual([
      ["Sections", "/tok/docs/tree/Sections#v:x"],
      ["ab", "/tok/docs/tree/pkg:lib:A.B"],
    ]);
    expect(root.textContent).toContain("Nowhere");
  });

  it("headings one below the level, h6 at most", () => {
    const h = (level: number): Block => ({
      kind: "heading",
      level,
      inlines: [t(`l${level}`)],
    });
    const root = show(doc([h(1), h(2), h(6)]), 3);
    expect(
      [...root.querySelectorAll(".doc-heading")].map((e) => e.tagName),
    ).toEqual(["H4", "H5", "H6"]);
  });

  it("lists, definitions, code, examples, property", () => {
    const root = show(
      doc([
        { kind: "bullets", items: [[para(t("one"))], [para(t("two"))]] },
        {
          kind: "definitions",
          items: [{ term: [t("term")], blocks: [para(t("def"))] }],
        },
        { kind: "code", inlines: [t("x = 1")] },
        {
          kind: "examples",
          items: [
            { expression: "1 + 1", results: ["2"] },
            { expression: "x", results: [] },
          ],
        },
        { kind: "property", text: "x == x" },
      ]),
    );
    expect(
      [...root.querySelectorAll("ul > li")].map((l) => l.textContent),
    ).toEqual(["one", "two"]);
    expect(root.querySelector("dl dt")?.textContent).toBe("term");
    expect(root.querySelector("dl dd")?.textContent).toBe("def");
    expect(root.querySelector("pre.doc-code code")?.textContent).toBe("x = 1");
    expect(root.querySelector("pre.doc-examples")?.textContent).toBe(
      ">>> 1 + 1\n2\n>>> x",
    );
    expect(root.querySelector("pre.doc-property")?.textContent).toBe(
      "prop> x == x",
    );
  });

  it("numbered lists: own labels, exact, null goes on from the last", () => {
    const root = show(
      doc([
        {
          kind: "numbered",
          items: [
            { number: "9007199254740993", blocks: [para(t("a"))] },
            { number: null, blocks: [para(t("b"))] },
          ],
        },
      ]),
    );
    expect(
      [...root.querySelectorAll(".doc-number")].map((e) => e.textContent),
    ).toEqual(["9007199254740993.", "9007199254740994."]);
    expect(numberLabels([null, null, "7", null])).toEqual(["1", "2", "7", "8"]);
  });

  it("tables with spans", () => {
    const cell = (s: string, colspan = 1, rowspan = 1) => ({
      colspan,
      rowspan,
      inlines: [t(s)],
    });
    const root = show(
      doc([
        {
          kind: "table",
          header: [[cell("h", 2)]],
          body: [[cell("a", 1, 2), cell("b")], [cell("c")]],
        },
      ]),
    );
    const th = root.querySelector("thead th");
    expect(th?.getAttribute("colspan")).toBe("2");
    const tds = [...root.querySelectorAll("tbody td")];
    expect(tds.map((td) => td.textContent)).toEqual(["a", "b", "c"]);
    expect(tds[0]?.getAttribute("rowspan")).toBe("2");
    expect(tds[1]?.hasAttribute("rowspan")).toBe(false);
  });

  it("since, with and without a package", () => {
    expect(
      show(doc([], { package: "base", version: "4.18.0" })).textContent,
    ).toBe("Since: base-4.18.0");
    dispose?.();
    expect(show(doc([], { package: null, version: "1.2" })).textContent).toBe(
      "Since: 1.2",
    );
  });
});

describe("Code and Signature", () => {
  const code: Tokens = [
    ["var", "f"],
    ["sp", " "],
    ["pun", "::"],
    ["sp", " "],
    ["con", "T", 1],
    ["sp", " "],
    ["pun", "->"],
    ["sp", " "],
    ["con", "U", 9],
  ];

  it("a span per token with its class; links to the page", () => {
    const root = mount(() => <Code code={code} />);
    const c = root.querySelector("code.code");
    expect(c?.textContent).toBe("f :: T -> U");
    expect(
      [...(c?.children ?? [])].map((e) => [e.tagName, e.className]),
    ).toEqual([
      ["SPAN", "tok-var"],
      ["SPAN", "tok-pun"],
      ["A", "tok-con"],
      ["SPAN", "tok-pun"],
      // Not on the page: no link.
      ["SPAN", "tok-con"],
    ]);
    expect(c?.querySelector("a")?.getAttribute("href")).toBe(
      "/tok/docs/tree/M#h1",
    );
  });

  it("signature: parts cut before arrows, a space between", () => {
    const root = mount(() => <Signature code={code} />);
    const sig = root.querySelector("code.sig");
    expect(sig?.textContent).toBe("f :: T -> U");
    expect(
      [...(sig?.querySelectorAll(".sig-part") ?? [])].map((p) => p.textContent),
    ).toEqual(["f :: T", "-> U"]);
  });
});
