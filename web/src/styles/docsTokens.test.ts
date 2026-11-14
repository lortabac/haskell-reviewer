// @vitest-environment node
// Every token class has a colour in docs.css, or is plain on purpose,
// so a new class can't slip by (D-300).
import { readFileSync } from "node:fs";
import { join } from "node:path";
import { expect, it } from "vitest";
import { tokenClasses } from "../api/code";

const css = readFileSync(join(import.meta.dirname, "docs.css"), "utf-8");
const plain = ["sp", "rop", "pun"];

it.each(tokenClasses.filter((c) => !plain.includes(c)))(
  ".tok-%s has a colour",
  (c) => {
    const re = new RegExp(`\\.tok-${c}\\b[^{]*\\{[^}]*color:`);
    expect(css).toMatch(re);
  },
);
