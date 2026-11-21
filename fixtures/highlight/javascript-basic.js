#!/usr/bin/env node
// Every kind of run.
/* a block
   /* not nested */
import { readFile } from "node:fs";

export default class Point extends Base {
  static #count = 0;
  constructor(x, y) {
    super();
    this.x = x;	// a tab before
    this.$y = y ?? null;
  }
}

async function f(_a, $b) {
  const s = "a \"quoted\" \\ string";
  let t = 'it\'s';
  var u = `two
lines ${s + 1}`;
  if (typeof _a === "undefined" && !(_a instanceof Point)) return undefined;
  for (const k of [0xFF, 1_000n, 1.5e3, .5, NaN, Infinity]) await k;
  return Array.from(t).get;
}

const é = "astral 😀"; // after astral
"no end
