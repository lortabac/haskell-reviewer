// What Copy gives for a declaration on the docs page (D-299).
import type { AnyDecl } from "../api/module";
import { qualified } from "../copy/names";
import { readable, type Shown } from "./Decl";
import type { PageIndex } from "./pageIndex";
import { firstCodeLine } from "./source";

// Whether declCopyText has something, without making it (an instance
// head can be big).
export function hasCopyText(d: Shown | AnyDecl): boolean {
  if ("kind" in d && d.kind === "splice") return false;
  if ("kind" in d && (d.kind === "instance" || d.kind === "faminst")) {
    return true;
  }
  return d.name !== null && d.name !== "";
}

// Data.Foo.bar; an instance: its head, "instance Eq (Box a)". A splice
// has no name: undefined.
export function declCopyText(
  d: Shown | AnyDecl,
  ix: PageIndex,
  mod: string,
): string | undefined {
  if ("kind" in d) {
    if (d.kind === "splice") return undefined;
    if (d.kind === "instance" || d.kind === "faminst") return readable(d, ix);
  }
  return d.name === null || d.name === "" ? undefined : qualified(mod, d.name);
}

// The line "Copy location" gives: where its code starts (what 2
// opens); a nested item has only the line of its name.
export function declLine(d: Shown | AnyDecl): number {
  if ("source" in d) {
    const l = firstCodeLine(d.source);
    if (l !== undefined) return l;
  }
  return d.line;
}
