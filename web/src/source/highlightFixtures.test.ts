// @vitest-environment node
// Server and client agree on columns: every run of every
// fixtures/highlight golden (written by HighlightGoldenTest.hs), cut
// with our helpers, gives the golden's text. Goldens show the first
// line of each run, as Haskell's show.
import { readdirSync, readFileSync } from "node:fs";
import { join } from "node:path";
import { describe, expect, it } from "vitest";
import { displayClasses, packRuns, type Run } from "../api/source";
import { utf16Cols } from "./columns";
import { pieces, styledPieces } from "./display";
import { lineRuns } from "./highlight";
import { lineIndex, lineText } from "./lines";

const dir = join(import.meta.dirname, "../../../fixtures/highlight");
const files = readdirSync(dir);

const names = (
  "NUL SOH STX ETX EOT ENQ ACK BEL BS HT LF VT FF CR SO SI " +
  "DLE DC1 DC2 DC3 DC4 NAK SYN ETB CAN EM SUB ESC FS GS RS US"
).split(" ");
const simple: Record<string, string> = {
  a: "\x07",
  b: "\b",
  f: "\f",
  n: "\n",
  r: "\r",
  t: "\t",
  v: "\v",
  "\\": "\\",
  '"': '"',
  "'": "'",
  "&": "",
};

// Haskell's show of a String, read back.
function unshow(s: string): string {
  if (!s.startsWith('"') || !s.endsWith('"'))
    throw new Error(`not shown: ${s}`);
  // Longest names first: SOH before SO.
  const alts = [...names, "DEL", "SP"].sort((a, b) => b.length - a.length);
  const re = new RegExp(`\\\\(\\d+|${alts.join("|")}|.)`, "g");
  return s.slice(1, -1).replace(re, (_, e: string) => {
    if (/^\d+$/.test(e)) return String.fromCodePoint(Number(e));
    if (e === "DEL") return "\x7f";
    if (e === "SP") return " ";
    const i = names.indexOf(e);
    if (i !== -1) return String.fromCharCode(i);
    const c = simple[e];
    if (c === undefined) throw new Error(`odd escape \\${e}`);
    return c;
  });
}

interface GoldenRun extends Run {
  text: string;
}

function readGolden(name: string): GoldenRun[] {
  const out: GoldenRun[] = [];
  const re = /^(\d+):(\d+)-(\d+):(\d+) (\w+) (".*")(…)?$/;
  for (const l of readFileSync(join(dir, name), "utf-8").split("\n")) {
    const m = re.exec(l);
    if (m === null) continue;
    const cls = displayClasses.indexOf(m[5] as (typeof displayClasses)[number]);
    if (cls === -1) throw new Error(`odd class in ${l}`);
    out.push({
      startLine: Number(m[1]),
      startCol: Number(m[2]),
      endLine: Number(m[3]),
      endCol: Number(m[4]),
      cls,
      link: out.length + 1,
      text: unshow(m[6] ?? '""'),
    });
  }
  return out;
}

const cases = files
  .filter((f) => f.endsWith(".golden"))
  .map((g) => {
    const base = g.replace(/\.golden$/, "");
    const src = files.find((f) => f === `${base}.hs` || f === `${base}.lhs`);
    if (src === undefined) throw new Error(`no source for ${g}`);
    return [g, src] as const;
  });

describe("highlight fixtures: client columns match the server's", () => {
  it("unshow", () => {
    expect(unshow('"a\\"b\\\\c"')).toBe('a"b\\c');
    expect(unshow('"\\128512\\&1\\SO\\&H\\SOH\\t"')).toBe(
      "\u{1f600}1\x0eH\x01\t",
    );
  });

  it("found the goldens", () => {
    expect(cases.length).toBeGreaterThan(10);
  });

  it.each(cases)("%s", (golden, src) => {
    const text = readFileSync(join(dir, src), "utf-8");
    const index = lineIndex(text);
    const runs = readGolden(golden);
    const line = (n: number) => lineText(text, index, n);
    // utf16Cols alone.
    for (const r of runs) {
      const l = line(r.startLine);
      const end = r.endLine === r.startLine ? r.endCol : 0xffffffff;
      const [a, b] = utf16Cols(l, [r.startCol, end]);
      expect(l.slice(a, b), JSON.stringify(r)).toBe(r.text);
    }
    // The whole way: runs cut per line, then pieces. Links tell runs
    // apart.
    const lr = lineRuns(packRuns(runs), index.count);
    for (let n = 1; n <= index.count; n++) {
      const ps = styledPieces(line(n), lr[n] ?? []);
      expect(ps.map((p) => p.text).join("")).toBe(
        pieces(line(n))
          .map((p) => p.text)
          .join(""),
      );
      for (const p of ps) {
        if (p.link === 0) continue;
        expect(p.cls).toBe(displayClasses[runs[p.link - 1]?.cls ?? -1]);
      }
    }
    for (const r of runs) {
      const got = styledPieces(line(r.startLine), lr[r.startLine] ?? [])
        .filter((p) => p.link === r.link)
        .map((p) => p.text)
        .join("");
      const want = pieces(r.text)
        .map((p) => p.text)
        .join("");
      expect(got, JSON.stringify(r)).toBe(want);
    }
  });
});
