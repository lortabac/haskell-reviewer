import { decode } from "cbor-x/decode-no-eval";
import { describe, expect, it } from "vitest";
import { cborFixture, cborFixtures } from "../../test/fixtures";
import { decodeDiffView, type DiffSide } from "../api/diff";
import {
  changeNoteWords,
  diffNotes,
  notTextNote,
  typeChangedNote,
} from "./notes";

const view = (name: string) => decodeDiffView(decode(cborFixture(name)));

// Every api/diff fixture, and its notes.
const expected: Record<string, string[]> = {
  "diff-added": [],
  "diff-binary": ["Binary file changed (200 bytes → 300 bytes)."],
  "diff-eol": [
    "Line endings changed on 2 lines; not shown as changes.",
    "Byte order mark added.",
  ],
  "diff-filtered": [
    "Shown as on disk: a git filter applies; the stored content may differ.",
  ],
  "diff-modified": [],
  "diff-not-local": ["Not in the local repo (partial clone)."],
  "diff-removed": [],
  "diff-renamed": ["Renamed from old/R.hs (60%).", "File mode changed."],
  "diff-submodule": ["Subproject commit ccccccc → nested git repository."],
  "diff-symlink": ["Symbolic link: the lines are its target."],
  "diff-too-large": [
    "Too large to diff (head: 2,000,000 bytes; the limit is 1 MiB).",
  ],
  "diff-too-slow": ["This diff took over 2 s, so it isn't shown."],
  "diff-type-changed": ["Changed from a file to a symbolic link."],
  "diff-unchanged": ["No changes in this file."],
  "diff-ws-ignore": [
    "Byte order mark removed.",
    "No changes when whitespace is ignored.",
  ],
};

describe("diffNotes", () => {
  it("covers every api/diff fixture", () => {
    const names = [...cborFixtures.keys()].filter((n) => n.startsWith("diff-"));
    expect(names.sort()).toEqual(Object.keys(expected).sort());
  });

  it.each(Object.entries(expected))("%s", (name, notes) => {
    expect(diffNotes(view(name))).toEqual(notes);
  });

  it("all equal, nothing else said: no change in the text", () => {
    const v = view("diff-unchanged");
    expect(diffNotes({ ...v, status: "modified" })).toEqual([
      "No changes in the text.",
    ]);
  });
});

describe("notTextNote", () => {
  const bin = (size: number): DiffSide => ({ kind: "binary", size });
  const absent: DiffSide = { kind: "absent" };
  const text: DiffSide = { kind: "text" };
  const big = (size: number): DiffSide => ({
    kind: "too-large",
    size,
    limit: 1048576,
  });
  const sub = (commit: string | null): DiffSide => ({
    kind: "submodule",
    commit,
  });

  it("added and removed", () => {
    expect(notTextNote(absent, bin(3))).toBe("Binary file added (3 bytes).");
    expect(notTextNote(bin(1), absent)).toBe("Binary file removed (1 byte).");
    expect(notTextNote(absent, sub("abcdef0123"))).toBe(
      "Subproject commit abcdef0 added.",
    );
    expect(notTextNote(sub(null), absent)).toBe(
      "Nested git repository removed.",
    );
    expect(notTextNote(absent, big(5))).toBe(
      "Too large to diff (5 bytes; the limit is 1 MiB).",
    );
  });

  it("both too large; text and too large", () => {
    expect(notTextNote(big(5), big(6))).toBe(
      "Too large to diff (5 bytes → 6 bytes; the limit is 1 MiB).",
    );
    expect(notTextNote(text, big(6))).toBe(
      "Too large to diff (head: 6 bytes; the limit is 1 MiB).",
    );
  });

  it("mixed kinds: base and head", () => {
    expect(notTextNote(bin(3), { kind: "symlink", target: "x" })).toBe(
      "Base: binary (3 bytes); head: symbolic link to x.",
    );
    expect(notTextNote(text, bin(3))).toBe(
      "Base: text; head: binary (3 bytes).",
    );
    expect(notTextNote({ kind: "not-local" }, text)).toBe(
      "Not in the local repo (partial clone).",
    );
  });
});

describe("typeChangedNote and the Home words", () => {
  it("says the direction", () => {
    expect(
      typeChangedNote({ kind: "symlink", target: "x" }, { kind: "text" }),
    ).toBe("Changed from a symbolic link to a file.");
    expect(
      typeChangedNote({ kind: "text" }, { kind: "submodule", commit: null }),
    ).toBe("Changed from a file to a subproject.");
    expect(typeChangedNote({ kind: "text" }, { kind: "binary", size: 1 })).toBe(
      "File type changed.",
    );
  });

  it("words", () => {
    expect(changeNoteWords("type-changed")).toBe("type changed");
    expect(changeNoteWords("mode-changed")).toBe("mode changed");
    expect(changeNoteWords("filtered")).toBe("git filter");
  });
});
