// The order of ] and [ (D-350): changed files in sidebar order. Pure.
import type { DiffFiles } from "../api/diffFiles";
import type { FileMarks } from "../files/changes";
import { compareTreePaths, sortTreePaths } from "../files/tree";
import { type FileStep, stepIndex } from "../keys/fileSteps";
import { memoBy } from "../memo";
import { fileStepTexts } from "./words";

// The step from path in a list in file tree order.
export function stepPath(
  paths: readonly string[],
  path: string,
  dir: 1 | -1,
): number | undefined {
  return stepIndex(
    paths.length,
    (i) => compareTreePaths(paths[i] ?? "", path),
    dir,
  );
}

export interface DiffOrder {
  // Every listed file at its head path (base path if removed).
  paths: readonly string[];
  // A rename's base path -> its head path.
  heads: ReadonlyMap<string, string>;
}

// The diff view's order: what the sidebar shows on head, ghosts too.
// Made once per answer: a list can have 100k files.
export const diffOrder = memoBy((files: DiffFiles): DiffOrder => {
  const heads = new Map<string, string>();
  for (const f of files.files) {
    if (f.oldPath !== null) heads.set(f.oldPath, f.path);
  }
  return { paths: sortTreePaths(files.files.map((f) => f.path)), heads };
});

// The source view's order: this side's own marked files. No ghosts:
// they are on the other side (no silent side switch).
export const sourceOrder = memoBy((marks: FileMarks): readonly string[] =>
  sortTreePaths(marks.marks.keys()),
);

// ] or [ from path over paths (in tree order): the file there, or why
// not.
export function fileStep(
  paths: readonly string[],
  path: string,
  dir: 1 | -1,
  href: (path: string) => string,
): FileStep {
  if (paths.length === 0) return fileStepTexts.noFiles;
  const i = stepPath(paths, path, dir);
  const to = i === undefined ? undefined : paths[i];
  if (i === undefined || to === undefined) return fileStepTexts.noMoreFiles;
  return {
    href: href(to),
    words: `${to}, changed file ${i + 1} of ${paths.length}.`,
  };
}
