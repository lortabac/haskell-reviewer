import type { Session } from "../src/api/session";

// A diff session, for tests.
export const diffSession: Session = {
  mode: "diff",
  spec: "main..feature",
  base: { label: "main", commit: "a".repeat(40) },
  head: { label: "feature", commit: "b".repeat(40) },
  mergeBases: null,
};

// --diff REV: head is the working tree (reload works, D-354).
export const worktreeSession: Session = {
  mode: "diff",
  spec: "main",
  base: { label: "main", commit: "a".repeat(40) },
  head: { label: "working tree", commit: null },
  mergeBases: null,
};
