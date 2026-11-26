// Seeded random data for property tests.
import type { DiffRun } from "../src/api/diff";

// A small seeded generator (mulberry32): numbers in [0, 1).
export function rng(seed: number): () => number {
  let a = seed;
  return () => {
    a = (a + 0x6d2b79f5) | 0;
    let t = Math.imul(a ^ (a >>> 15), 1 | a);
    t = (t + Math.imul(t ^ (t >>> 7), 61 | t)) ^ t;
    return ((t ^ (t >>> 14)) >>> 0) / 4294967296;
  };
}

// Up to 7 runs of 1-4 rows each, any op.
export function randomRuns(next: () => number): DiffRun[] {
  const ops = ["equal", "delete", "insert"] as const;
  const out: DiffRun[] = [];
  const n = Math.floor(next() * 8);
  for (let i = 0; i < n; i++) {
    out.push([ops[Math.floor(next() * 3)]!, 1 + Math.floor(next() * 4)]);
  }
  return out;
}
