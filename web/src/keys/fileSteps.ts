// ] and [ (D-350): what a view's step gives, and the stepping. Pure.

// The next or previous changed file or module, in the same view (words
// said there), or words why not.
export type FileStep = { href: string; words: string } | string;

// The first i in 0..n with cmp(i) >= 0 (cmp ascending).
function lowerBound(n: number, cmp: (i: number) => number): number {
  let lo = 0;
  let hi = n;
  while (lo < hi) {
    const mid = (lo + hi) >>> 1;
    if (cmp(mid) < 0) lo = mid + 1;
    else hi = mid;
  }
  return lo;
}

// The next (1) or previous (-1) item of a sorted list, from a place
// that may not be in it. cmp(i): item i against that place. No wrap:
// undefined past the ends.
export function stepIndex(
  n: number,
  cmp: (i: number) => number,
  dir: 1 | -1,
): number | undefined {
  const lo = lowerBound(n, cmp);
  const here = lo < n && cmp(lo) === 0;
  const i = dir === 1 ? (here ? lo + 1 : lo) : lo - 1;
  return i >= 0 && i < n ? i : undefined;
}
