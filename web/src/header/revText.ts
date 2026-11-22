// Revision texts shared by the header and Home (D-335, D-338).

// 7 hex, whatever the hash length.
export function shortHash(commit: string): string {
  return commit.slice(0, 7);
}

// For "...": base is the merge base of the two labels.
export function mergeBaseText(base: string, head: string): string {
  return `merge base of ${base} and ${head}`;
}

// "1 of 2" when "..." found several merge bases, else undefined.
export function mergeBaseCount(n: number | null): string | undefined {
  return n !== null && n > 1 ? `1 of ${n}` : undefined;
}
