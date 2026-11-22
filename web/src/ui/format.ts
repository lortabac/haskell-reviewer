// Sizes and counts as text.

export function bytes(n: number): string {
  return n === 1 ? "1 byte" : `${n.toLocaleString("en-US")} bytes`;
}

// A size limit: in MiB when it is a whole number of them.
export function limit(n: number): string {
  const mib = 1024 * 1024;
  return n > 0 && n % mib === 0 ? `${n / mib} MiB` : bytes(n);
}

// "1 file", "1,200 files".
export function plural(n: number, one: string, many: string): string {
  return `${n.toLocaleString("en-US")} ${n === 1 ? one : many}`;
}
