// Small runtime decoders for API payloads (D-50). Production uses them,
// and the tests feed them the fixtures the Haskell tests write, so a
// payload that drifts from the types fails here, loudly.

export class DecodeError extends Error {
  constructor(
    readonly path: string,
    readonly expected: string,
  ) {
    super(`bad payload at ${path}: expected ${expected}`);
    this.name = "DecodeError";
  }
}

export type Fields = Readonly<Record<string, unknown>>;

export function object(value: unknown, path: string): Fields {
  if (typeof value !== "object" || value === null || Array.isArray(value)) {
    throw new DecodeError(path, "an object");
  }
  return value as Fields;
}

// A field of an object, decoded. Missing fields are undefined, which
// every decoder rejects.
export function field<T>(
  obj: Fields,
  name: string,
  path: string,
  decode: (value: unknown, path: string) => T,
): T {
  return decode(obj[name], `${path}.${name}`);
}

export function str(value: unknown, path: string): string {
  if (typeof value !== "string") throw new DecodeError(path, "a string");
  return value;
}

export function int(value: unknown, path: string): number {
  if (typeof value !== "number" || !Number.isSafeInteger(value)) {
    throw new DecodeError(path, "an integer");
  }
  return value;
}

// A size, a count or a generation: an integer, 0 or more.
export function nat(value: unknown, path: string): number {
  const n = int(value, path);
  if (n < 0) throw new DecodeError(path, "a non-negative integer");
  return n;
}

export function bool(value: unknown, path: string): boolean {
  if (typeof value !== "boolean") throw new DecodeError(path, "a boolean");
  return value;
}

// null, or what the decoder takes. A missing field is not null.
export function nullable<T>(
  decode: (value: unknown, path: string) => T,
): (value: unknown, path: string) => T | null {
  return (value, path) => (value === null ? null : decode(value, path));
}

export function array<T>(
  decode: (value: unknown, path: string) => T,
): (value: unknown, path: string) => T[] {
  return (value, path) => {
    if (!Array.isArray(value)) throw new DecodeError(path, "an array");
    return value.map((v: unknown, i) => decode(v, `${path}[${i}]`));
  };
}

// One of the given strings.
export function oneOf<T extends string>(
  values: readonly T[],
): (value: unknown, path: string) => T {
  return (value, path) => {
    if (
      typeof value === "string" &&
      (values as readonly string[]).includes(value)
    ) {
      return value as T;
    }
    throw new DecodeError(path, `one of ${values.join(", ")}`);
  };
}
