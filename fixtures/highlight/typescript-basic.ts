// TS keywords and types on top of the JS rules.
/** A doc comment. */
export interface Shape<T extends object = {}> {
  readonly name: string;
  area(): number;
  tag?: T | undefined;
}

type Key = keyof Shape<{}>;
declare namespace NS { let flag: boolean; }

abstract class Base implements Shape {
  private readonly name = 'base';
  protected abstract area(): number;
}

function isShape(x: unknown): x is Shape {
  const v = x as any satisfies never;
  return typeof v === `object` && v !== null;
}

enum Color { Red = 1, Green = 0b10 }
let big: bigint = 10n, sym: symbol, u: void = undefined;
