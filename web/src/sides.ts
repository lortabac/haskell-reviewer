// The sides of a session: tree in browse mode, base and head in diff
// mode (SPEC §2.2). Every page URL has one: /docs/<side>/A (D-332).
export const sides = ["tree", "base", "head"] as const;
export type Side = (typeof sides)[number];

export function isSide(s: string | undefined): s is Side {
  return (sides as readonly (string | undefined)[]).includes(s);
}
