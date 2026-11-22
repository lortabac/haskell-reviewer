// The sides of a session: tree in browse mode, base and head in diff
// mode (SPEC §2.2). Every page URL has one: /docs/<side>/A (D-332).
export const sides = ["tree", "base", "head"] as const;
export type Side = (typeof sides)[number];

export function isSide(s: string | undefined): s is Side {
  return (sides as readonly (string | undefined)[]).includes(s);
}

// The other side of a diff session. Tree has none.
export function otherSide(s: Side): Side | undefined {
  switch (s) {
    case "base":
      return "head";
    case "head":
      return "base";
    case "tree":
      return undefined;
  }
}

// The marker of an entry that is only on side `on`, seen from the other
// side: what base alone has was removed, what head alone has was added
// (D-337).
export function ghostChange(on: Side): "added" | "removed" | undefined {
  switch (on) {
    case "base":
      return "removed";
    case "head":
      return "added";
    case "tree":
      return undefined;
  }
}
