// Keys of the source view, as pure functions.

export interface Mods {
  ctrl: boolean;
  meta: boolean;
  alt: boolean;
  shift: boolean;
}

// The line a key moves the cursor to (clamped into 1..count), or
// undefined if the key isn't ours. page: lines on screen less one.
export function keyTarget(
  key: string,
  mods: Mods,
  line: number,
  count: number,
  page: number,
): number | undefined {
  if (mods.alt || mods.shift || mods.meta) return undefined;
  const to: number | undefined = mods.ctrl
    ? { Home: 1, End: count }[key]
    : {
        ArrowUp: line - 1,
        ArrowDown: line + 1,
        PageUp: line - page,
        PageDown: line + page,
        Home: 1,
        End: count,
      }[key];
  return to === undefined ? undefined : Math.min(Math.max(1, to), count);
}

// Ctrl+A, or Cmd+A on a Mac.
export function isSelectAll(key: string, mods: Mods): boolean {
  return (
    (mods.ctrl || mods.meta) &&
    !mods.alt &&
    !mods.shift &&
    key.toLowerCase() === "a"
  );
}

// A link on the cursor's line: its UTF-16 columns, end exclusive.
export interface Stop {
  start: number;
  end: number;
}

// Where the cursor is on its line: the cell at a column, or a link
// (by its start).
export interface Spot {
  link: boolean;
  col: number;
}

// The next (dir 1) or previous (dir −1) spot for → and ←, undefined at
// the end (scroll sideways). The cell at c sits just before a link
// starting at c: → from it goes to that link, ← to links before c.
// ← from the first link goes to the cell at its start. stops in order.
export function nextStop(
  stops: readonly Stop[],
  at: Spot,
  dir: 1 | -1,
): Spot | undefined {
  // The cell at c is c − ½, a link at s is s.
  const pos = at.link ? at.col : at.col - 0.5;
  if (dir === 1) {
    const s = stops.find((s) => s.start > pos);
    return s === undefined ? undefined : { link: true, col: s.start };
  }
  const before = stops.filter((s) => s.start < pos);
  const s = before[before.length - 1];
  if (s !== undefined) return { link: true, col: s.start };
  return at.link ? { link: false, col: at.col } : undefined;
}

// Where ↑, ↓, Page and Home/End land: the link under the goal column,
// else the cell at it.
export function landOn(stops: readonly Stop[], goal: number): Spot {
  const s = stops.find((s) => s.start <= goal && goal < s.end);
  return s === undefined
    ? { link: false, col: goal }
    : { link: true, col: s.start };
}
