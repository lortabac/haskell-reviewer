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
