// A refs link landed before the highlight came, so the token was no
// link yet. Once it comes, land again, but only if the cursor is still
// where we put it and focus is in the view (T12).
export function shouldReland(
  land: { line: number; col: number },
  cursor: { line: number; col: number } | undefined,
  hasFocus: boolean,
): boolean {
  return (
    hasFocus &&
    cursor !== undefined &&
    cursor.line === land.line &&
    cursor.col === land.col
  );
}
