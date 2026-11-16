// What the search box text asks for (D-309). Pure.
//   foo     names (D-206), plus a "Search text for 'foo'" row
//   "fo     text, quote not closed yet: no request
//   "foo"   text (D-307), the part between the quotes as is
// Leading spaces never count. Trailing ones count inside an open quote
// (the user is typing there), not after a closing one. Smart quotes
// (“foo”) are plain names.
export type BoxQuery =
  | { kind: "name"; text: string }
  | { kind: "open"; text: string }
  | { kind: "text"; text: string };

export function parseBoxQuery(raw: string): BoxQuery {
  const t = raw.trimStart();
  if (!t.startsWith('"')) return { kind: "name", text: t.trimEnd() };
  const closed = t.trimEnd();
  if (closed.length >= 2 && closed.endsWith('"')) {
    return { kind: "text", text: closed.slice(1, -1) };
  }
  return { kind: "open", text: t.slice(1) };
}

// The box text for a text search.
export function quoted(text: string): string {
  return `"${text}"`;
}

// The server's limit for a text query (D-307), in code points.
export const maxTextLength = 128;

export function textTooLong(text: string): boolean {
  return [...text].length > maxTextLength;
}

export const textTooLongText = `Text too long: at most ${maxTextLength} characters.`;
