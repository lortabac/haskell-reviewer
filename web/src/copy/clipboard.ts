// Copy a text with the clipboard API (SPEC §5.7, D-299). writeText
// must start inside the user's gesture (WebKit): call this at once,
// with no await before it.
export const copiedWords = "Copied.";
export const failedWords = "Couldn't copy: the browser blocked it.";

// say: the status line. Gives true when the text was copied.
export async function copyText(
  text: string,
  say?: (words: string) => void,
): Promise<boolean> {
  let ok = false;
  try {
    const clip = navigator.clipboard as Clipboard | undefined;
    if (clip !== undefined) {
      await clip.writeText(text);
      ok = true;
    }
  } catch {
    ok = false;
  }
  say?.(ok ? copiedWords : failedWords);
  return ok;
}
