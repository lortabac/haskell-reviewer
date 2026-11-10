// Copy and selection helpers for the source view specs.
import { expect, type Page } from "@playwright/test";
import { lines } from "./project";

export type At = [line: number, col: number];

// text from (line, col) to (line, col): UTF-16 columns, lines from 1.
export function textSlice(text: string, a: At, b: At): string {
  const ls = lines(text);
  const off = ([n, col]: At) => {
    let o = 0;
    for (let i = 0; i < n - 1; i++) o += (ls[i]?.length ?? 0) + 1;
    return o + col;
  };
  return text.slice(off(a), off(b));
}

// Records what each copy put on the clipboard, after the app's own
// handler (document) has run: window sees the event last.
export async function recordCopies(page: Page): Promise<void> {
  await page.evaluate(() => {
    window.__copies = [];
    window.addEventListener("copy", (e) => {
      window.__copies.push(
        e.defaultPrevented
          ? (e.clipboardData?.getData("text/plain") ?? null)
          : null,
      );
    });
  });
}

// Ctrl+C, and what it put on the clipboard (null: the browser's own).
export async function copy(page: Page): Promise<string | null> {
  const before = await page.evaluate(() => window.__copies.length);
  await page.keyboard.press("ControlOrMeta+c");
  await expect
    .poll(() => page.evaluate(() => window.__copies.length))
    .toBe(before + 1);
  return page.evaluate(
    () => window.__copies[window.__copies.length - 1] ?? null,
  );
}

// Put the selection's anchor ("start") or focus ("extend") at column
// col of line n in the main source view, whatever the spans. Done
// once the app has seen it, so the row is pinned.
export async function selectAt(
  page: Page,
  how: "start" | "extend",
  n: number,
  col: number,
): Promise<void> {
  await page
    .locator(`main .code-view .code-row[data-line="${n}"] .code-cell`)
    .evaluate(
      (cell, [h, c]) => {
        const w = document.createTreeWalker(cell, NodeFilter.SHOW_TEXT);
        let node: Node | undefined;
        let left = c;
        for (let t = w.nextNode(); t !== null; t = w.nextNode()) {
          const len = (t as Text).length;
          if (left <= len) {
            node = t;
            break;
          }
          left -= len;
        }
        if (node === undefined) throw new Error(`no col ${c}`);
        const at = node;
        return new Promise<void>((done) => {
          document.addEventListener("selectionchange", () => done(), {
            once: true,
          });
          const sel = getSelection();
          if (h === "start") sel?.setBaseAndExtent(at, left, at, left);
          else sel?.extend(at, left);
        });
      },
      [how, col] as const,
    );
}

// Select from a to b (both rows mounted).
export async function select(page: Page, a: At, b: At): Promise<void> {
  await selectAt(page, "start", ...a);
  await selectAt(page, "extend", ...b);
}
