// The key dispatcher (D-297): tinykeys does the matching (sequences,
// Shift); we decide which events it never sees. Attach the returned
// listener to document, bubble phase. Component keys are mostly Solid
// delegated handlers, also on document, so the phase alone doesn't
// order them: what keeps us out of their way is the defaultPrevented
// check. A component key must call preventDefault, not only
// stopPropagation. (The hover card's Esc is a capture listener and
// stops the event, so it goes first anyway.)
import { createKeybindingsHandler } from "tinykeys";
import {
  type ActionId,
  keymap,
  type KeyRow,
  repeatable,
  sequenceTimeout,
} from "./keymap";

export interface DispatcherOpts {
  // Shortcuts on (the pref).
  enabled: () => boolean;
  // Our overlay is open: it handles its own keys.
  dialogOpen: () => boolean;
  // Run an action. True: done, the key's default is stopped.
  run: (id: ActionId, e: KeyboardEvent) => boolean;
  rows?: readonly KeyRow[];
  timeout?: number;
}

const textInputs = new Set([
  "text",
  "search",
  "email",
  "url",
  "tel",
  "password",
  "number",
  "date",
  "datetime-local",
  "month",
  "week",
  "time",
]);

const textRoles = new Set(["textbox", "searchbox", "combobox"]);

// A place where keys type text (or pick a value): keys stay there.
export function isEditable(t: EventTarget | null): boolean {
  if (!(t instanceof HTMLElement)) return false;
  if (t.isContentEditable) return true;
  if (t instanceof HTMLTextAreaElement || t instanceof HTMLSelectElement) {
    return true;
  }
  if (t instanceof HTMLInputElement) return textInputs.has(t.type);
  const role = t.getAttribute("role");
  return role !== null && textRoles.has(role);
}

// True: the dispatcher leaves this event alone.
export function ignored(
  e: KeyboardEvent,
  opts: Pick<DispatcherOpts, "enabled" | "dialogOpen">,
): boolean {
  if (!opts.enabled() || opts.dialogOpen()) return true;
  // A component took it (see the top of this file).
  if (e.defaultPrevented) return true;
  // 229: Safari's end of a composition.
  if (e.isComposing || e.keyCode === 229) return true;
  if (e.ctrlKey || e.metaKey || e.altKey) return true;
  if (e.repeat && !repeatable.has(e.key.toLowerCase())) return true;
  return isEditable(e.target);
}

const isSequence = (r: KeyRow) => r.bindings.some((b) => b.includes(" "));

export function createDispatcher(opts: DispatcherOpts): EventListener {
  const map: Record<string, (e: KeyboardEvent) => void> = {};
  let fired = false;
  // Sequences first: tinykeys stops at the first binding that fires,
  // so a single key listed before g d would leave g d waiting.
  const rows = [...(opts.rows ?? keymap)].sort(
    (a, b) => Number(isSequence(b)) - Number(isSequence(a)),
  );
  for (const row of rows) {
    for (const b of row.bindings) {
      map[b] = (e) => {
        fired = true;
        if (opts.run(row.id, e)) e.preventDefault();
      };
    }
  }
  const make = () =>
    createKeybindingsHandler(map, {
      timeout: opts.timeout ?? sequenceTimeout,
      ignore: (e) => ignored(e, opts),
    });
  let handler = make();
  return (e) => {
    handler(e);
    // tinykeys keeps the other half-done sequences after one fires
    // (g d leaves g r waiting for r): start fresh.
    if (fired) {
      fired = false;
      handler = make();
    }
  };
}
