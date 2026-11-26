// Reload in the header (SPEC §5.2, D-354), and its key r. On its own,
// so it is easy to move (T22). Only in sessions that can reload
// (browse mode, --diff REV). The POST only asks: the new generation
// comes back here and in the event stream, and ReloadApply does the
// rest.
import {
  createSignal,
  createUniqueId,
  type JSX,
  Show,
  useContext,
} from "solid-js";
import { postJson } from "../api/client";
import { decodeReloaded } from "../api/reload";
import { canReload } from "../api/session";
import { ConnectionContext } from "../events/ConnectionNotice";
import { useGeneration } from "../events/generation";
import { useKeyActions, useKeys } from "../keys/KeysProvider";
import { usePrefs } from "../prefs/store";
import { useSession } from "../session";

export const reloadTexts = {
  pending: "Reloading…",
  done: "Files reloaded.",
  listing: "Couldn't list the files. See the terminal.",
  busy: "The server can't reload now. See the terminal.",
  unavailable: "Reload isn't available in this session.",
  ended: "This session has ended.",
  other: (message: string) => `Couldn't reload: ${message}.`,
};

// What to say about a failed POST.
export function reloadError(r: { message: string; status?: number }): string {
  switch (r.status) {
    case 500:
      return reloadTexts.listing;
    case 503:
      return reloadTexts.busy;
    case 404:
      return reloadTexts.unavailable;
    default:
      return reloadTexts.other(r.message);
  }
}

export function ReloadButton(): JSX.Element {
  const session = useSession();
  const keys = useKeys();
  const prefs = usePrefs();
  const conn = useContext(ConnectionContext);
  const gen = useGeneration();
  const can = canReload(session);
  const ended = () => conn.status() === "ended";
  // One POST at a time per tab. Other tabs: the server coalesces.
  const [pending, setPending] = createSignal(false);
  const reload = (): void => {
    if (!can) {
      keys?.say(reloadTexts.unavailable);
      return;
    }
    if (ended()) {
      keys?.say(reloadTexts.ended);
      return;
    }
    if (pending()) return;
    setPending(true);
    keys?.say(reloadTexts.pending);
    void postJson("/api/reload", {}, decodeReloaded).then((r) => {
      setPending(false);
      // This tab asked: a change even with no baseline yet.
      if (r.kind === "ok") gen.seen(r.value.generation, true);
      // postJson never says "not-found".
      else if (r.kind === "error") keys?.say(reloadError(r));
    });
  };
  useKeyActions({
    reload: () => {
      reload();
      return true;
    },
  });
  const off = () => pending() || ended();
  const id = createUniqueId();
  // Off: aria-disabled but focusable; ended: the reason in a tip and
  // said on use (as SideSwitch).
  return (
    <Show when={can}>
      <button
        type="button"
        class="reload-button"
        aria-disabled={off() ? "true" : undefined}
        aria-describedby={ended() ? id : undefined}
        data-tip={ended() ? reloadTexts.ended : undefined}
        onClick={reload}
      >
        Reload{prefs.singleKeyShortcuts() ? " (r)" : ""}
      </button>
      <Show when={ended()}>
        <span id={id} hidden>
          {reloadTexts.ended}
        </span>
      </Show>
    </Show>
  );
}
