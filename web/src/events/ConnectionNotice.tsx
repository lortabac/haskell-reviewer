import {
  createContext,
  For,
  useContext,
  type Accessor,
  type JSX,
} from "solid-js";
import type { Status } from "./protocol";

// The state of the event stream, as the tab sees it.
export interface Connection {
  status: Accessor<Status>;
  // True if this tab has its own stream (no working SharedWorker).
  fallback: Accessor<boolean>;
}

// Default: connecting, no fallback. Shows nothing.
const idle: Connection = {
  status: () => "connecting",
  fallback: () => false,
};

export const ConnectionContext = createContext<Connection>(idle);

export const noticeTexts = {
  reconnecting: "Connection lost. Reconnecting…",
  ended: "This session has ended. Run haskell-reviewer again.",
  fallback:
    "This browser can't share one connection between tabs, so each tab keeps its own. With more than about five tabs open, pages may stop loading.",
};

// The notices about the connection. The live region is always there,
// so screen readers announce the text when it shows up.
export function ConnectionNotice(): JSX.Element {
  const conn = useContext(ConnectionContext);
  const texts = (): string[] => {
    const out: string[] = [];
    const status = conn.status();
    if (status === "reconnecting") out.push(noticeTexts.reconnecting);
    if (status === "ended") out.push(noticeTexts.ended);
    if (conn.fallback()) out.push(noticeTexts.fallback);
    return out;
  };
  return (
    <div class="connection-notices" role="status">
      <For each={texts()}>
        {(text) => <p class="connection-notice">{text}</p>}
      </For>
    </div>
  );
}
