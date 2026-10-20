// Messages from the events worker (or the per-tab hub) to a tab.
// See SPEC §11.6.2.

export type Status = "connecting" | "open" | "reconnecting" | "ended";

export type WorkerMessage =
  | { type: "status"; status: Status }
  | { type: "hello" }
  // The worker has no EventSource (some browsers). The tab falls back
  // to its own stream.
  | { type: "unsupported" };

const statuses: readonly string[] = [
  "connecting",
  "open",
  "reconnecting",
  "ended",
];

function isStatus(value: unknown): value is Status {
  return typeof value === "string" && statuses.includes(value);
}

// Checks a message that came through a MessagePort. Undefined if it is
// not one of ours.
export function decodeWorkerMessage(data: unknown): WorkerMessage | undefined {
  if (typeof data !== "object" || data === null) return undefined;
  const msg = data as Record<string, unknown>;
  switch (msg.type) {
    case "status":
      return isStatus(msg.status)
        ? { type: "status", status: msg.status }
        : undefined;
    case "hello":
      return { type: "hello" };
    case "unsupported":
      return { type: "unsupported" };
    default:
      return undefined;
  }
}
