// Payloads of the event stream (SPEC §11.6.2).
import { DecodeError, object } from "./decode";

// The hello event: an empty object for now.
export type Hello = Record<string, never>;

export function decodeHello(value: unknown): Hello {
  object(value, "$");
  return {};
}

// The data of an SSE event, as JSON.
export function parseEventData(data: unknown): unknown {
  if (typeof data !== "string") throw new DecodeError("$", "event data");
  try {
    return JSON.parse(data) as unknown;
  } catch {
    throw new DecodeError("$", "JSON");
  }
}
