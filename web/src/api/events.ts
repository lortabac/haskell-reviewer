// Payloads of the event stream (SPEC §11.6.2).
import { sides, type Side } from "../sides";
import { DecodeError, field, nat, object, oneOf, positive } from "./decode";

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

// The indexing event, and GET api/indexing/<side>: how far indexing is
// on one side. Done when ready + failed === total.
export type Indexing = {
  generation: number;
  side: Side;
  total: number;
  ready: number;
  failed: number;
};

export function decodeIndexing(value: unknown, path = "$"): Indexing {
  const o = object(value, path);
  const indexing = {
    generation: field(o, "generation", path, nat),
    side: field(o, "side", path, oneOf(sides)),
    total: field(o, "total", path, nat),
    ready: field(o, "ready", path, nat),
    failed: field(o, "failed", path, nat),
  };
  if (indexing.ready + indexing.failed > indexing.total) {
    throw new DecodeError(path, "ready + failed <= total");
  }
  return indexing;
}

// The generation event: a reload swapped in a new generation (D-351).
// It comes before that generation's indexing events.
export type GenerationEvent = { generation: number };

export function decodeGeneration(value: unknown, path = "$"): GenerationEvent {
  const o = object(value, path);
  return { generation: field(o, "generation", path, positive) };
}
