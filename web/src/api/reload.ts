// The answer to POST api/reload (D-351): the new generation, the same
// shape as the generation event. See encodeReloaded in
// HaskellReviewer.Server.Wire.
import { decodeGeneration, type GenerationEvent } from "./events";

export type Reloaded = GenerationEvent;

export function decodeReloaded(value: unknown, path = "$"): Reloaded {
  return decodeGeneration(value, path);
}
