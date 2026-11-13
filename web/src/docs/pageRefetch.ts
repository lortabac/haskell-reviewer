// A partial module page, asked again while indexing (D-286, D-293).
// The page on screen stays the first answer: a new one only changes
// links (relink.ts), so nothing is drawn again but the links that
// changed. An answer that differs in anything else is dropped and the
// page stops asking (it is stale until a reload). Failed answers keep
// the old links.
import { batch, createSignal } from "solid-js";
import type { CodeToken } from "../api/code";
import { getCbor, sameBytes } from "../api/client";
import {
  decodeModulePage,
  type ModulePage,
  type PageTarget,
} from "../api/module";
import { createRefetchLoop } from "../events/refetchLoop";
import { type NameInline, type Overlay, relink } from "./relink";
import { apiModuleUrl } from "./urls";

export interface PageLinks {
  // The targets, merged (indices never change).
  targets: () => readonly PageTarget[];
  // A token or name of the first answer, as it is now. Tracked.
  token: (t: CodeToken) => CodeToken;
  name: (n: NameInline) => NameInline;
}

// first: a parsed, ok answer; firstBytes: its body, if known. before/
// after: called around a change (focus, DocsView).
export function createPageRefetch(
  typedRef: string,
  first: ModulePage,
  firstBytes: Uint8Array | undefined,
  around: { before: () => void; after: () => void },
): PageLinks {
  const links = new Map<object, CodeToken | NameInline>();
  let now: Overlay = {
    partial: first.partial,
    targets: first.parse.kind === "parsed" ? first.parse.targets : [],
    links,
  };
  const [targets, setTargets] = createSignal(now.targets);
  // Bumped on each change: token and name read it.
  const [rev, setRev] = createSignal(0);
  // The body of the last answer taken, and of the one being read. The
  // same bytes again: nothing to decode or walk (a big page takes
  // ~0.5 s for both, D-293).
  let last = firstBytes;
  let reading: Uint8Array | undefined;
  createRefetchLoop(
    (signal) =>
      getCbor<ModulePage | "same">(apiModuleUrl(typedRef), decodeModulePage, {
        signal,
        raw: (b) => {
          if (last !== undefined && sameBytes(b, last)) return "same";
          reading = b;
          return undefined;
        },
      }),
    (r) => {
      // Never kept past here: one big body is enough.
      const body = reading;
      reading = undefined;
      if (r.kind !== "ok") return undefined;
      const next = r.value;
      if (next === "same") {
        return { generation: first.generation, partial: now.partial };
      }
      const d = relink(first, now, next);
      // Done (not partial) or stopped: no more asks, drop the bytes.
      last = d !== "other" && next.partial ? body : undefined;
      if (d === "other") return "stop";
      if (d === "same") return next;
      for (const [k, v] of d.changes) links.set(k, v);
      now = { partial: d.partial, targets: d.targets, links };
      if (d.changes.size > 0 || d.targets !== targets()) {
        around.before();
        batch(() => {
          setTargets(d.targets);
          setRev((x) => x + 1);
        });
        around.after();
      }
      return next;
    },
    first,
  );
  return {
    targets,
    token: (t) => {
      rev();
      return (links.get(t) as CodeToken | undefined) ?? t;
    },
    name: (n) => {
      rev();
      return (links.get(n) as NameInline | undefined) ?? n;
    },
  };
}
