// The snapshot generation this tab is on (D-354). Every source counts:
// the generation event, indexing events, the POST api/reload answer
// and the answers of pages and providers. The first value is the
// baseline. A rise from a known value is a reload, whoever started it
// (this tab, another tab, curl): Root applies it (the page mounts
// again at its place, providers refetch). So is a change (the POST
// answer, a live generation event) with no baseline yet: a start-up
// listing that failed sends no generation at all. An answer older than
// the current generation means a page fetched before the reload: it is
// applied too, once per generation, with no notice.
import {
  type Accessor,
  createContext,
  createEffect,
  createSignal,
  untrack,
  useContext,
} from "solid-js";
import type { Side } from "../sides";

// One apply. n counts up from 1. notice: say "Files reloaded." (a
// rise). Else an old answer: silent.
export interface Apply {
  n: number;
  notice: boolean;
}

export interface GenerationState {
  // The newest generation seen. Undefined: none yet.
  current: Accessor<number | undefined>;
  // The last apply. Undefined: none yet.
  applied: Accessor<Apply | undefined>;
  // A generation from any source. change: it says a reload happened
  // (the POST answer, a live generation event).
  seen: (g: number, change?: boolean) => void;
}

export const idleGeneration: GenerationState = {
  current: () => undefined,
  applied: () => undefined,
  seen: () => {},
};

// Idle unless main.tsx gives one, so tests need no provider.
export const GenerationContext = createContext<GenerationState>(idleGeneration);

export function useGeneration(): GenerationState {
  return useContext(GenerationContext);
}

export function createGenerationState(): GenerationState {
  const [current, setCurrent] = createSignal<number>();
  const [applied, setApplied] = createSignal<Apply>();
  let n = 0;
  // The generation the last apply was for: one apply per generation.
  let appliedFor: number | undefined;
  const apply = (g: number, notice: boolean) => {
    appliedFor = g;
    n++;
    setApplied({ n, notice });
  };
  const seen = (g: number, change = false) => {
    const c = untrack(current);
    if (c === undefined) {
      setCurrent(g);
      if (change) apply(g, true);
    } else if (g > c) {
      setCurrent(g);
      apply(g, true);
    } else if (g < c && appliedFor !== c) apply(c, false);
  };
  return { current, applied, seen };
}

// A page's or provider's answer: tell the generation state. g:
// undefined while there is none.
export function useAnswerGeneration(g: () => number | undefined): void {
  const gen = useGeneration();
  createEffect(() => {
    const x = g();
    if (x !== undefined) gen.seen(x);
  });
}

// How many applies so far: providers refetch when it changes. 0
// before any.
export function useReloads(): Accessor<number> {
  const gen = useGeneration();
  return () => gen.applied()?.n ?? 0;
}

// A per-side cache that a reload empties (D-354). n: useReloads().
export function reloadCache<V>(): {
  get: (s: Side, n: number) => V | undefined;
  set: (s: Side, n: number, v: V) => void;
} {
  const m = new Map<Side, V>();
  let of = 0;
  return {
    get: (s, n) => {
      if (n !== of) {
        m.clear();
        of = n;
      }
      return m.get(s);
    },
    // A late answer of an older reload is not kept.
    set: (s, n, v) => {
      if (n === of) m.set(s, v);
    },
  };
}
