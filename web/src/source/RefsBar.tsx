// "References" in the source toolbar (T12): on a declaration it goes to
// the refs page; on a local it opens a bar like find's, over the name's
// binder and uses in this file.
import { useNavigate } from "@solidjs/router";
import {
  createEffect,
  createMemo,
  createSignal,
  createUniqueId,
  type JSX,
  on,
} from "solid-js";
import type { Target } from "../api/source";
import { refsHref } from "../refs/urls";
import type { CodeApi, FindHits } from "./CodeView";
import { nearest, step } from "./find";
import type { Highlight } from "./highlight";
import { localHits } from "./localRefs";
import { binderMap, type RefTarget, refTargetAt } from "./refTarget";

const nameLimit = 30;

export function cut(name: string, max = nameLimit): string {
  const cps = [...name];
  return cps.length <= max ? name : cps.slice(0, max).join("") + "…";
}

export interface RefsBarProps {
  hl: () => Highlight | undefined;
  targets: () => readonly Target[];
  lineText: (n: number) => string;
  api: () => CodeApi | undefined;
  // The hits to draw; undefined when the bar is closed.
  onHits: (hits: FindHits | undefined) => void;
  // Called when the bar opens, before it draws: others close then.
  onOpen?: () => void;
  // Gets a way to close the bar with no focus change.
  handle?: (h: { closeQuietly: () => void }) => void;
}

export function RefsBar(props: RefsBarProps): JSX.Element {
  const barId = createUniqueId();
  const navigate = useNavigate();
  let next: HTMLButtonElement | undefined;
  const [open, setOpen] = createSignal(false);
  const [binder, setBinder] = createSignal<{ line: number; cp: number }>();
  const [name, setName] = createSignal("");
  const [current, setCurrent] = createSignal(0);
  const [said, setSaid] = createSignal("");
  const [note, setNote] = createSignal("");
  const binders = createMemo(() => binderMap(props.targets()));

  const target = createMemo((): RefTarget => {
    const h = props.hl();
    const c = props.api()?.cursor();
    if (h === undefined || c === undefined) return { kind: "none" };
    return refTargetAt(h.runs, props.targets(), binders(), props.lineText, c);
  });
  const label = () => {
    const t = target();
    return t.kind === "decl" || t.kind === "local"
      ? `References to ${cut(t.name)}`
      : "References";
  };
  // A note is for the token it was said about.
  createEffect(on(target, () => setNote(""), { defer: true }));

  const hits = createMemo(() => {
    const h = props.hl();
    const b = binder();
    if (!open() || h === undefined || b === undefined) return undefined;
    return localHits(h.runs, props.targets(), b, props.lineText);
  });
  const total = () => hits()?.matches.count ?? 0;

  const publish = () => {
    const h = hits();
    props.onHits(
      h === undefined || !open()
        ? undefined
        : { matches: h.matches, current: current() },
    );
  };
  const nbsp = "\u00a0";
  const say = (text: string) => {
    const old = said();
    if (text !== "" && old.replace(/\u00a0$/, "") === text) {
      setSaid(old.endsWith(nbsp) ? text : text + nbsp);
    } else setSaid(text);
  };
  const sayCurrent = () => {
    const h = hits();
    if (h === undefined) return;
    const i = current();
    say(
      `Reference ${i + 1} of ${h.matches.count}, line ${h.matches.line[i]}` +
        (i === h.binderAt ? ", the binder" : ""),
    );
  };
  // New hits (the highlight came again): draw them. A name that is
  // gone closes the bar.
  // The current hit stays by its place, else the nearest one.
  createEffect(
    on(hits, (h, before) => {
      if (!open()) return;
      if (h === undefined || h.binderAt < 0) {
        closeQuietly();
        return;
      }
      const i = current();
      const old = before?.matches;
      if (old !== undefined && i < old.count) {
        setCurrent(Math.max(0, nearest(h.matches, old.line[i]!, old.from[i]!)));
      } else setCurrent((c) => Math.min(c, h.matches.count - 1));
      publish();
    }),
  );

  const closeQuietly = () => {
    if (!open()) return;
    setOpen(false);
    setBinder(undefined);
    say("");
    props.onHits(undefined);
  };
  // eslint-disable-next-line solid/reactivity -- given once
  props.handle?.({ closeQuietly });
  const close = () => {
    const h = hits();
    const i = current();
    if (h !== undefined && i < h.matches.count) {
      props.api()?.focusAt(h.matches.line[i]!, h.matches.from[i]!);
    }
    closeQuietly();
  };
  const move = (d: 1 | -1) => {
    const h = hits();
    if (h === undefined) return;
    setCurrent(step(h.matches, current(), d));
    publish();
    sayCurrent();
  };

  const run = () => {
    const t = target();
    setNote("");
    // No stale bar next to a note, or on another page.
    closeQuietly();
    switch (t.kind) {
      case "decl":
        navigate(refsHref(t.ref, t.id));
        return;
      case "local": {
        const h = props.hl();
        const lh =
          h === undefined
            ? undefined
            : localHits(h.runs, props.targets(), t.binder, props.lineText);
        if (lh === undefined || lh.binderAt < 0) {
          setNote("Put the cursor on a name first.");
          return;
        }
        if (lh.matches.count < 2) {
          setNote(`Nothing else uses ${cut(t.name)}.`);
          return;
        }
        props.onOpen?.();
        setName(t.name);
        setBinder(t.binder);
        // The hit under the cursor first, else the binder.
        const c = props.api()?.cursor();
        let at = lh.binderAt;
        if (c !== undefined) {
          for (let i = 0; i < lh.matches.count; i++) {
            if (
              lh.matches.line[i] === c.line &&
              lh.matches.from[i]! <= c.col &&
              c.col < Math.max(lh.matches.to[i]!, lh.matches.from[i]! + 1)
            ) {
              at = i;
            }
          }
        }
        setCurrent(at);
        setOpen(true);
        next?.focus();
        sayCurrent();
        return;
      }
      case "other":
        setNote("References need a name declared in this project.");
        return;
      case "none":
        setNote("Put the cursor on a name first.");
        return;
    }
  };
  const onKey = (e: KeyboardEvent) => {
    if (e.key !== "Escape" || e.isComposing || e.keyCode === 229) return;
    e.preventDefault();
    e.stopPropagation();
    close();
  };

  return (
    <>
      <button
        type="button"
        class="code-refs-button"
        aria-controls={barId}
        onClick={run}
      >
        {label()}
      </button>
      <div
        id={barId}
        class="code-refs-bar"
        role="group"
        aria-label="References in file"
        hidden={!open()}
        onKeyDown={onKey}
      >
        <span class="code-refs-name">{cut(name())}, local</span>
        <button type="button" onClick={() => move(-1)}>
          Previous
        </button>
        <button ref={(el) => (next = el)} type="button" onClick={() => move(1)}>
          Next
        </button>
        <span class="code-refs-count" aria-hidden="true">
          {current() + 1} of {total()}
        </span>
        <button type="button" onClick={close}>
          Close
        </button>
      </div>
      <span class="code-refs-note" role="status">
        {note()}
      </span>
      <p class="sr-only code-refs-status" role="status">
        {said()}
      </p>
    </>
  );
}
