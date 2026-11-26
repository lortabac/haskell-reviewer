// The source view's reload landing (D-356): where the old page was
// (here), and the landing on the new page by the outline.
//
// Before the outline comes: the old lines, clamped. When it comes:
// again by declaration, if the cursor and the hash are still where we
// put them. Focus then moves only if it is still in the view; if the
// view was scrolled meanwhile, only the cursor moves.
import { useLocation, useNavigate } from "@solidjs/router";
import { type Accessor, createEffect, on, onCleanup, untrack } from "solid-js";
import type { OutlineEntry } from "../api/source";
import { useReloadKeep } from "../events/ReloadApply";
import { lineHash } from "../files/paths";
import { useKeys, type ViewTarget } from "../keys/KeysProvider";
import { bringOnPage } from "../ui/scroll";
import type { CodeApi } from "./CodeView";
import {
  lineOf,
  placeOf,
  type SourceKeep,
  sourceKeepFrom,
  type SourcePlace,
} from "./keepLine";

// How a landing goes: focus the cursor, scroll to it.
interface How {
  focus: boolean;
  scroll: boolean;
}

// wait: not landed yet. fixing: the URL is being fixed (the marks
// moved), we come back. landed: the cursor is at `at`; top: the top
// line on screen then.
type Step =
  | { step: "wait"; keep: SourceKeep }
  | { step: "fixing"; keep: SourceKeep; hash: string; how: How }
  | {
      step: "landed";
      keep: SourceKeep;
      hash: string;
      at: { line: number; col: number };
      top?: number;
    };

export function createKeepLanding(o: {
  api: () => CodeApi | undefined;
  outline: Accessor<readonly OutlineEntry[] | undefined>;
  hl: Accessor<unknown>;
  marks: Accessor<{ from: number; to: number } | undefined>;
  count: () => number;
  // The page's href, no hash.
  href: () => string;
}): {
  // In the marks effect: true, it took this landing.
  land: (a: CodeApi) => boolean;
  here: () => ViewTarget | undefined;
} {
  const location = useLocation();
  const navigate = useNavigate();
  const keys = useKeys();
  const reloadKeep = useReloadKeep();
  const first = sourceKeepFrom(reloadKeep(untrack(() => location.state))?.keep);
  let st: Step | undefined =
    first === undefined ? undefined : { step: "wait", keep: first };

  const at = (p: SourcePlace) => lineOf(p, untrack(o.outline), o.count());
  const keptHash = (k: SourceKeep) => {
    const m = k.marks?.map(at);
    return m === undefined ? "" : lineHash(Math.min(...m), Math.max(...m));
  };

  // Lands, or fixes the URL first.
  const go = (a: CodeApi, keep: SourceKeep, how: How): void => {
    const hash = keptHash(keep);
    if (hash !== location.hash) {
      st = { step: "fixing", keep, hash, how };
      keys?.keepStatus();
      navigate(o.href() + location.search + hash, {
        replace: true,
        scroll: false,
        state: untrack(() => location.state),
      });
      return;
    }
    const line = at(keep.cursor);
    if (how.focus) a.focusAt(line, keep.col);
    else a.setCursor(line, keep.col);
    if (how.scroll) scrollTo(a, keep, line);
    const c = a.cursor();
    // By the outline: done.
    const landed: Step | undefined =
      untrack(o.hl) === undefined
        ? { step: "landed", keep, hash, at: { line: c.line, col: c.col } }
        : undefined;
    st = landed;
    const frame = requestAnimationFrame(() => {
      if (landed !== undefined) landed.top = a.onScreen()?.from;
      const r = a.row(line);
      if (how.scroll && r !== undefined) bringOnPage(r);
    });
    onCleanup(() => cancelAnimationFrame(frame));
  };

  // The old top line at the top, if the cursor fits under it. Else
  // the cursor in the middle.
  const scrollTo = (a: CodeApi, keep: SourceKeep, line: number) => {
    const t = keep.top;
    const top = t === undefined ? undefined : at(t.place);
    if (t !== undefined && top !== undefined) {
      if (top <= line && line <= top + t.below) {
        a.scrollToLine(top, "start");
        return;
      }
    }
    a.scrollToLine(line, "center");
  };

  const land = (a: CodeApi): boolean => {
    const s = st;
    if (s === undefined) return false;
    const r = reloadKeep(untrack(() => location.state));
    st = undefined;
    // Left that landing (a link, Back), or landed already: plain
    // landings from now on.
    if (r === undefined || s.step === "landed") return false;
    if (s.step === "wait") go(a, s.keep, { focus: r.focus, scroll: true });
    else if (location.hash === s.hash) go(a, s.keep, s.how);
    else return false;
    return true;
  };

  // The cursor or the hash is not where we put them any more.
  const moved = (a: CodeApi, s: Step & { step: "landed" }): boolean => {
    const c = a.cursor();
    return (
      c.line !== s.at.line || c.col !== s.at.col || location.hash !== s.hash
    );
  };

  // The outline came after we landed.
  createEffect(
    on(
      o.hl,
      (h) => {
        const s = st;
        const a = o.api();
        if (h === undefined || a === undefined || s?.step !== "landed") return;
        st = undefined;
        const r = reloadKeep(untrack(() => location.state));
        if (r === undefined || moved(a, s)) return;
        // Scrolled meanwhile: only the cursor moves (focusAt would
        // scroll too).
        const still = s.top === undefined || a.onScreen()?.from === s.top;
        go(a, s.keep, {
          focus: still && r.focus && a.hasFocus(),
          scroll: still,
        });
      },
      { defer: true },
    ),
  );

  // Where a reload of this page lands, from what it keeps.
  const target = (keep: SourceKeep): ViewTarget => {
    const href = o.href() + keptHash(keep);
    const m = keep.marks;
    if (m === undefined) return { href, state: { keep } };
    // A later Back or Forward to this entry (keep not live then) lands
    // on the first marked line: at the cursor's column only if the
    // cursor is on that line (D-339).
    const col = keep.cursor.line === m[0].line ? keep.col : 0;
    return { href, state: { keep, origin: { col } } };
  };

  // The cursor, the marked lines and the top line on screen, each by
  // its declaration. No marks, no hash: a page that was never touched
  // stays at its top line, not line 1 (T17a review N3). A landing still
  // waiting for its outline (nothing moved) keeps what it came with:
  // its places are by the older outline, and ours would be by line.
  const here = (): ViewTarget | undefined => {
    const a = o.api();
    if (a === undefined) return undefined;
    const p = st;
    if (p !== undefined && (p.step !== "landed" || !moved(a, p))) {
      return target(p.keep);
    }
    const c = a.cursor();
    const ol = untrack(o.outline) ?? [];
    const m = untrack(o.marks);
    const s = a.onScreen();
    const out: SourceKeep = { cursor: placeOf(ol, c.line), col: c.col };
    if (m !== undefined) out.marks = [placeOf(ol, m.from), placeOf(ol, m.to)];
    if (s !== undefined) {
      out.top = { place: placeOf(ol, s.from), below: s.to - s.from };
    }
    return target(out);
  };

  return { land, here };
}
