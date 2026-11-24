// The find toolbar above the source view (T09, G11): the Find button,
// the hint and, when open, the bar. Enter in the input moves to the
// next hit and stays in the input; Esc or Close puts focus on the
// current hit. The count goes to a live region. The diff view uses it
// too, with its own Finder (diff/find.ts).
import {
  createComputed,
  createEffect,
  createMemo,
  createSignal,
  createUniqueId,
  type JSX,
  on,
  onCleanup,
  untrack,
} from "solid-js";
import {
  type Finder,
  type FindHits,
  type Matches,
  nearest,
  noMatches,
  status as lineStatus,
  step,
} from "./find";

// Typing: say the count this long after the last key.
export const typingDelay = 500;

// What the bar needs from the view, in the finder's lines.
export interface FindApi {
  cursor: () => { line: number; col: number } | undefined;
  focusAt: (line: number, col: number) => void;
}

export interface FindBarProps {
  // A new finder while open: the same query again, from the cursor.
  finder: () => Finder;
  api: () => FindApi | undefined;
  // For the live region; default: "Match 3 of 12, line 40".
  status?: ((m: Matches, i: number) => string) | undefined;
  // The hits to draw; undefined when the bar is closed.
  onFind: (hits: FindHits | undefined) => void;
  // More for the toolbar, after the Find button (the references bar).
  children?: JSX.Element;
  // Called when the bar opens, before it draws: others close then.
  onOpen?: () => void;
  // Gets ways to drive the bar (the f and Esc keys).
  handle?: (h: FindHandle) => void;
  // Shown after "Find" in the hint, like "(f)".
  keyHint?: string | undefined;
  // Only the children show: no button, bar or hint (a diff with no
  // rows). The toolbar stays, so focus on a child stays too.
  off?: boolean | undefined;
}

export interface FindHandle {
  // Close with no focus change.
  closeQuietly: () => void;
  // Open, or put focus in the input if it is open.
  open: () => void;
  isOpen: () => boolean;
  // Close; focus goes to the current hit, like Esc in the bar.
  close: () => void;
}

export function FindBar(props: FindBarProps): JSX.Element {
  const barId = createUniqueId();
  let root!: HTMLDivElement;
  let button!: HTMLButtonElement;
  let barEl!: HTMLDivElement;
  let input!: HTMLInputElement;
  const [open, setOpen] = createSignal(false);
  const [query, setQuery] = createSignal("");
  const [matches, setMatches] = createSignal<Matches>(noMatches);
  const [current, setCurrent] = createSignal(-1);
  const [said, setSaid] = createSignal("");
  // The cursor when the bar opened: the first hit is after it.
  let origin = { line: 1, col: 0 };
  let timer: ReturnType<typeof setTimeout> | undefined;

  const hits = createMemo((): FindHits | undefined =>
    open() ? { matches: matches(), current: current() } : undefined,
  );
  // A new object per move: it scrolls to the hit again.
  const send = () => {
    const h = hits();
    props.onFind(h === undefined ? undefined : { ...h });
  };
  const publish = (m: Matches, cur: number) => {
    setMatches(m);
    setCurrent(cur);
    send();
  };

  const nbsp = "\u00a0";
  // The same text again isn't read: it gets a no-break space at the
  // end, or loses it. Clearing first is too quick for some readers.
  const say = (text: string) => {
    const old = said();
    if (text !== "" && old.replace(/\u00a0$/, "") === text) {
      setSaid(old.endsWith(nbsp) ? text : text + nbsp);
    } else setSaid(text);
  };
  const sayNow = (m: Matches, cur: number) => {
    clearTimeout(timer);
    say(query() === "" ? "" : (props.status ?? lineStatus)(m, cur));
  };
  onCleanup(() => {
    clearTimeout(timer);
  });

  const find = (q: string) => {
    setQuery(q);
    const m = props.finder().search(q);
    const cur = nearest(m, origin.line, origin.col);
    publish(m, cur);
    clearTimeout(timer);
    timer = setTimeout(() => sayNow(m, cur), typingDelay);
  };
  // A new finder (the diff's layout or rows changed): search again.
  // Open: from the cursor, and say the count. Closed: only the hits.
  createEffect(
    on(
      () => props.finder(),
      (f) => {
        if (!open()) {
          setMatches(f.search(query()));
          return;
        }
        const c = props.api()?.cursor();
        if (c !== undefined) origin = c;
        find(query());
      },
      { defer: true },
    ),
  );
  const move = (d: 1 | -1) => {
    const m = matches();
    if (m.count === 0) return;
    const cur = step(m, current(), d);
    publish(m, cur);
    sayNow(m, cur);
  };

  const show = () => {
    props.onOpen?.();
    const c = props.api()?.cursor();
    origin = c ?? { line: 1, col: 0 };
    setOpen(true);
    // The old query stays: its hits again, from the cursor.
    const m = matches();
    const cur = nearest(m, origin.line, origin.col);
    publish(m, cur);
    input.focus();
    input.select();
    sayNow(m, cur);
  };
  const close = () => {
    const m = matches();
    const cur = current();
    clearTimeout(timer);
    // Focus first: it must not be in a hidden bar.
    const api = props.api();
    if (api !== undefined && cur >= 0 && cur < m.count) {
      api.focusAt(m.line[cur]!, m.from[cur]!);
    } else button.focus();
    say("");
    setOpen(false);
    props.onFind(undefined);
  };
  const quiet = () => {
    if (!open()) return;
    clearTimeout(timer);
    say("");
    setOpen(false);
    props.onFind(undefined);
  };
  // eslint-disable-next-line solid/reactivity -- given once
  props.handle?.({
    closeQuietly: quiet,
    open: () => {
      if (open()) {
        input.focus();
        input.select();
      } else show();
    },
    isOpen: open,
    close: () => {
      if (open()) close();
    },
  });
  const toggle = () => (open() ? close() : show());
  // Off: close if open. Focus in what goes (the button, the bar) moves
  // to the first child button still shown. Read before the parts hide.
  let lost = false;
  createComputed(
    on(
      () => props.off === true,
      (off) => {
        const a = document.activeElement;
        lost = off && a !== null && (a === button || barEl.contains(a));
      },
      { defer: true },
    ),
  );
  createEffect(() => {
    if (props.off !== true) return;
    untrack(quiet);
    if (!lost) return;
    lost = false;
    const to = [...root.querySelectorAll("button")].find(
      (b) => b !== button && !b.hidden && !barEl.contains(b),
    );
    to?.focus();
  });

  const onInputKey = (e: KeyboardEvent) => {
    // 229: Safari's end of a composition (isComposing is already false).
    if (e.key !== "Enter" || e.isComposing || e.keyCode === 229) return;
    e.preventDefault();
    move(e.shiftKey ? -1 : 1);
  };
  const onBarKey = (e: KeyboardEvent) => {
    if (e.key !== "Escape" || e.isComposing || e.keyCode === 229) return;
    e.preventDefault();
    e.stopPropagation();
    close();
  };
  const shown = () => {
    const m = matches();
    if (query() === "") return "";
    return m.count === 0 ? "No matches" : `${current() + 1} of ${m.count}`;
  };

  return (
    <div class="code-toolbar" ref={(el) => (root = el)}>
      <button
        ref={(el) => (button = el)}
        type="button"
        class="code-find-button"
        hidden={props.off}
        aria-expanded={open()}
        aria-controls={barId}
        onClick={toggle}
      >
        Find
      </button>
      {props.children}
      <div
        ref={(el) => (barEl = el)}
        id={barId}
        class="code-find-bar"
        role="search"
        aria-label="Find in file"
        hidden={!open() || props.off === true}
        onKeyDown={onBarKey}
      >
        <label>
          Find{" "}
          <input
            ref={(el) => (input = el)}
            type="text"
            autocomplete="off"
            autocapitalize="off"
            spellcheck={false}
            value={query()}
            onInput={(e) => find(e.currentTarget.value)}
            onKeyDown={onInputKey}
          />
        </label>
        <button
          type="button"
          disabled={matches().count === 0}
          onClick={() => move(-1)}
        >
          Previous
        </button>
        <button
          type="button"
          disabled={matches().count === 0}
          onClick={() => move(1)}
        >
          Next
        </button>
        <span class="code-find-count" aria-hidden="true">
          {shown()}
        </span>
        <button type="button" onClick={close}>
          Close
        </button>
      </div>
      <span class="code-hint" hidden={props.off}>
        {`Browser find sees only visible lines — use Find${
          props.keyHint === undefined ? "" : ` ${props.keyHint}`
        } to search the whole file`}
      </span>
      <p class="sr-only code-find-status" role="status">
        {said()}
      </p>
    </div>
  );
}
