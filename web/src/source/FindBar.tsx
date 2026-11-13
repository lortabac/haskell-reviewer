// The find toolbar above the source view (T09, G11): the Find button,
// the hint and, when open, the bar. Enter in the input moves to the
// next hit and stays in the input; Esc or Close puts focus on the
// current hit. The count goes to a live region.
import {
  createMemo,
  createSignal,
  createUniqueId,
  type JSX,
  onCleanup,
} from "solid-js";
import type { CodeApi, FindHits } from "./CodeView";
import {
  createFinder,
  type Matches,
  nearest,
  noMatches,
  status,
  step,
} from "./find";
import type { LineIndex } from "./lines";

// Typing: say the count this long after the last key.
export const typingDelay = 500;

export interface FindBarProps {
  text: string;
  index: LineIndex;
  api: () => CodeApi | undefined;
  // The hits to draw; undefined when the bar is closed.
  onFind: (hits: FindHits | undefined) => void;
  // More for the toolbar, after the Find button (the references bar).
  children?: JSX.Element;
  // Called when the bar opens, before it draws: others close then.
  onOpen?: () => void;
  // Gets a way to close the bar with no focus change.
  handle?: (h: { closeQuietly: () => void }) => void;
}

export function FindBar(props: FindBarProps): JSX.Element {
  const barId = createUniqueId();
  let button!: HTMLButtonElement;
  let input!: HTMLInputElement;
  // The file never changes here: a new text is a new FindBar.
  // eslint-disable-next-line solid/reactivity
  const finder = createFinder(props.text, props.index);
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
    say(query() === "" ? "" : status(m, cur));
  };
  onCleanup(() => {
    clearTimeout(timer);
  });

  const find = (q: string) => {
    setQuery(q);
    const m = finder.search(q);
    const cur = nearest(m, origin.line, origin.col);
    publish(m, cur);
    clearTimeout(timer);
    timer = setTimeout(() => sayNow(m, cur), typingDelay);
  };
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
    origin =
      c === undefined ? { line: 1, col: 0 } : { line: c.line, col: c.col };
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
    if (cur >= 0 && cur < m.count) {
      props.api()?.focusAt(m.line[cur]!, m.from[cur]!);
    } else button.focus();
    say("");
    setOpen(false);
    props.onFind(undefined);
  };
  // eslint-disable-next-line solid/reactivity -- given once
  props.handle?.({
    closeQuietly: () => {
      if (!open()) return;
      clearTimeout(timer);
      say("");
      setOpen(false);
      props.onFind(undefined);
    },
  });
  const toggle = () => (open() ? close() : show());

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
    <div class="code-toolbar">
      <button
        ref={(el) => (button = el)}
        type="button"
        class="code-find-button"
        aria-expanded={open()}
        aria-controls={barId}
        onClick={toggle}
      >
        Find
      </button>
      {props.children}
      <div
        id={barId}
        class="code-find-bar"
        role="search"
        aria-label="Find in file"
        hidden={!open()}
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
      <span class="code-hint">
        Browser find sees only visible lines — use Find to search the whole file
      </span>
      <p class="sr-only code-find-status" role="status">
        {said()}
      </p>
    </div>
  );
}
