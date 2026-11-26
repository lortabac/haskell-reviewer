// Home in diff mode (D-338, SPEC §4.1): the revisions as text, counts
// by status, notes, the changed files and the ones outside ROOT. Paths
// are user data: text only. Links go to the diff view (D-342).
import { A } from "@solidjs/router";
import { createMemo, createSignal, For, type JSX, Show } from "solid-js";
import type { ChangedFile, DiffFiles, Outside } from "../api/diffFiles";
import type { Session, SessionEnd } from "../api/session";
import {
  failedText,
  loadingText,
  truncatedText,
  useDiffFiles,
} from "../diff/store";
import { changeNoteWords } from "../diff/notes";
import { diffHref } from "../diff/urls";
import { mergeBaseCount, mergeBaseText, shortHash } from "../header/revText";
import { useModules } from "../modules/store";
import { unionModules } from "../modules/union";
import { ChangeSign } from "../ui/ChangeSign";
import { plural } from "../ui/format";
import {
  fileCounts,
  filesLine,
  moduleCounts,
  modulesLine,
  outsideLine,
} from "./summary";

// Rows shown before "Show all".
export const firstRows = 1000;

type DiffSession = Extract<Session, { mode: "diff" }>;

export function DiffSummary(props: { session: DiffSession }): JSX.Element {
  const diff = useDiffFiles();
  return (
    <>
      <Revisions session={props.session} />
      <h2>Changes</h2>
      <Show when={diff.loading() && diff.files() === undefined}>
        <p class="home-note">{loadingText}</p>
      </Show>
      <Show when={diff.failed()}>
        <p class="home-note">{failedText}</p>
      </Show>
      <Show when={diff.files()}>{(f) => <Changes diff={f()} />}</Show>
    </>
  );
}

function Revisions(props: { session: DiffSession }): JSX.Element {
  const s = () => props.session;
  const base = () =>
    s().mergeBases === null
      ? s().base.label
      : mergeBaseText(s().base.label, s().head.label);
  return (
    <p class="home-revisions">
      Diff {s().spec}. Base: {base()}
      {suffix(s().base, mergeBaseCount(s().mergeBases))}. Head: {s().head.label}
      {suffix(s().head)}.
    </p>
  );
}

// " (abc1234)", " (1 of 2, abc1234)", or nothing (the working tree).
function suffix(end: SessionEnd<string | null>, count?: string): string {
  const parts = [
    count,
    end.commit === null ? undefined : shortHash(end.commit),
  ].filter((p) => p !== undefined);
  return parts.length === 0 ? "" : ` (${parts.join(", ")})`;
}

function Changes(props: { diff: DiffFiles }): JSX.Element {
  const counts = createMemo(() => fileCounts(props.diff.files));
  return (
    <>
      <p class="home-counts">{filesLine(counts())}</p>
      <ModuleLine />
      <Show when={props.diff.renameLimitHit}>
        <p class="home-note">
          Too many files: only exact and same-name renames were looked for.
        </p>
      </Show>
      <Show when={props.diff.truncated}>
        <p class="home-note">{truncatedText}</p>
      </Show>
      <Show when={props.diff.files.length > 0}>
        <h2>Changed files</h2>
        <FileList files={props.diff.files} />
      </Show>
      <h2>Outside ROOT</h2>
      <OutsideList outside={props.diff.outside} />
    </>
  );
}

// Home's URL has no side, so the page side is the default one: head in
// diff mode (D-332). So ok() is head's tree and other() is base's.
function ModuleLine(): JSX.Element {
  const modules = useModules();
  const counts = createMemo(() => {
    const head = modules.ok();
    if (head === undefined) return undefined;
    const base = modules.other();
    if (base === undefined && !modules.otherFailed()) return undefined;
    return moduleCounts(unionModules(head, base, "head"));
  });
  const headFailed = () => !modules.loading() && modules.ok() === undefined;
  return (
    <Show
      when={counts()}
      fallback={
        <p class="home-note">
          {headFailed()
            ? "Couldn't load the modules: they aren't counted."
            : "Loading module changes…"}
        </p>
      }
    >
      {(c) => (
        <>
          <p class="home-counts">{modulesLine(c())}</p>
          <Show when={modules.otherFailed()}>
            <p class="home-note">
              Couldn't load base's modules: removed ones aren't counted.
            </p>
          </Show>
        </>
      )}
    </Show>
  );
}

function FileList(props: { files: readonly ChangedFile[] }): JSX.Element {
  const [all, setAll] = createSignal(false);
  const rows = () => (all() ? props.files : props.files.slice(0, firstRows));
  let list!: HTMLUListElement;
  // The button goes away: focus moves to the first new row, not body.
  const showAll = (): void => {
    setAll(true);
    list.children[firstRows]?.querySelector("a")?.focus();
  };
  return (
    <>
      <ul class="home-files" ref={(el) => (list = el)}>
        <For each={rows()}>{(f) => <FileRow file={f} />}</For>
      </ul>
      <Show when={!all() && props.files.length > firstRows}>
        <button type="button" class="home-show-all" onClick={showAll}>
          Show all {props.files.length.toLocaleString("en-US")}
        </button>
      </Show>
    </>
  );
}

// The path links to its diff (D-342). A rename: the new path, then
// "from <old> (n%)", as in the sidebar. Then the file's note, if any.
function FileRow(props: { file: ChangedFile }): JSX.Element {
  const f = () => props.file;
  return (
    <li>
      <ChangeSign change={f().status} />{" "}
      <A href={diffHref(f().path)}>{f().path}</A>
      <Show when={f().oldPath !== null ? f().oldPath : undefined}>
        {(old) => (
          <>
            {" "}
            <span class="home-muted">
              from {old()}
              {f().similarity !== null ? ` (${f().similarity}%)` : ""}
            </span>
          </>
        )}
      </Show>
      <Show when={f().note}>
        {(n) => (
          <>
            {" "}
            <span class="home-muted">({changeNoteWords(n())})</span>
          </>
        )}
      </Show>
    </li>
  );
}

function OutsideList(props: { outside: Outside | null }): JSX.Element {
  return (
    <Show when={props.outside} fallback={<p class="home-note">Not known.</p>}>
      {(o) => (
        <Show
          when={o().total > 0}
          fallback={<p class="home-counts">No files changed outside ROOT.</p>}
        >
          <p class="home-counts">{outsideLine(o())}</p>
          <details class="home-outside">
            <summary>
              {o().paths.length < o().total || o().truncated
                ? "Show the first "
                : "Show the "}
              {plural(o().paths.length, "path", "paths")}
            </summary>
            <ul>
              <For each={o().paths}>{(p) => <li>{p}</li>}</For>
            </ul>
          </details>
        </Show>
      )}
    </Show>
  );
}
