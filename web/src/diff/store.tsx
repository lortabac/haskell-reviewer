// The changed files of a diff session (api/diff-files), fetched once
// and shared by the sidebar, Home and the header. Browse mode: no
// fetch, nothing loading.
import {
  createContext,
  createMemo,
  createResource,
  type JSX,
  type ParentProps,
  Show,
  useContext,
} from "solid-js";
import { getJson } from "../api/client";
import { type DiffFiles, decodeDiffFiles } from "../api/diffFiles";
import { fileMarks, type FileMarks } from "../files/changes";
import { usePageSide, useSession } from "../session";

// With "changed only" on, sidebar groups open by default up to this
// many rows (D-337).
export const openAllUpTo = 500;

// The notes on api/diff-files, in the sidebar and on Home.
export const loadingText = "Loading changes…";
export const failedText =
  "Couldn't load the changes. See the terminal for details.";
export const truncatedText =
  "A side has over 100,000 files: changes past that point aren't shown.";

export interface DiffFilesApi {
  // Once loaded, else undefined.
  files: () => DiffFiles | undefined;
  loading: () => boolean;
  // The server answered with an error (pairing failed, say).
  failed: () => boolean;
  // The page side's markers and ghosts, once loaded.
  marks: () => FileMarks | undefined;
}

const none: DiffFilesApi = {
  files: () => undefined,
  loading: () => false,
  failed: () => false,
  marks: () => undefined,
};

// Browse mode unless a provider says otherwise, so tests that mount a
// part of the app need none.
const DiffFilesContext = createContext<DiffFilesApi>(none);

export function DiffFilesProvider(props: ParentProps): JSX.Element {
  const session = useSession();
  const side = usePageSide();
  // The server's answer is the same for the whole process (until
  // reloads, T16), so one fetch.
  const [answer] = createResource(
    () => session.mode === "diff",
    () => getJson("/api/diff-files", decodeDiffFiles),
  );
  const files = (): DiffFiles | undefined => {
    const r = answer();
    return r?.kind === "ok" ? r.value : undefined;
  };
  const marks = createMemo(() => {
    const f = files();
    return f === undefined ? undefined : fileMarks(f, side());
  });
  const value: DiffFilesApi = {
    files,
    loading: () => session.mode === "diff" && answer() === undefined,
    failed: () => {
      const r = answer();
      return r !== undefined && r.kind !== "ok";
    },
    marks,
  };
  return (
    <DiffFilesContext.Provider value={value}>
      {props.children}
    </DiffFilesContext.Provider>
  );
}

export function useDiffFiles(): DiffFilesApi {
  return useContext(DiffFilesContext);
}

// The state of api/diff-files above a sidebar tree. loading: say so
// while it loads (Files needs it, Modules doesn't).
export function DiffFilesNotes(props: { loading: boolean }): JSX.Element {
  const diff = useDiffFiles();
  return (
    <>
      <Show when={props.loading && diff.loading()}>
        <p class="tree-note">{loadingText}</p>
      </Show>
      <Show when={diff.failed()}>
        <p class="tree-note">{failedText}</p>
      </Show>
    </>
  );
}

// Why the list isn't there: call it only then.
export function filesWhy(d: DiffFilesApi): string {
  return d.failed() ? failedText : loadingText;
}
