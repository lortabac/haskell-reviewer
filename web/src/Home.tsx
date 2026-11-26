import { type JSX, onMount, Show } from "solid-js";
import { useGone } from "./events/ReloadApply";
import { DiffSummary } from "./home/DiffSummary";
import { PageTitle } from "./PageTitle";
import { useSession } from "./session";
import { focusLost } from "./ui/focus";

// Diff mode: also a summary of the changes (D-338). After a reload
// took the page's module or file away: a notice (D-354).
export function Home(): JSX.Element {
  const session = useSession();
  const gone = useGone();
  let h1: HTMLHeadingElement | undefined;
  // The old page's focus went with it.
  onMount(() => {
    if (gone !== undefined && focusLost()) h1?.focus();
  });
  return (
    <>
      <PageTitle text="Home" />
      <h1 tabindex="-1" ref={(el) => (h1 = el)}>
        Home
      </h1>
      <Show when={gone}>
        {(g) => (
          <div class="docs-notice reload-gone">
            <p>
              <code>{g()}</code> is not in the files any more.
            </p>
          </div>
        )}
      </Show>
      <p>Pick a module or file in the sidebar.</p>
      <Show when={session.mode === "diff" ? session : undefined}>
        {(s) => <DiffSummary session={s()} />}
      </Show>
    </>
  );
}
