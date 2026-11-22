import { type JSX, Show } from "solid-js";
import { DiffSummary } from "./home/DiffSummary";
import { PageTitle } from "./PageTitle";
import { useSession } from "./session";

// Diff mode: also a summary of the changes (D-338).
export function Home(): JSX.Element {
  const session = useSession();
  return (
    <>
      <PageTitle text="Home" />
      <h1 tabindex="-1">Home</h1>
      <p>Pick a module or file in the sidebar.</p>
      <Show when={session.mode === "diff" ? session : undefined}>
        {(s) => <DiffSummary session={s()} />}
      </Show>
    </>
  );
}
