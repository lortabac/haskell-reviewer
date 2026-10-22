import { For, type JSX, Show } from "solid-js";
import { noticeText, warningsTitle } from "./notices";
import { useModules } from "./store";

// Project notices (stack, hpack, implicit) and warnings, at the top of
// the main view. Nothing while loading or if the tree failed.
export function ProjectNotices(): JSX.Element {
  const modules = useModules();
  return (
    <Show when={modules.ok()}>
      {(t) => (
        <>
          <For each={t().notices}>
            {(n) => <p class="project-notice">{noticeText(n)}</p>}
          </For>
          <Show when={t().warnings.length > 0}>
            <details class="project-warnings">
              <summary>{warningsTitle(t().warnings.length)}</summary>
              <ul>
                <For each={t().warnings}>{(w) => <li>{w}</li>}</For>
              </ul>
            </details>
          </Show>
        </>
      )}
    </Show>
  );
}
