import { createEffect, createMemo, createSignal, type JSX, on } from "solid-js";
import { FilesPanel } from "./files/FilesPanel";
import { ModulesPanel } from "./modules/ModulesPanel";
import { useModules } from "./modules/store";

type View = "modules" | "files";

// The sidebar: a Modules / Files toggle and both views. Both stay
// mounted, so open groups and folders survive a switch.
export function Sidebar(): JSX.Element {
  const modules = useModules();
  // The user's pick. Kept while the app runs, lost on reload (server
  // preferences come in T10).
  const [choice, setChoice] = createSignal<View>();
  // No pick yet: Modules while loading or if there are any, else Files.
  // A memo, so the scroll below runs only when the view really changes.
  const view = createMemo<View>(
    () =>
      choice() ??
      (modules.tree.loading || modules.hasModules() ? "modules" : "files"),
  );
  let modulesPanel: HTMLDivElement | undefined;
  let filesPanel: HTMLDivElement | undefined;
  // A hidden view can't scroll: scroll to the current item when shown.
  createEffect(
    on(
      view,
      (v) => {
        const panel = v === "modules" ? modulesPanel : filesPanel;
        panel
          ?.querySelector('[aria-current="page"]')
          ?.scrollIntoView({ block: "nearest" });
      },
      { defer: true },
    ),
  );
  const button = (v: View, label: string) => (
    <button
      type="button"
      class="sidebar-toggle-button"
      aria-pressed={view() === v}
      onClick={() => setChoice(v)}
    >
      {label}
    </button>
  );
  return (
    <>
      <div class="sidebar-toggle" role="group" aria-label="Sidebar view">
        {button("modules", "Modules")}
        {button("files", "Files")}
      </div>
      <div
        ref={(el) => (modulesPanel = el)}
        class="modules-panel"
        hidden={view() !== "modules"}
      >
        <ModulesPanel />
      </div>
      <div
        ref={(el) => (filesPanel = el)}
        class="files-panel"
        hidden={view() !== "files"}
      >
        <FilesPanel />
      </div>
    </>
  );
}
