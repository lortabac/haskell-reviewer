import { createEffect, createMemo, type JSX, on, Show } from "solid-js";
import { useDiffFiles } from "./diff/store";
import { FilesPanel } from "./files/FilesPanel";
import { ModulesPanel } from "./modules/ModulesPanel";
import { useModules } from "./modules/store";
import { usePrefs } from "./prefs/store";
import { useSession } from "./session";
import { ChangeLegend } from "./ui/ChangeSign";

type View = "modules" | "files";

// The sidebar: a Modules / Files toggle and both views. Both stay
// mounted, so open groups and folders survive a switch. The pick is
// remembered via api/prefs (D-205); "auto" (the default) is D-111's
// rule. Diff mode: a "Changed only" box for both views, and the legend
// of the markers (D-337).
export function Sidebar(): JSX.Element {
  const session = useSession();
  const modules = useModules();
  const diffFiles = useDiffFiles();
  const prefs = usePrefs();
  // A memo, so the scroll below runs only when the view really changes.
  const view = createMemo<View>(() => {
    const choice = prefs.sidebar();
    if (choice !== "auto") return choice;
    return modules.loading() || modules.hasModules() ? "modules" : "files";
  });
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
      onClick={() => prefs.setSidebar(v)}
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
      <Show when={session.mode === "diff"}>
        <label class="changed-only">
          <input
            type="checkbox"
            checked={prefs.changedOnly()}
            onChange={(e) => prefs.setChangedOnly(e.currentTarget.checked)}
          />{" "}
          Changed only
        </label>
        <ChangeLegend />
        <Show when={prefs.changedOnly() && diffFiles.files()?.truncated}>
          <p class="tree-note">
            A side has over 100,000 files: changes past that point aren't shown.
          </p>
        </Show>
      </Show>
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
