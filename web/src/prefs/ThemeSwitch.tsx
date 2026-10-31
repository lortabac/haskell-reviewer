// The theme control in the header: System / Light / Dark, remembered
// (D-203). Like Sidebar's view toggle: a labelled group of
// aria-pressed buttons, not colour alone.
import { For, type JSX } from "solid-js";
import { themes, type Theme } from "../api/prefs";
import { usePrefs } from "./store";

const labels: Record<Theme, string> = {
  system: "System",
  light: "Light",
  dark: "Dark",
};

export function ThemeSwitch(): JSX.Element {
  const prefs = usePrefs();
  return (
    <div class="theme-switch" role="group" aria-label="Theme">
      <For each={themes}>
        {(t) => (
          <button
            type="button"
            class="theme-switch-button"
            aria-pressed={prefs.theme() === t}
            onClick={() => prefs.setTheme(t)}
          >
            {labels[t]}
          </button>
        )}
      </For>
    </div>
  );
}
