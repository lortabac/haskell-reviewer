// Applies a theme choice to the page (D-203). "system" removes the
// override, so tokens.css's plain color-scheme (light dark) picks the
// OS one, live; "light" and "dark" set data-theme, which tokens.css
// reads to fix color-scheme (and so light-dark()) one way.
import type { Theme } from "../api/prefs";

export function applyTheme(el: HTMLElement, theme: Theme): void {
  if (theme === "system") el.removeAttribute("data-theme");
  else el.setAttribute("data-theme", theme);
}
