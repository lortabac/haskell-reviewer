// GET and POST api/prefs: the UI preferences, defaults filled in
// (D-197). No generation: they don't belong to a snapshot.
import { bool, field, object, oneOf } from "./decode";

export const themes = ["system", "light", "dark"] as const;
export type Theme = (typeof themes)[number];

export const sidebarViews = ["auto", "modules", "files"] as const;
export type SidebarView = (typeof sidebarViews)[number];

export type Prefs = {
  theme: Theme;
  exportedOnly: boolean;
  sidebar: SidebarView;
};

// Before the first GET answers (or if it's slow or fails): same as the
// config file's built-in defaults (D-199).
export const defaultPrefs: Prefs = {
  theme: "system",
  exportedOnly: false,
  sidebar: "auto",
};

export function decodePrefs(value: unknown, path = "$"): Prefs {
  const o = object(value, path);
  return {
    theme: field(o, "theme", path, oneOf(themes)),
    exportedOnly: field(o, "exportedOnly", path, bool),
    sidebar: field(o, "sidebar", path, oneOf(sidebarViews)),
  };
}
