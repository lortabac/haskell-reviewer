// A search result the user picked can be the page already showing
// (e.g. re-picking the declaration already at the top): the router
// ignores a link to its own URL, same as any other link (DocsView.tsx
// has the same case for an in-page click, `inPage`). SearchBox has no
// way to know which view, if any, can resolve a declaration anchor, so
// it only asks via a plain window event; a view that can, answers by
// re-running its own "go to this anchor" (scroll into view, focus).
const eventName = "hr:reselect-anchor";

// hash: including the "#".
export function dispatchReselectAnchor(hash: string): void {
  window.dispatchEvent(new CustomEvent<string>(eventName, { detail: hash }));
}

// Returns the unsubscribe function, for onCleanup.
export function onReselectAnchor(handler: (hash: string) => void): () => void {
  const listener = (e: Event): void => {
    handler((e as CustomEvent<string>).detail);
  };
  window.addEventListener(eventName, listener);
  return () => window.removeEventListener(eventName, listener);
}
