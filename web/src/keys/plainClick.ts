// A plain left click, no modifier: the app handles it (a new tab or
// window is the browser's). It also stops the router's own handler.
export function plainClick(e: MouseEvent): boolean {
  if (e.button !== 0) return false;
  if (e.ctrlKey || e.metaKey || e.shiftKey || e.altKey) return false;
  e.preventDefault();
  return true;
}
