// Page scrolling helpers.

// Scroll the page (up or down only) so el is on screen: a view can be
// partly off the page.
export function bringOnPage(el: Element): void {
  const r = el.getBoundingClientRect();
  if (r.bottom > window.innerHeight) {
    window.scrollBy(0, r.bottom - window.innerHeight);
  } else if (r.top < 0) {
    window.scrollBy(0, r.top);
  }
}
