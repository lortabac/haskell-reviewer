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

// el is all inside the scroll box (its borders and scrollbar out).
export function inBox(el: Element, box: Element): boolean {
  const b = box.getBoundingClientRect();
  const r = el.getBoundingClientRect();
  const top = b.top + box.clientTop;
  return r.top >= top && r.bottom <= top + box.clientHeight;
}
