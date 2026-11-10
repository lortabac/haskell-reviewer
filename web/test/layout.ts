import { vi } from "vitest";

// happy-dom has no layout: give the source view's scroll box a size,
// so the virtualiser mounts rows (fallbackRowHeight px each, from
// CodeView). Its scroll height is the rows' height (--code-h), and
// scrollTop is clamped to it as in a browser. Undo with
// vi.restoreAllMocks().
export function fakeLayout(width = 800, height = 600): void {
  const isView = (el: Element) => el.classList.contains("code-view");
  vi.spyOn(HTMLElement.prototype, "clientHeight", "get").mockImplementation(
    function (this: HTMLElement) {
      return isView(this) ? height : 0;
    },
  );
  vi.spyOn(HTMLElement.prototype, "clientWidth", "get").mockImplementation(
    function (this: HTMLElement) {
      return isView(this) ? width : 0;
    },
  );
  vi.spyOn(HTMLElement.prototype, "scrollHeight", "get").mockImplementation(
    function (this: HTMLElement) {
      if (!isView(this)) return 0;
      const style = this.firstElementChild?.getAttribute("style") ?? "";
      const m = /--code-h:\s*([\d.]+)px/.exec(style);
      return Math.max(height, m === null ? 0 : Number(m[1]));
    },
  );
  const scrollTop = Object.getOwnPropertyDescriptor(
    Element.prototype,
    "scrollTop",
  );
  vi.spyOn(Element.prototype, "scrollTop", "set").mockImplementation(function (
    this: Element,
    top: number,
  ) {
    const max = isView(this) ? this.scrollHeight - this.clientHeight : top;
    scrollTop?.set?.call(this, Math.max(0, Math.min(top, max)));
  });
}
