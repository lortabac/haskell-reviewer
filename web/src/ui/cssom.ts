// The only place that writes inline styles (D-79). The CSP blocks style
// attributes but not CSSOM writes, so this goes through el.style. Only
// custom properties: the CSS files say what they do. Lint bans `.style`
// everywhere else (eslint.config.js).

const varName = /^--[a-z][a-z0-9-]*$/;

export function setVar(el: HTMLElement, name: string, value: string): void {
  if (!varName.test(name)) throw new Error(`not a custom property: ${name}`);
  el.style.setProperty(name, value);
}
