// @vitest-environment node
import { ESLint } from "eslint";
import { describe, expect, it } from "vitest";

// Each snippet must fail one of our rules (SPEC §11.7). Linted with the
// real config. The file name is listed in eslint.config.js.
const banned: [string, string][] = [
  ["document.body.innerHTML = 'x';", "no-restricted-syntax"],
  ["document.body.outerHTML = 'x';", "no-restricted-syntax"],
  ["export const s = document.body.innerHTML;", "no-restricted-syntax"],
  ["document.body['innerHTML'] = 'x';", "no-restricted-syntax"],
  [
    "document.body.insertAdjacentHTML('beforeend', 'x');",
    "no-restricted-syntax",
  ],
  [
    "document.createRange().createContextualFragment('x');",
    "no-restricted-syntax",
  ],
  ["document.body.setHTMLUnsafe('x');", "no-restricted-syntax"],
  [
    "new DOMParser().parseFromString('x', 'text/html');",
    "no-restricted-syntax",
  ],
  ["document.write('x');", "no-restricted-syntax"],
  ["document.writeln('x');", "no-restricted-syntax"],
  ["eval('1');", "no-eval"],
  ["export const f = new Function('return 1');", "no-new-func"],
  ["setTimeout('alert(1)', 1);", "@typescript-eslint/no-implied-eval"],
  ["location.href = 'javascript:void 0';", "no-script-url"],
  ["export const A = () => <div innerHTML={'x'} />;", "solid/no-innerhtml"],
  ["export const A = () => <div innerHTML='x' />;", "solid/no-innerhtml"],
  [
    "export const A = () => <div prop:innerHTML={'x'} />;",
    "no-restricted-syntax",
  ],
  ["export const A = () => <iframe srcdoc='x' />;", "no-restricted-syntax"],
  [
    "export const A = () => <div style={{ color: 'red' }} />;",
    "no-restricted-syntax",
  ],
  [
    "export const A = () => <div style='color: red' />;",
    "no-restricted-syntax",
  ],
  ["Object.assign(document.body, { innerHTML: 'x' });", "no-restricted-syntax"],
  [
    "Object.assign(document.body, { 'outerHTML': 'x' });",
    "no-restricted-syntax",
  ],
  [
    "export const A = () => <div {...{ innerHTML: 'x' }} />;",
    "no-restricted-syntax",
  ],
  ["document.createElement('iframe').srcdoc = 'x';", "no-restricted-syntax"],
  ["document.body.setAttribute('style', 'x');", "no-restricted-syntax"],
  ["document.body.setAttribute('srcdoc', 'x');", "no-restricted-syntax"],
  ["document.body.setAttribute('onclick', 'x');", "no-restricted-syntax"],
  ["document.body.setAttribute('OnClick', 'x');", "no-restricted-syntax"],
  ["window.document.write('x');", "no-restricted-syntax"],
  ["document['write']('x');", "no-restricted-syntax"],
  ["document['writeln']('x');", "no-restricted-syntax"],
  ["document.execCommand('insertHTML', false, 'x');", "no-restricted-syntax"],
  ["new window.DOMParser();", "no-restricted-syntax"],
  ["importScripts('x.js');", "no-restricted-syntax"],
  ["self.importScripts('x.js');", "no-restricted-syntax"],
];

const clean = "export const A = () => <div class='x'>{'text'}</div>;\n";

async function lint(code: string): Promise<ESLint.LintResult> {
  const eslint = new ESLint();
  const [result] = await eslint.lintText(code, {
    filePath: "lint-snippet.tsx",
  });
  if (result === undefined) throw new Error("no lint result");
  return result;
}

describe("lint rules", () => {
  it.each(banned)("rejects %s", async (code, rule) => {
    const result = await lint(code);
    expect(result.messages.filter((m) => m.fatal)).toEqual([]);
    const rules = result.messages
      .filter((m) => m.severity === 2)
      .map((m) => m.ruleId);
    expect(rules).toContain(rule);
  });

  it("accepts clean code", async () => {
    const result = await lint(clean);
    expect(result.messages).toEqual([]);
  });
});
