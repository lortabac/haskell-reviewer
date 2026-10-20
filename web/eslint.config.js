import js from "@eslint/js";
import solid from "eslint-plugin-solid/configs/typescript";
import tseslint from "typescript-eslint";

// Raw HTML is banned: untrusted text must go in as text (SPEC §9.2, §11.7).
// Inline styles are banned too: the CSP blocks them.
const htmlNames = "/^(innerHTML|outerHTML|srcdoc)$/";
const htmlCalls =
  "/^(insertAdjacentHTML|createContextualFragment|setHTMLUnsafe|parseHTMLUnsafe|parseFromString|execCommand)$/";
const writeNames = "/^(write|writeln)$/";

const rawHtml = "No raw HTML: build DOM nodes or use text.";

const banned = [
  // el.innerHTML, el["innerHTML"]
  {
    selector: `MemberExpression[property.name=${htmlNames}]`,
    message: rawHtml,
  },
  {
    selector: `MemberExpression[computed=true][property.value=${htmlNames}]`,
    message: rawHtml,
  },
  // { innerHTML: x }: catches Object.assign and JSX spread.
  {
    selector: `Property[key.name=${htmlNames}]`,
    message: rawHtml,
  },
  {
    selector: `Property[key.value=${htmlNames}]`,
    message: rawHtml,
  },
  {
    selector: `CallExpression[callee.property.name=${htmlCalls}]`,
    message: rawHtml,
  },
  {
    selector: `CallExpression[callee.computed=true][callee.property.value=${htmlCalls}]`,
    message: rawHtml,
  },
  {
    selector:
      "CallExpression[callee.property.name='setAttribute'][arguments.0.value=/^(style|srcdoc|on.*)$/i]",
    message: "No inline styles, srcdoc or on... handlers.",
  },
  // document.write, window.document.write, document["write"]
  {
    selector: `CallExpression:matches([callee.property.name=${writeNames}], [callee.property.value=${writeNames}]):matches([callee.object.name='document'], [callee.object.property.name='document'])`,
    message: "No document.write.",
  },
  {
    selector:
      "NewExpression:matches([callee.name='DOMParser'], [callee.property.name='DOMParser'])",
    message: rawHtml,
  },
  {
    selector: `JSXAttribute[name.name=${htmlNames}]`,
    message: "No raw HTML: use text.",
  },
  {
    selector: "JSXAttribute[name.name='style']",
    message: "No inline styles: the CSP blocks them. Use a class.",
  },
  {
    selector:
      "JSXNamespacedName[name.name=/^(innerHTML|outerHTML|srcdoc|style)$/]",
    message: "No raw HTML or inline styles.",
  },
];

export default tseslint.config(
  { ignores: ["dist/"] },
  js.configs.recommended,
  tseslint.configs.recommendedTypeChecked,
  {
    languageOptions: {
      parserOptions: {
        projectService: {
          // test/lint.test.ts lints snippets under this name. It is not a
          // real file.
          allowDefaultProject: ["lint-snippet.tsx"],
          defaultProject: "tsconfig.json",
        },
        tsconfigRootDir: import.meta.dirname,
      },
    },
  },
  {
    files: ["**/*.{ts,tsx}"],
    ...solid,
    rules: {
      ...solid.rules,
      "solid/no-innerhtml": ["error", { allowStatic: false }],
    },
  },
  {
    rules: {
      "no-eval": "error",
      "no-implied-eval": "error",
      "no-new-func": "error",
      "no-script-url": "error",
      "no-restricted-syntax": ["error", ...banned],
    },
  },
  {
    files: ["**/*.{js,mjs}"],
    ...tseslint.configs.disableTypeChecked,
  },
  {
    files: ["scripts/**/*.mjs"],
    languageOptions: {
      globals: { process: "readonly", console: "readonly", URL: "readonly" },
    },
  },
);
