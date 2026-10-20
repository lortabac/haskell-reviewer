// Checks web/dist against the CSP and the server's rules (SPEC §9.2,
// §11.7). No dependencies. Exits 1 with a list of problems.
import { readdirSync, readFileSync, statSync } from "node:fs";
import { join } from "node:path";

const dist = new URL("../dist/", import.meta.url).pathname;
const problems = [];

function fail(msg) {
  problems.push(msg);
}

// Only index.html and assets/<name>. The server serves nothing else.
const assets = [];
for (const entry of readdirSync(dist)) {
  const path = join(dist, entry);
  if (entry === "index.html" && statSync(path).isFile()) continue;
  if (entry === "assets" && statSync(path).isDirectory()) {
    for (const name of readdirSync(path)) {
      if (!statSync(join(path, name)).isFile()) {
        fail(`not a file: assets/${name}`);
        continue;
      }
      assets.push(name);
    }
    continue;
  }
  fail(`unexpected entry in dist: ${entry}`);
}

// Hashed names only, so the server can cache them as immutable.
// Vite names them <name>-<8 char hash>.<ext>. Only js and css: to allow
// another extension, also add its glob to extra-source-files in the .cabal
// file and its type to contentTypeFor in HaskellReviewer.Server.Assets.
for (const name of assets) {
  if (!/^[\w.-]+-[\w-]{8}\.(js|css)$/.test(name)) {
    fail(`asset without a hash or with a bad extension: ${name}`);
  }
}

// index.html: nothing inline, nothing remote.
const html = readFileSync(join(dist, "index.html"), "utf8");
for (const tag of html.match(/<script\b[^>]*>/gi) ?? []) {
  if (!/\ssrc=/i.test(tag)) fail(`inline script in index.html: ${tag}`);
}
const htmlBans = [
  [/<style\b/i, "<style>"],
  [/\sstyle\s*=/i, "style attribute"],
  [/[\s/]on\w+\s*=/i, "on... handler"],
  [/<meta\s[^>]*http-equiv/i, "<meta http-equiv>"],
  [/<iframe\b/i, "<iframe>"],
  [/<object\b/i, "<object>"],
  [/<embed\b/i, "<embed>"],
  [/javascript:/i, "script URL"],
  [/https?:\/\//i, "remote URL"],
];
for (const [re, what] of htmlBans) {
  if (re.test(html)) fail(`index.html has a ${what}`);
}
// Only double-quoted refs, so the check below sees them all.
for (const [attr] of html.matchAll(/[\s/](?:src|href)\s*=\s*[^"\s]/gi)) {
  fail(`index.html has a ref that is not double-quoted: ${attr}`);
}
for (const [, ref] of html.matchAll(/\s(?:src|href)\s*=\s*"([^"]*)"/gi)) {
  if (ref === "data:,") continue; // the empty favicon
  if (!ref.startsWith("./assets/")) {
    fail(`index.html refers to something outside ./assets/: ${ref}`);
  } else if (!assets.includes(ref.slice("./assets/".length))) {
    fail(`index.html refers to a missing asset: ${ref}`);
  }
}

// JS: the CSP blocks eval and friends (no 'unsafe-eval'). The words are
// banned outright, plus `.constructor("...")`, a common way to reach
// Function.
const jsBans = [
  [/\b(eval|Function)\b/, "eval or Function"],
  [/\.constructor\s*\(\s*["'`]/, ".constructor(<string>)"],
];
// CSS: no fonts (system fonts only), no imports, nothing remote.
const cssBans = [
  [/@font-face/i, "@font-face"],
  [/@import/i, "@import"],
  [/url\(\s*["']?\s*(?:https?:)?\/\//i, "remote url()"],
];
for (const name of assets) {
  const text = readFileSync(join(dist, "assets", name), "utf8");
  const bans = name.endsWith(".js") ? jsBans : cssBans;
  for (const [re, what] of bans) {
    if (re.test(text)) fail(`assets/${name} has ${what}`);
  }
}

if (problems.length > 0) {
  for (const p of problems) console.error(`check-bundle: ${p}`);
  process.exit(1);
}
console.log(`check-bundle: ok (${assets.length} assets)`);
