// The fixture project: a small git repo in a temp dir, built fresh by
// each worker. Fixed author and dates, no user git config.
import { execFileSync } from "node:child_process";
import { mkdir, mkdtemp, rm, symlink, writeFile } from "node:fs/promises";
import { tmpdir } from "node:os";
import { dirname, join } from "node:path";

// Exactly 5 lines.
export const mainHs = [
  "module Main where",
  "",
  "main :: IO ()",
  "main =",
  '  putStrLn "hello"',
  "",
].join("\n");

export const readme = "# Fixture\n\nA test project.\n";
export const deepHs = "module Data.Deep.Nested.Mod where\n\nx :: Int\nx = 1\n";

// Markup that must show up as text, never run.
export const evilHs =
  '<script>window.__pwned=1</script>\n<img src=x onerror="window.__pwned=2">\n';
// A name with markup. Not "<b>bold</b>.txt": "/" can't be in a name.
export const markupName = "<b>bold.txt";

// Odd names, each with one known line.
export const oddFiles: Record<string, string> = {
  "odd/with space.txt": "a space\n",
  "odd/hash#and%percent.txt": "hash and percent\n",
  "odd/q?.txt": "question mark\n",
  "odd/ünï.txt": "not ascii\n",
};

export const bigSize = 1024 * 1024 + 1;

// What the file view says, exactly.
export const notices = {
  binary: "This file is not UTF-8 text, so it isn't shown (4 bytes).",
  tooLarge:
    "This file is too large to show (1,048,577 bytes; the limit is 1 MiB).",
  nested: "Nested git repository. Its files aren't shown.",
};

// The top-level folders, in tree order.
export const topFolders = ["data", "odd", "src", "vendor", "xss"];

const gitEnv: NodeJS.ProcessEnv = {
  ...process.env,
  GIT_CONFIG_GLOBAL: "/dev/null",
  GIT_CONFIG_NOSYSTEM: "1",
  GIT_AUTHOR_NAME: "Fixture",
  GIT_AUTHOR_EMAIL: "fixture@example.com",
  GIT_AUTHOR_DATE: "2000-01-01T00:00:00Z",
  GIT_COMMITTER_NAME: "Fixture",
  GIT_COMMITTER_EMAIL: "fixture@example.com",
  GIT_COMMITTER_DATE: "2000-01-01T00:00:00Z",
};

function git(cwd: string, ...args: string[]): void {
  execFileSync("git", ["-c", "init.defaultBranch=main", ...args], {
    cwd,
    env: gitEnv,
    stdio: ["ignore", "ignore", "pipe"],
  });
}

async function put(root: string, path: string, data: string | Buffer) {
  const full = join(root, path);
  await mkdir(dirname(full), { recursive: true });
  await writeFile(full, data);
}

export interface Project {
  root: string;
  remove: () => Promise<void>;
}

export async function makeProject(): Promise<Project> {
  const root = await mkdtemp(join(tmpdir(), "hr-e2e-"));
  git(root, "init", "-q");
  await put(root, "README.md", readme);
  await put(root, "src/Main.hs", mainHs);
  await put(root, "src/Data/Deep/Nested/Mod.hs", deepHs);
  await put(root, "xss/Evil.hs", evilHs);
  await put(root, `xss/${markupName}`, "markup in the name\n");
  for (const [path, text] of Object.entries(oddFiles)) {
    await put(root, path, text);
  }
  await put(root, "data/blob.bin", Buffer.from([0x00, 0x01, 0x02, 0xff]));
  await put(root, "data/big.txt", "a".repeat(bigSize));
  await symlink("src/Main.hs", join(root, "link-to-main"));
  await symlink("nowhere", join(root, "dangling"));
  await put(root, ".gitignore", "*.log\n");
  await put(root, "secret.log", "secret\n");
  git(root, "add", "-A");
  git(root, "commit", "-q", "-m", "fixture");
  await put(root, "untracked.txt", "not in git yet\n");
  await mkdir(join(root, "vendor/nested"), { recursive: true });
  git(join(root, "vendor/nested"), "init", "-q");
  return {
    root,
    remove: () => rm(root, { recursive: true, force: true }),
  };
}
