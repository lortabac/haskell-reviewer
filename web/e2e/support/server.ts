// Runs the real binary. `make e2e` sets HASKELL_REVIEWER_EXE.
import { type ChildProcess, spawn } from "node:child_process";
import { existsSync } from "node:fs";
import { isAbsolute } from "node:path";
import { createInterface } from "node:readline";

export interface Server {
  // http://127.0.0.1:<port>/<token>/
  url: string;
  // Same as url: pages live under it.
  base: string;
  port: number;
  token: string;
  // stderr, one entry per line.
  log: string[];
  // Wait until `times` lines match. Returns the last match.
  waitForLog: (re: RegExp, ms: number, times?: number) => Promise<string>;
  count: (re: RegExp) => number;
  stop: () => Promise<void>;
  // null while running, or if killed by a signal.
  exitCode: () => number | null;
}

export function exePath(): string {
  const exe = process.env.HASKELL_REVIEWER_EXE;
  if (exe === undefined || !isAbsolute(exe) || !existsSync(exe)) {
    throw new Error(
      "run `make e2e`, or set HASKELL_REVIEWER_EXE to `cabal list-bin exe:haskell-reviewer`",
    );
  }
  return exe;
}

// Every child of this worker, killed if the worker exits early.
const children = new Set<ChildProcess>();
process.on("exit", () => {
  for (const c of children) c.kill("SIGKILL");
});

const urlLine =
  /haskell-reviewer: serving at (http:\/\/127\.0\.0\.1:(\d+)\/([^/\s]+)\/)/;

export async function startServer(opts: { root: string }): Promise<Server> {
  const args = ["--no-open", "--verbose", "--idle-exit", "1", opts.root];
  const child = spawn(exePath(), args, {
    stdio: ["ignore", "ignore", "pipe"],
  });
  children.add(child);

  const log: string[] = [];
  const waiters = new Set<() => void>();
  let exited = false;
  let code: number | null = null;
  const exit = new Promise<void>((resolve) => {
    child.on("exit", (c) => {
      exited = true;
      code = c;
      children.delete(child);
      resolve();
      for (const w of waiters) w();
    });
  });
  createInterface({ input: child.stderr }).on("line", (line) => {
    log.push(line);
    for (const w of waiters) w();
  });

  const count = (re: RegExp): number => log.filter((l) => re.test(l)).length;

  const waitForLog = (re: RegExp, ms: number, times = 1): Promise<string> =>
    new Promise((resolve, reject) => {
      const check = (): void => {
        const hits = log.filter((l) => re.test(l));
        if (hits.length >= times) {
          done();
          resolve(hits[hits.length - 1]!);
        } else if (exited) {
          done();
          reject(new Error(`exited before ${re}; stderr:\n${log.join("\n")}`));
        }
      };
      const timer = setTimeout(() => {
        done();
        reject(
          new Error(
            `no ${re} (x${times}) in ${ms} ms; stderr:\n${log.join("\n")}`,
          ),
        );
      }, ms);
      const done = (): void => {
        clearTimeout(timer);
        waiters.delete(check);
      };
      waiters.add(check);
      check();
    });

  const stop = async (): Promise<void> => {
    if (exited) return;
    child.kill("SIGTERM");
    const timeout = new Promise<boolean>((r) =>
      setTimeout(() => r(false), 5000),
    );
    const ok = await Promise.race([exit.then(() => true), timeout]);
    if (!ok) {
      child.kill("SIGKILL");
      await exit;
    }
  };

  let line: string;
  try {
    line = await waitForLog(urlLine, 10_000);
  } catch (e) {
    await stop();
    throw e;
  }
  const m = urlLine.exec(line)!;
  const url = m[1]!;
  return {
    url,
    base: url,
    port: Number(m[2]),
    token: m[3]!,
    log,
    waitForLog,
    count,
    stop,
    exitCode: () => code,
  };
}
