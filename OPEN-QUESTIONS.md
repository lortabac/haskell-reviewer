# Open questions

The spec is approved as v1.1. The items below are verification tasks for the
implementation phase. Their outcomes are logged in DECISIONS.md, and SPEC.md
is updated if needed (§10.3).

## To verify during implementation (not blocking the spec)
- **Q-MACOS-SIGINT**: resolved as a known macOS limitation (D-139). On
  macOS (arm64; GHC 9.10, 9.12 and 9.14) a signal that arrives while the
  process is busy can be handled late: its message waits in the timer
  manager's control pipe until the next wakeup (another signal or a
  timer). Shown by a tiny program with only `installHandler`: 10 of 40
  signals delayed while a thread allocated for 50 ms, 0 of 40 when idle;
  0 of 20 late signals lost on an idle server; all zero on Linux. A GHC
  bug, not ours; a draft report exists (not filed yet). Not fixed (human,
  2026-10-26: no extra complexity). Stop tests that signal during
  start-up are skipped on macOS.
- **Q-RTS-BLACKHOLE**: On GHC 9.12 and 9.14 the threaded test
  binary sometimes hung (about 5% of runs of the PackageGlob tests): several
  capabilities spin on a `CAF_BLACKHOLE` whose blocking queue was already
  woken, so GC sync never ends and SIGTERM is ignored. Not our code (pure,
  no `unsafe*`); standalone repros didn't hang. Workaround: shared test
  values are built once with `withResource` (Note [Shared values in tests]).
  Open: root cause, GHC issue, and whether the server can hit it. Until
  then, the server builds shared values fully before request threads use
  them.
  9.10 hung too (idle, not spinning: see T10h below).
  T10h (9.10, "Config load", 3 hangs in 300 runs): not a spin this time.
  All capabilities idle, and a debug RTS (`printAllThreads` from gdb)
  showed 1–6 test threads blocked on one black hole that had already
  been evaluated and moved by the GC; their block info still pointed at
  the old address. So a lost wake-up in the RTS. The threads were all in
  temporary's `createTempDirectory`, in `Text.Printf`'s `parseIntFormat`
  (it formats the dir name with `printf "-%.*x"`): the black hole was
  one of printf's shared modifier strings (e.g. "l", "L"). The server
  reaches printf only through GHC's error codes, which warmUpParser
  already builds (D-126), and through config errors at startup.
  Workaround: the test `main` makes one temp dir first
  (`warmUpTempDirs`). Small repros
  (many threads doing their first printf, or stat on a missing file,
  under GC load) still didn't hang.
  M2/T03: a test that repeated `parseWithin 1_000 … (slowModule 20000)`
  let GHC share that call as one top-level thunk; two test threads
  evaluated it at once, both stopped by 1 ms timeouts, and the 9.12 RTS
  segfaulted (7 in ~357 runs; gdb: a worker inside the test's static
  closure). Duplicate removed (D-235). It broke D-125's rule (inputs
  forced before a timed parse).
  M3/T16a (9.12.4 only): the RunTest "many threads while 20 reloads run"
  test (16 request threads, 20 reloads, workers cancelled each time)
  crashes the RTS now and then: SIGSEGV or SIGABRT. Debug RTS messages:
  `internal error: invalid closure, info=0xaaaaaaaaaaaaaaaa` (also
  `info=(nil)` and other bad pointers), and assertions in
  `rts/sm/Sanity.c` line 421 and `rts/include/rts/storage/Block.h` line
  197. gdb: the GC scavenges an `EAGER_BLACKHOLE` whose owner TSO points
  to freed memory (0xaa fill); the stacks around it are `AP_STACK`s
  frozen by async exceptions in the timer manager (`timeout`,
  `registerDelay`, `threadDelay`). Not our FFI (fd fixes found on the
  way are T23). Crashes:

  | Build | Crashes / runs |
  |---|---|
  | T16a, 9.12.4, debug RTS | 7/56 to 28/128 |
  | master, 9.12.4, debug RTS | 0/56 |
  | T16a, 9.10.3 and 9.14.1, debug RTS | 0/128 each |
  | T16a without that test, 9.12.4, debug RTS | 0/96 |
  | T16a, 9.12.4, release | 4/420 |

  Production: a 9.12-built server under concurrent requests and reloads
  may hit it too (same timer manager, same cancels). Workaround: that
  test is skipped on 9.12 (`skipOnGhc912` in RunTest), so CI stays
  green; 9.10 and 9.14 still run it. GHC 9.12.4 RTS bug, likely
  https://gitlab.haskell.org/ghc/ghc/-/issues/27432 (fixed in 9.12.5 by
  MR !16249: 9.12.4 shipped without the #26495 fix). A standalone repro
  crashes 9.12.4 every run, never 9.10.3, 9.12.2 or 9.14.1 (dev notes:
  `haskell-reviewer-dev/goals/m3-plans/ghc-rts-ticket/ticket.md`).
  Re-enable the test on 9.12 once 9.12.5 is the tested 9.12.
- **Q-OLD-SYNTAX**: closed (D-217): no shim in v1; reopen on user
  reports. How real is "the newest parser rejects old code"? The
  known cases (e.g. changes to `forall`/`!`/`~` lexing, `NoStarIsType`) need
  to be checked against the 3 supported GHCs, which may require a lexing-mode
  shim.
  M0 (D-83): the 9.14 parser rejects 0 of 2,355 files in the reference
  projects, and no old-syntax case in the extras. On a checklist of known
  cases, 9.10, 9.12 and 9.14 accept and reject the same things; `forall`
  as a term name is rejected from 9.10 on (9.6 accepts it; 9.8 not
  tested). Re-run: `spikes/parser-count/old-syntax-probes/run.sh`.
  M1 (T13b): the product's census finds no parse or pragma error in the
  reference projects, and none of the 79 in the extras is old syntax
  (missing flags, CPP, templates, markdown-unlit docs outside any
  component). The probes give the same codes as in M0.
- **Q-CLAUDE-CLI-FLAGS**: Confirm the exact `claude` flags for D-34/D-47
  with a real headless run on the tested CLI version range. Flags were seen in
  `claude --help` 2.1.281: `--tools`, `--mcp-config`, `--strict-mcp-config`,
  `--allowedTools`, `--restricted`, `--setting-sources`, `--system-prompt`,
  `--no-session-persistence`, `--input-format/--output-format stream-json`.
  Also confirm that the tool list appears in the init event, the MCP tool
  naming (`mcp__<server>__<tool>`), and a turn-limit flag for the §6.5
  backstop.
- **Q-PERF-COLD**: Set a bound for the cold HIE-conversion cache from the
  first benchmarks (§10.1).
- **Q-GC**: closed for M1 (D-219) and M2 (D-305): keep the copying GC and
  the D-162 defaults. Re-check when the precise tier or diff mode grows the
  index. Options then: compact regions for finished module models, or the
  non-moving collector (§11.5). M2 reference run: wire-server max major
  pause after indexing 18 ms, idle-gap navigation p95 12.1 ms (module) and
  13.6 ms (source). T18b: re-check on the range runs, both sides
  indexed (D-357).
- **Q-OPENAI-COMPAT**: Check that the `openai` package's raw stream and our
  delta decoder work with Ollama, llama.cpp and vLLM, not only OpenAI
  (§11.6.3).
- **Q-SSE**: Check SharedWorker + SSE keep-alive and reconnect on the
  three browsers, including background-tab throttling (§11.6.2).
  Known: Chrome's Memory Saver discards idle tabs, which closes their
  stream, so the tool may idle-exit while the tab still looks open (D-80).
  M0 (D-89): `make e2e` now checks, on Chromium and Firefox (and WebKit in
  CI): one stream for several tabs, the fallback (no SharedWorker), a
  second browser, keep-alive over 40 s, a closed browser noticed within
  about 30 s, reconnect after a cut, the end of a session (new run, and
  30 s refused), and on Chromium a tab frozen for 40 s (CDP) keeps the
  stream. All pass locally on Chromium and Firefox.
  WebKit: can't run on the dev machine (Fedora; Playwright's WebKit wants
  Ubuntu libs), so it runs only in CI. Not known yet whether its
  SharedWorker has EventSource; if not, the fallback kicks in and the
  "one stream" test fails there.
  Still manual: real background-tab throttling, Memory Saver, real
  Safari, sleep/wake.
- **Q-HIE-SIZE**: Measure the index size and decode time on pandoc and
  wire-server built with `-fwrite-ide-info`, against the guess of 10–20 KB
  per module (D-87). Are types at uses and local types worth keeping?
- **Q-HIE-UNIT**: Which unit ids does cabal give executables, test suites
  and sub-libraries, with cabal 3.14/3.16 and GHC 9.10–9.14? Does the
  `-inplace-<comp>` pattern hold without `plan.json`, and with
  `--hie-dir`? (Matching a `.hie` to a component, D-87.)
- **Q-HIE-STANZA**: A `.hie` file doesn't record extensions, flags or
  cpp-options. How do we check "same build-relevant stanza" (§3)? Option:
  compare the side's stanza with the working tree's, and require the
  `.hie` to be newer than the `.cabal` file.
- **Q-HIE-PATCH**: `readHieFile` rejects other patch levels of the same
  series (`hieVersion` 9122 vs 9124). Check `readHieFileWithVersion` with
  a series-only check. Else, one companion per exact GHC version (a §7.1
  change).
- **Q-DIFF-LINK-DRAG**: In the diff grid a drag that starts on a code link
  selects nothing in Chromium and WebKit (they don't start a selection on
  a link). The source view works around it by hand (CodeView
  `onMouseDown`, `linkDrag`). Move that code to a shared helper
  and use it in the diff grid too? Out of scope for T14b (D-347).
