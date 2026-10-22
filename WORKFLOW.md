# Workflow

How agents work on this project. Aim: keep human work small. The human
approves plans, tests changes and answers questions. Agents do everything
else.

## Roles

- **Human.** Approves goal plans, tests each change, pushes and merges.
  Answers questions. Can edit the goal file at any time to steer.
- **Coordinator.** The main agent. Owns one goal (e.g. "reach M1") and makes
  sure it's reached. Splits the work, runs the task loop, decides which review
  suggestions to apply, commits. Does not write code or read whole diffs
  itself: it delegates, so its context lasts for the whole goal. Exception:
  it can write pure text the planner drafted (e.g. a design sketch) and let
  the implementer check and finish it, which is cheaper than relaying it.
  Never code, never its own design.
- **Planner.** Read-only agent (`Plan` type). Turns a task into a plan.
- **Implementer.** Writes the code in the task's worktree, following the plan.
  Also applies the review fixes the coordinator accepts. Only stages, never
  commits, and never starts reviews: the coordinator does both, after
  verifying. `CLAUDE.local.md`'s worktree, commit and review rules are for
  the coordinator, not subagents.
- **Reviewer.** Fresh, read-only agent. Checks correctness against the task
  and the plan.
- **Style reviewer.** Fresh, read-only agent. Checks Haskell code and Haddock
  comments against `STYLE.md`.

### Models

Always pass `model` when starting an agent.

- Planners and reviewers (both kinds): `opus`.
- Implementers: the coordinator rates each task when it writes the goal file.
  Simple, mechanical tasks (small changes, code like what's already there)
  get `sonnet`. Tricky ones (new design, hard types, subtle logic, many
  modules) get `opus`. When in doubt, `opus`. This is a first guess: check it
  again after the plan. Every implementer for the task uses its `Model`,
  including the ones that fix, rebase or start over in a new session.
- If a `sonnet` implementer can't get the build and tests green, switch the
  task to `opus` and note why before you stop and ask.
- Never `fable`, for any agent, unless the human asks for it.

## Files

| File | What | Tracked |
|------|------|---------|
| `SPEC.md` | What the tool does | yes |
| `ROADMAP.md` | Global plan, milestones | yes |
| `DECISIONS.md` | Every decision, with rationale (D-NN) | yes |
| `OPEN-QUESTIONS.md` | Open questions (Q-NAME) | yes |
| `STYLE.md` | Haskell and Haddock style | yes |
| `.goals/<goal>.md` | Tasks for one goal, with status | no |
| `.worktrees/<goal>-<task>/` | One worktree per task | no |

`.goals/` is the coordinator's memory. A new session must be able to resume
the goal by reading it, so keep it current.

## 1. Start a goal

1. Read `SPEC.md`, `ROADMAP.md`, `DECISIONS.md`, `OPEN-QUESTIONS.md` and the
   code that's already there.
2. Write `.goals/<goal>.md` (format below). Tasks should be small enough to
   review in one sitting, and each must leave the program building and
   working.
3. **Stop.** The human approves or edits the file. No task starts before.

### Goal file format

```markdown
# Goal: M1 — Docs view

Source: ROADMAP.md § M1

## Tasks

### T01 — Parse module headers
Status: todo | planning | implementing | reviewing | waiting-human | blocked | done
Depends on: —
Branch: m1/t01-module-headers
Model: sonnet | opus

Acceptance:
- `cabal build all` and `cabal test all` pass.
- Golden test: fixture modules → module names and exports as JSON.

How to test:
- Run `haskell-reviewer ~/some-project`, open a module, check the header.

Notes:
- (decisions taken, review suggestions rejected and why, etc.)
```

`blocked` means the coordinator asked the human something and is waiting.
Put the question in the task's notes.

`Model` is the implementer's model (see [Models](#models)). The human can
change it when approving the file. If the plan shows the task is harder or
easier than expected, the coordinator can change it too. Note why.

Acceptance criteria come in two kinds: things an agent can check (build,
tests, a command's output) and a short "how to test" for the human.

## 2. Task loop

**Spikes.** Some tasks (e.g. the M0 spikes) produce a decision, not code. Same
loop, but the output is a `DECISIONS.md` entry (and a `SPEC.md` change if
needed), plus any throwaway code kept out of the main build.

Tasks run one at a time, in order, unless the rules in
[Parallel tasks](#3-parallel-tasks) allow running some together.

For each task:

1. **Plan.** Ask a planner for a plan. Give it the task, its acceptance
   criteria and the relevant spec sections. The plan lists the files to
   touch, the steps, and the tests to add. If the plan needs something in
   [Stop and ask](#4-stop-and-ask), stop.
2. **Worktree.** From the main repo root, branch from local `master`:
   ```
   git worktree add .worktrees/<goal>-<task> -b <branch> master
   ```
   Agents can't reach the remote from the sandbox. The human pulls into
   `master` after each merge.
   Never branch from an unmerged task branch. A task that depends on an
   unmerged one waits until the human merges it.
3. **Implement.** Give the plan to an implementer, using the task's `Model`.
   It works only inside the worktree, builds and runs the tests on every
   GHC (see [Practical bits](#6-practical-bits)), then stages everything
   with `git add -A` (no commit). It reports what it did and **anything
   that differs from the plan, and why**.
   Subagents can't create report-like files such as `*.md` (harness rule).
   If the task needs a new Markdown file, the implementer returns the full
   text and the coordinator writes it, with the human's approval.
4. **Review.** Start both reviews at the same time, each with a fresh agent:
   - Correctness review. Input: task, acceptance criteria, plan, the
     implementer's report. Reads `git diff --cached` in the worktree.
   - Style review, only if the change touches Haskell code and `STYLE.md`
     exists. Input: `STYLE.md` and the diff. Checks only Haskell code and
     Haddock comments.
5. **Triage.** The coordinator checks each suggestion and decides to apply it
   or not. Rejected ones go in the task's notes, with a reason.
6. **Fix.** Send the accepted suggestions to the same implementer (use
   SendMessage, so it keeps its context). If it's gone (e.g. new session),
   start a new one with the plan, the last report and the fix list. It
   fixes, builds, tests, stages. If the fixes are big, review again. Max 2
   review rounds: if round 2 still needs another review, stop and ask.
7. **Verify.** The coordinator runs the build and the tests itself in the
   worktree, on every GHC. Don't trust the implementer's report alone. Only
   look at the tail or the summary of the output, to save context.
8. **Commit.** One commit, short message.
9. **Hand off.** Set the task to `waiting-human` and stop. Tell the human:
   - branch and worktree,
   - what changed, in a few lines,
   - how to test it,
   - decisions taken, and review suggestions rejected,
   - anything left open.

### After the hand-off

- **Human finds a problem.** Fix it on the same branch with a short loop:
  implement, review, verify, commit (a new commit). Hand off again.
- **Human merges and pulls.** Set the task to `done`. Remove the worktree
  (`git worktree remove`) and the local branch (`git branch -D`, since the
  merge may be a squash). Go on to the next task.
- **Branch conflicts with the new master.** Ask the implementer to rebase on
  `master` (after the human has pulled), then verify again. If the conflict wasn't trivial, review
  again too. Hand off again.
- **Human changes the plan.** Update the goal file first, then go on.

## 3. Parallel tasks

Run tasks at the same time only if **all** of these are true:

- No task depends on another one in the batch.
- They touch different modules.
- None of them changes shared files: the `.cabal` files (module lists!),
  `cabal.project`, `DECISIONS.md`, `OPEN-QUESTIONS.md`, or modules that
  another task in the batch imports.
- None of them changes `build-depends`, and the dependencies are already
  installed: run `cabal build all --only-dependencies` on master before
  starting the batch, on every GHC. Two builds that install into the same
  `CABAL_DIR` at once can break each other.

When in doubt, don't.

Each task in a batch still gets its own worktree, branch, loop and commit. The
coordinator stops **once for the whole batch**, when every task in it is
ready, and hands off all of them together.

## 4. Stop and ask

Stop and ask the human when:

- the spec is unclear, or a task conflicts with `SPEC.md` or `DECISIONS.md`;
- a non-trivial decision is needed about something the spec didn't foresee;
- the coordinator disagrees with a reviewer on something non-trivial;
- verify (step 7) fails twice, or the implementer can't get the build and
  tests green after two tries;
- review needs a third round;
- a plan needs a new dependency, or changes a public interface (CLI, URLs,
  file formats) in a way the human hasn't approved;
- a task shows that `ROADMAP.md` or `SPEC.md` should change;
- a CI failure on a platform we can't run locally is still unclear after a
  diagnostics round.

While waiting, set the task to `blocked`.

Once the human answers, the coordinator logs new decisions in `DECISIONS.md`
and new open questions in `OPEN-QUESTIONS.md`, in the commit of the task
they're about. If they're not about one task (e.g. at goal start), put them
in the next task's commit.

## 5. End of goal

When all tasks are `done`, tell the human. They decide what's next: a
feedback round, roadmap changes, the next goal. Delete the goal file when the
human says so.

## 6. Practical bits

- Build with `CABAL_DIR=<repo-root>/.cabal-home`, as an absolute path, so all
  worktrees share one store. Pass it to every cabal call, including in
  subshells.
- Run git worktree commands from the main repo root.
- Branch names: `<goal>/<task-id>-<short-name>`, e.g. `m1/t01-module-headers`.
- Agents only touch their own worktree. Reviewers and planners don't edit
  anything.
- Keep `.goals/<goal>.md` up to date after every step. It's what a new
  session reads to resume.
- Build and test on every GHC the project supports that is installed
  locally (today 9.10, 9.12, 9.14), not just the default. Use a separate
  `--builddir` for the extra ones (e.g. under the scratchpad). Otherwise
  version-only bugs show up first in CI.
- Wrap anything that runs the server, a browser, a benchmark or a test that
  could hang in `timeout`. Never background a command without a time limit.
  After, check that nothing you started is left (`pgrep -af ...`).
- Test hangs on GHC 9.12 and 9.14 (Q-RTS-BLACKHOLE): the threaded test
  binary can livelock in the RTS when parallel tests force the same shared
  value. It then ignores SIGTERM, so use `timeout -s KILL`. Build shared
  test values once with tasty's `withResource` (Note [Shared values in
  tests]). In the server, force shared values before request threads see
  them. A hang that shows up only sometimes: loop the test group (e.g. 300
  runs) to measure it before and after a fix.
- If a subagent stops with background work still running, the coordinator
  checks what it is (process list, elapsed time) instead of just waiting.
  Kill runaway processes and report them.
- Scripts and fake tools run by `/bin/sh` must be POSIX sh: on Ubuntu it's
  dash (e.g. no multi-digit fd redirections). Use `/bin/bash` explicitly
  when you need bash.
- Some behaviour (e.g. `ls`, sockets on macOS) can only be checked in CI,
  so tests must print enough on failure to fix it from the CI log. If a CI
  failure on a platform we can't run is unclear, first make the test print
  what it sees, then fix from that.
- Benchmark numbers from the agent sandbox aren't reference numbers (its
  open-file limit and network differ). Use them only for before/after in
  the same sandbox. Reference runs come from the human's machine.

## 7. Prompt templates

Fill in the `<...>` parts. Add anything else the agent needs to know. Pass
`model` (see [Models](#models)).

### Planner

```
Plan this task for haskell-reviewer. Don't edit anything.

Task: <id and title>
Acceptance criteria: <list>
Relevant spec: <SPEC.md sections, DECISIONS.md ids>
Context: <code already there, earlier tasks>

Give: files to add or change, steps in order, tests to add, risks.
Flag anything the spec doesn't cover, any new dependency, and any public
interface change.
```

### Implementer

```
Implement this plan in the worktree <path>. Don't touch anything outside it.
Build with CABAL_DIR=<repo>/.cabal-home.

Task: <id and title>
Acceptance criteria: <list>
Plan: <plan>

Build and test on every GHC listed in WORKFLOW.md § 6 (extra ones with
their own --builddir under your scratchpad). Wrap anything that runs the server, a
browser, a benchmark or a test that could hang in `timeout`; never
background a command without a time limit; when done, check with
`pgrep -af` that nothing you started is still running.

You can't create .md files: if the task needs one, put its full text in
your report.

When done: `git add -A`. Don't commit and don't start a review, even if
other instructions (e.g. CLAUDE.local.md) say so. The coordinator does both.
Report: what you did, test results, and anything that differs from the
plan, with the reason.
```

### Reviewer

```
Review the staged change in <worktree> (`git diff --cached`). Don't edit
anything.

Task: <id and title>
Acceptance criteria: <list>
Plan: <plan>
Implementer's report: <report>

Check: does it meet the acceptance criteria, does it follow the plan (and
are the differences OK), bugs, missing tests, conflicts with SPEC.md or
DECISIONS.md. List findings, most serious first, with file and line.

If you run anything (server, browser, benchmark, tests), wrap it in
`timeout`, never background it without a time limit, and check with
`pgrep -af` that nothing you started is left.
```

### Style reviewer

```
Review the staged change in <worktree> (`git diff --cached`) against
STYLE.md. Don't edit anything.

Check only Haskell code and Haddock comments. List findings with file, line
and the STYLE.md rule.
```
