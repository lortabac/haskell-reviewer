CABAL ?= cabal
PREFIX ?= $(HOME)/.local
BINDIR ?= $(PREFIX)/bin
# Where the user config is (SPEC §7). A relative XDG_CONFIG_HOME doesn't
# count, as in the tool.
CONFDIR ?= $(if $(filter /%,$(XDG_CONFIG_HOME)),$(XDG_CONFIG_HOME),$(HOME)/.config)/haskell-reviewer
BENCH_RUNS ?= 10
BENCH_PROJECTS ?=
BENCH_JSON ?=
# Text report only, without the build log and progress lines.
BENCH_REPORT ?=
BENCH_LABEL ?=
# Extra +RTS options for the product, e.g. BENCH_RTS="--nonmoving-gc".
BENCH_RTS ?=
# Any other bench flags, e.g. BENCH_ARGS="--steady-runs 1 --nav-idle 10".
BENCH_ARGS ?=

.PHONY: build test install freeze web e2e bench bench-smoke cpp-check resolve-check linediff-check cbor-json

build:
	$(CABAL) build all

test:
	$(CABAL) test all --test-show-details=direct

# Runs the three reference projects (SPEC §10, §11.9; D-76, D-82, D-215,
# D-303, D-308): the §10.1 targets (module and source pages, search, find
# references, text search), memory, GC pauses and the fallback census. 10-20
# minutes. By hand, before a release: shared CI runners are too noisy for
# timings. Needs network the first time, to fetch the projects into
# bench/projects/ (git-ignored). Exit 1 for a broken run or wrong fallback
# counts, never for a missed target.
bench: build
	bench/fetch-projects.sh $(BENCH_PROJECTS)
	"$$($(CABAL) list-bin bench:haskell-reviewer-bench)" \
	    --exe "$$($(CABAL) list-bin exe:haskell-reviewer)" \
	    --runs $(BENCH_RUNS) \
	    $(if $(BENCH_JSON),--json $(BENCH_JSON)) \
	    $(if $(BENCH_REPORT),--report $(BENCH_REPORT)) \
	    $(if $(BENCH_LABEL),--label "$(BENCH_LABEL)") \
	    $(if $(BENCH_RTS),--rts "$(BENCH_RTS)") \
	    $(BENCH_ARGS) \
	    $(BENCH_PROJECTS)

# CI smoke test (D-76): checks the harness itself works, on one small
# project, with no timing checks; the fallback census and the refs
# queries are checked. Needs network the first time.
bench-smoke: build
	bench/fetch-projects.sh optparse-applicative
	"$$($(CABAL) list-bin bench:haskell-reviewer-bench)" \
	    --exe "$$($(CABAL) list-bin exe:haskell-reviewer)" \
	    --runs 1 --warmup 0 --steady-runs 1 --search-runs 1 --refs-runs 1 \
	    --nav-idle 2 \
	    optparse-applicative

# Dev only, not in CI: compare our CPP with gcc -traditional, run as GHC
# runs it, line by line, on fixtures/cpp and the bench projects (and any
# dirs in CPP_CHECK_DIRS). Prints the files that differ. Linux gcc only:
# macOS cpp is clang, which reads indented directives.
cpp-check:
	$(CABAL) build lib:haskell-reviewer
	$(CABAL) exec -v0 -- runghc dev/CppCheck.hs fixtures/cpp bench/projects $(CPP_CHECK_DIRS)

# Dev only, not in CI: the GHC oracle for resolution. Compiles the
# fixtures/resolve cases GHC can compile, with -ddump-minimal-imports,
# and writes fixtures/resolve/<case>.imports (commit them). Tests read
# those files and fail when a case changed since. Needs ghc-9.14.1 (or
# RESOLVE_CHECK_GHC) with its boot packages.
# The -X flags: the extensions the cabal file turns on for the whole
# project; runghc doesn't read them, and the script and ResolveDump use
# them.
resolve-check:
	$(CABAL) build lib:haskell-reviewer
	$(CABAL) exec -v0 -- runghc --ghc-arg=-XGHC2024 --ghc-arg=-XDuplicateRecordFields \
	    --ghc-arg=-XNoFieldSelectors --ghc-arg=-XOverloadedRecordDot --ghc-arg=-XOverloadedStrings \
	    --ghc-arg=-ihaskell-reviewer/test dev/ResolveCheck.hs fixtures/resolve

# Dev only, not in CI: the line diff against git on a repo's history
# (D-321). Clones LINEDIFF_CHECK_REPO (default: this repo; any local
# repo, only read) and diffs the files of its last LINEDIFF_CHECK_N
# first-parent commits (default 200), both modes, with ours and with git.
# Prints the share that is identical, the mismatches and our timings.
# Fails if one differs. Same -X flags as resolve-check. The script loads
# the test helpers (LineDiffSupport), so the test suite's dependencies
# are built first.
LINEDIFF_CHECK_REPO ?= .
LINEDIFF_CHECK_N ?= 200
linediff-check:
	$(CABAL) build lib:haskell-reviewer
	$(CABAL) build --only-dependencies haskell-reviewer:test:haskell-reviewer-test
	$(CABAL) exec -v0 -- runghc --ghc-arg=-XGHC2024 --ghc-arg=-XDuplicateRecordFields \
	    --ghc-arg=-XNoFieldSelectors --ghc-arg=-XOverloadedRecordDot --ghc-arg=-XOverloadedStrings \
	    --ghc-arg=-ihaskell-reviewer/test dev/LineDiffCheck.hs -n $(LINEDIFF_CHECK_N) $(LINEDIFF_CHECK_REPO)

# Dev only: CBOR on stdin, JSON on stdout. For a CBOR answer of the
# API, e.g. curl -s <url>/api/file/tree/a.hs | make -s cbor-json
cbor-json:
	@$(CABAL) build -v0 lib:haskell-reviewer
	@$(CABAL) exec -v0 -- runghc dev/CborJson.hs

# Rebuild the frontend bundle. Needs Node (version in web/.nvmrc).
# web/dist is committed: commit it after this.
web:
	cd web && npm ci && npm run build && npm run check:bundle

# Browser end-to-end tests (Playwright) against the built binary.
# First time: `cd web && npm ci && npx playwright install chromium firefox
# webkit`. They test web/dist as committed: run `make web` first if
# web/src changed. Pass Playwright flags in E2E_ARGS, e.g.
# make e2e E2E_ARGS='--project=chromium --grep-invert @slow'
e2e: build
	cd web && HASKELL_REVIEWER_EXE="$$($(CABAL) list-bin exe:haskell-reviewer)" \
	    npx playwright test $(E2E_ARGS)

# Not `cabal install`: it builds from an sdist, which would miss files
# outside the package (like ../web/dist). Copy the built binary.
#
# Also copies the highlighting rules (highlight/*.toml, SPEC §5.6) to
# the user config. A file that is already there and differs is kept as
# <name>.toml.bak first (.bak2, .bak3, ... if that one exists: no backup
# is ever overwritten).
install: build
	install -d "$(BINDIR)"
	install -m 0755 "$$($(CABAL) list-bin exe:haskell-reviewer)" "$(BINDIR)/haskell-reviewer"
	install -d "$(CONFDIR)/highlight"
	for f in highlight/*.toml; do \
	  to="$(CONFDIR)/$$f"; \
	  if [ -e "$$to" ] && ! cmp -s "$$f" "$$to"; then \
	    n=; while [ -e "$$to.bak$$n" ]; do n=$$(( $${n:-1} + 1 )); done; \
	    mv "$$to" "$$to.bak$$n" || exit 1; \
	  fi; \
	  install -m 0644 "$$f" "$$to" || exit 1; \
	done

# Pin all dependencies in cabal.project.freeze, except the packages that
# ship with GHC. Those differ between GHC versions, so with them pinned the
# file would only work with one GHC (we support three, D-64, D-77).
#
# Flag lines (`pkg +a -b`) are dropped too. Pinning a flag cabal picks
# by itself (like zlib's pkg-config) breaks the solve on machines where
# the other value is needed. Version pins are enough.
#
# How we find boot packages: in the build plan (dist-newstyle/cache/plan.json),
# cabal marks packages that come installed with GHC as "pre-existing".
# Needs jq. Stops if base is not in the list (jq or plan.json went wrong).
#
# The old freeze file is removed first, so we get a fresh solve against
# the current index (run `cabal update` before).
#
# haddock-library ships with GHC 9.12 and later, so with those this drops
# its line. GHC 9.10 builds it from Hackage: put
# `any.haddock-library ==1.11.0,` back by hand.
freeze:
	@command -v jq >/dev/null || { echo 'make freeze needs jq' >&2; exit 1; }
	rm -f cabal.project.freeze
	$(CABAL) freeze
	$(CABAL) build all --dry-run
	boot="$$(jq -r '."install-plan"[] | select(.type == "pre-existing") | ."pkg-name"' \
	    dist-newstyle/cache/plan.json | sort -u | tr '\n' ' ')"; \
	case " $$boot " in *" base "*) ;; \
	  *) echo "make freeze: no boot packages found in plan.json" >&2; exit 1;; \
	esac; \
	awk -v boot="$$boot" ' \
	  function add(e, name) { \
	    sub(/^[ \t]+/, "", e); sub(/,[ \t]*$$/, "", e); if (e == "") return; \
	    name = e; sub(/[ \t].*/, "", name); sub(/^any\./, "", name); \
	    if (e ~ /^[^ \t]+[ \t]+[-+]/) return; \
	    if (!(name in isboot)) kept[++n] = e \
	  } \
	  function flush(i) { \
	    if (n > 0) { \
	      printf "constraints: %s", kept[1]; \
	      for (i = 2; i <= n; i++) printf ",\n             %s", kept[i]; \
	      printf "\n" \
	    } \
	    inblock = 0 \
	  } \
	  BEGIN { split(boot, b, " "); for (i in b) isboot[b[i]] = 1 } \
	  /^constraints:/ { inblock = 1; sub(/^constraints:/, ""); add($$0); next } \
	  inblock && /^[ \t]/ { add($$0); next } \
	  { if (inblock) flush(); print } \
	  END { if (inblock) flush() } \
	' cabal.project.freeze > cabal.project.freeze.tmp
	mv cabal.project.freeze.tmp cabal.project.freeze
