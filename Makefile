CABAL ?= cabal
PREFIX ?= $(HOME)/.local
BINDIR ?= $(PREFIX)/bin
BENCH_RUNS ?= 10
BENCH_PROJECTS ?=
BENCH_JSON ?=
BENCH_LABEL ?=

.PHONY: build test install freeze web e2e bench bench-smoke cpp-check cbor-json

build:
	$(CABAL) build all

test:
	$(CABAL) test all --test-show-details=direct

# Runs the three reference projects (SPEC §10, §11.9; D-76, D-82, D-85).
# By hand, before a release: shared CI runners are too noisy for timings.
# Needs network the first time, to fetch the projects into bench/projects/
# (git-ignored).
bench: build
	bench/fetch-projects.sh $(BENCH_PROJECTS)
	"$$($(CABAL) list-bin bench:haskell-reviewer-bench)" \
	    --exe "$$($(CABAL) list-bin exe:haskell-reviewer)" \
	    --runs $(BENCH_RUNS) \
	    $(if $(BENCH_JSON),--json $(BENCH_JSON)) \
	    $(if $(BENCH_LABEL),--label "$(BENCH_LABEL)") \
	    $(BENCH_PROJECTS)

# CI smoke test (D-76): checks the harness itself works, on one small
# project, with no timing checks. Needs network the first time.
bench-smoke: build
	bench/fetch-projects.sh optparse-applicative
	"$$($(CABAL) list-bin bench:haskell-reviewer-bench)" \
	    --exe "$$($(CABAL) list-bin exe:haskell-reviewer)" \
	    --runs 1 --warmup 0 \
	    optparse-applicative

# Dev only, not in CI: compare our CPP with gcc -traditional, run as GHC
# runs it, line by line, on fixtures/cpp and the bench projects (and any
# dirs in CPP_CHECK_DIRS). Prints the files that differ. Linux gcc only:
# macOS cpp is clang, which reads indented directives.
cpp-check:
	$(CABAL) build lib:haskell-reviewer
	$(CABAL) exec -v0 -- runghc dev/CppCheck.hs fixtures/cpp bench/projects $(CPP_CHECK_DIRS)

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
install: build
	install -d "$(BINDIR)"
	install -m 0755 "$$($(CABAL) list-bin exe:haskell-reviewer)" "$(BINDIR)/haskell-reviewer"

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
