CABAL ?= cabal
PREFIX ?= $(HOME)/.local
BINDIR ?= $(PREFIX)/bin

.PHONY: build test install freeze

build:
	$(CABAL) build all

test:
	$(CABAL) test all --test-show-details=direct

# Not `cabal install`: it builds from an sdist, which would miss files
# outside the package (like ../web/dist later). Copy the built binary.
install: build
	install -d "$(BINDIR)"
	install -m 0755 "$$($(CABAL) list-bin exe:haskell-reviewer)" "$(BINDIR)/haskell-reviewer"

# Pin all dependencies in cabal.project.freeze, except the packages that
# ship with GHC. Those differ between GHC versions, so with them pinned the
# file would only work with one GHC (we support three, D-64, D-77).
#
# How we find them: in the build plan (dist-newstyle/cache/plan.json),
# cabal marks packages that come installed with GHC as "pre-existing".
# Needs jq. Stops if base is not in the list (jq or plan.json went wrong).
#
# The old freeze file is removed first, so we get a fresh solve against
# the current index (run `cabal update` before).
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
