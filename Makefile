DEFAULT_GOAL: help

#==================== Hoogle ====================

.PHONY: hoogle-generate
## Generate hoogle
hoogle-generate:
	@cabal-hoogle generate

.PHONY: hoogle-server
## Run hoogle server
hoogle-server:
	@cabal-hoogle run -- server --local --port 9000

#==================== Install ===================

.PHONY: install
## Install the binaries
install:
	@cabal install --overwrite-policy=always

#===================== Help =====================

.PHONY: help
# Show help screen.
help:
	@echo "Please use \`make <target>' where <target> is one of\n\n"
	@awk '/^[a-zA-Z\-\_0-9]+:/ { \
		helpMessage = match(lastLine, /^## (.*)/); \
		if (helpMessage) { \
			helpCommand = substr($$1, 0, index($$1, ":")-1); \
			helpMessage = substr(lastLine, RSTART + 3, RLENGTH); \
			printf "%-30s %s\n", helpCommand, helpMessage; \
		} \
	} \
	{ lastLine = $$0 }' $(MAKEFILE_LIST)
