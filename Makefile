.DEFAULT_GOAL = help
export SHELL = /bin/bash

build: ## build and lint
	stack build $(STACK_OPTS) hlivy
	find src -name '*.hs' | xargs hlint
	find src -name '*.hs' | xargs stylish-haskell -i
clean: ## clean
	stack clean
clobber: clean ## clean and remove stack's working directory
	rm -rf .stack-work/*
ghcid-devel: ## low-feature ghc-based IDE
	ghcid --command "stack ghci hlivy"
help: ## help screen
	@grep -E '^[a-zA-Z_-]+:.*?## .*$$' $(MAKEFILE_LIST) \
	| awk 'BEGIN {FS = ":.*?## "}; {printf "\033[36m%-18s\033[0m %s\n", $$1, $$2}'
setup: ## install required tools for this project
	brew update
	brew install stack
	stack install hlint stylish-haskell ghcid
