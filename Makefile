all: setup build test lint

.PHONY: setup
setup:
	stack setup
	stack build --dependencies-only --test --no-run-tests
	stack install hlint weeder

.PHONY: build
build:
	stack build --pedantic --test --no-run-tests

.PHONY: test
test:
	stack test

.PHONY: lint
lint:
	hlint .
	weeder .

.PHONY: haddock
haddock:
	stack haddock --open

.PHONY: hoogle
hoogle:
	stack hoogle -- generate --local
	stack hoogle -- server --local --port=8080

.PHONY: clean
clean:
	stack clean
	-rm devices.db

devices.db: data/devices.sql
	-@rm devices.db
	sqlite3 devices.db < data/devices.sql
