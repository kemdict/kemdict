export LANG=en_US.UTF-8
export KEMDICT_VERSION=$(shell git describe --tags --always)

DEPS := public/l node_modules $(wildcard *.js *.mjs *.cjs *.ts *.astro) common

common: ../common
	cd ../common && $(MAKE) dist

entries.db: ../dicts
	cd ../dicts && $(MAKE) entries.db
	([ -n "$$ANDROID_DATA" ] && cp ../dicts/entries.db "$@") || ln -f -T ../dicts/entries.db "$@"

.PHONY: dist
dist: $(DEPS)
	npx astro build
	cp package.json dist/
	touch dist/pnpm-workspace.yaml # to mark it as a new root
	find server/ -type f -not -iname "readme*" -exec cp '{}' dist/ ';'
	mkdir -p dist/hack
	find hack/ -type f -not -iname "readme*" -exec cp '{}' dist/hack/ ';'
	npx size-limit

public/l: ../dicts
	cd ../dicts && $(MAKE) ministry-of-education/package.json
	mkdir -p public/l/
	cp ../dicts/ministry-of-education/license/* public/l/

.PHONY: dev build preview check build-no-data build-with-data

dev: $(DEPS) entries.db
	npx astro dev

build-with-data: dist entries.db
	cp --parents entries.db dist/

build-no-data: dist

preview: $(DEPS)
	$(MAKE) dist
	cd dist && bash start

# Currently this will succeed with a go fatal error at the end.
# That error goes to stderr while Astro's output thankfully goes to
# stdout.
# https://github.com/withastro/astro/issues/6306
check:
	npx astro check 2>/dev/null
	npx eslint .
