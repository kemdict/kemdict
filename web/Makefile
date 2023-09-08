export LANG=en_US.UTF-8
export KEMDICT_VERSION=$(shell git describe --tags --always)

.DEFAULT_GOAL := dist

DEPS := public/l $(wildcard *.js *.mjs *.cjs *.ts *.astro)

entries.db: ../dicts
	cd ../dicts && $(MAKE) entries.db
	([ -n "$$ANDROID_DATA" ] && cp ../dicts/entries.db "$@") || ln -f -T ../dicts/entries.db "$@"

.PHONY: dist
dist: $(DEPS)
	npx astro build
	cp package.json pnpm-lock.yaml dist/
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

check:
	npx astro check
