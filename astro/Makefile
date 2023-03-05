export LANG=en_US.UTF-8
export KEMDICT_VERSION=$(shell git describe --tags --always)

DEPS := public/l node_modules

node_modules: package.json
	npm install

entries.db: ../dicts
	cd ../dicts && make entries.db
	([ -n "$$ANDROID_DATA" ] && cp ../dicts/entries.db "$@") || ln -f -T ../dicts/entries.db "$@"

dist: $(DEPS)
	npx astro build
	cp package.json package-lock.json dist/

public/l: ../dicts
	cd ../dicts && make ministry-of-education/package.json
	mkdir -p public/l/
	cp ../dicts/ministry-of-education/license/* public/l/

.PHONY: dev build preview check build-no-data build-with-data

dev: $(DEPS) entries.db
	npx astro dev

build-with-data: dist entries.db
	cp --parents entries.db dist/

build-no-data: dist

preview: $(DEPS)
	npx astro preview

# TODO
check:
	npx svelte-check
