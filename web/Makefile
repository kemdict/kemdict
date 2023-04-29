export LANG=en_US.UTF-8
export KEMDICT_VERSION=$(shell git describe --tags --always)

DEPS := public/l node_modules $(wildcard *.js *.mjs *.cjs *.ts *.astro)

node_modules: pnpm-lock.yaml
	pnpm install --frozen-lockfile

entries.db: ../dicts
	cd ../dicts && make entries.db
	([ -n "$$ANDROID_DATA" ] && cp ../dicts/entries.db "$@") || ln -f -T ../dicts/entries.db "$@"

.PHONY: dist
dist: $(DEPS)
	npx astro build
	cp package.json pnpm-lock.yaml dist/
	find server -type f -not -iname "readme*" -exec cp '{}' dist/ ';'

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
	make dist
	cd dist && bash start

# Currently this will succeed with a go fatal error at the end.
# That error goes to stderr while Astro's output thankfully goes to
# stdout.
# https://github.com/withastro/astro/issues/6306
check:
	npx astro check 2>/dev/null
	npx eslint .
