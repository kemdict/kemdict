export LANG=en_US.UTF-8
export KEMDICT_VERSION=$(shell git describe --tags --always)

.DEFAULT_GOAL := dist

# NOTE this avoids having to clone that repository which is pretty big (since it
# contains raw data for the dictionaries)
../dicts/ministry-of-education/%:
	mkdir -p "$(dir $@)"
	curl -L "https://github.com/kemdict/kemdict-data-ministry-of-education/raw/refs/heads/main/$(patsubst ../dicts/ministry-of-education/%,%,$@)" > "$@"

MOE_DEPS := public/l $(addprefix ../dicts/ministry-of-education/,versions.ts kautian.ts)

# We had committed these to this repository so this one can feel free to depend
# on the submodule (as we don't need to run it in CI)
public/l:
	cd ../dicts && $(MAKE) ministry-of-education/package.json
	mkdir -p public/l/
	cp ../dicts/ministry-of-education/license/* public/l/

DEPS := $(MOE_DEPS) $(wildcard *.js *.mjs *.cjs *.ts *.astro)

entries.db: ../dicts
	cd ../dicts && $(MAKE) entries.db
	([ -n "$$ANDROID_DATA" ] && cp ../dicts/entries.db "$@") || ln -f -T ../dicts/entries.db "$@"

.PHONY: dist
dist: $(DEPS)
	npx astro build
	cp package.json pnpm-lock.yaml dist/
	find server/ -type f -not -iname "readme*" -exec cp '{}' dist/ ';'
	npx size-limit

.PHONY: dev build preview check build-no-data build-with-data

dev: $(DEPS) entries.db
	cd ../dicts && $(MAKE) props
	bunx --bun astro dev

build-with-data: dist entries.db
	cp --parents entries.db dist/

build-no-data: dist

preview: $(DEPS)
	$(MAKE) dist
	cd dist && bash start

check:
	bunx sv check
	bunx astro check
