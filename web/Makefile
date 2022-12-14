export LANG=en_US.UTF-8
export KEMDICT_VERSION=$(shell git describe --tags --always)

.DEFAULT_GOAL := build-no-data

MD = $(patsubst pages/%.md,src/routes/%/+page.svelte.md,$(wildcard pages/*.md))

$(MD): src/routes/%/+page.svelte.md: pages/%.md
	@mkdir -p "$(dir $@)"
	([ -n "$$ANDROID_DATA" ] && cp "$<" "$@") || ln -f -T "$<" "$@"

markdown: $(MD)

src/lib/entries.db: ../dicts
	cd ../dicts && make entries.db
	cp ../dicts/entries.db src/lib/

src/lib/kisaragi_dict.json: ../dicts
	cp ../dicts/kisaragi/kisaragi_dict.json src/lib/

DEPS := markdown static/l src/lib/kisaragi_dict.json

dev: $(DEPS) src/lib/entries.db
	npx vite dev

# Sveltekit's default tasks, moved here from package.json
check:
	npx svelte-kit sync && npx svelte-check

check-watch:
	npx svelte-kit sync && npx svelte-check --watch

.PHONY: check check-watch

# Build as a node application ready to be run after an npm install
# Runtime tasks are declared in package.json
built-node: $(DEPS)
	npx vite build
	cp package.json package-lock.json built-node/

build-with-data: built-node src/lib/entries.db
	cp --parents src/lib/entries.db built-node/

build-no-data: built-node

static/l: ../dicts
	cd ../dicts && make ministry-of-education/package.json
	mkdir -p static/l
	cp ../dicts/ministry-of-education/license/* static/l/

.PHONY: dev build-no-data build-with-data
