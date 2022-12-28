export LANG=en_US.UTF-8
export KEMDICT_VERSION=$(shell git describe --tags)

.DEFAULT_GOAL := build

MD = $(patsubst pages/%.md,src/routes/%/+page.svelte.md,$(wildcard pages/*.md))

$(MD): src/routes/%/+page.svelte.md: pages/%.md
	@mkdir -p "$(dir $@)"
	ln -f -T "$<" "$@"

markdown: $(MD)

src/lib/entries.db.gz: ../dicts
	cd ../dicts && make entries.db.gz
	cp ../dicts/entries.db.gz src/lib/
	cp ../dicts/entries.db src/lib/

src/lib/kisaragi_dict.json: ../dicts
	cd ../dicts && make kisaragi/kisaragi_dict.json
	cp ../dicts/kisaragi/kisaragi_dict.json src/lib/

DEPS = markdown static/l src/lib/kisaragi_dict.json src/lib/entries.db.gz

dev: $(DEPS)
	npx vite dev

# Sveltekit's default tasks, moved here from package.json
check:
	npx svelte-kit sync && npx svelte-check

check-watch:
	npx svelte-kit sync && npx svelte-check --watch

.PHONY: check check-watch

# This refers to a folder here.
build: $(DEPS)
	npx vite build

# Build as a node application ready to be run after an npm install
# Runtime tasks are declared in package.json
built-node: $(DEPS)
	env ADAPTER=node npx vite build
	cp package.json package-lock.json built-node/
	cp --parents src/lib/entries.db built-node/

static/l: ../dicts
	cd ../dicts && make ministry-of-education
	mkdir -p static/l
	cp ../dicts/ministry-of-education/license/* static/l/

.PHONY: dev
