export LANG=en_US.UTF-8

.DEFAULT_GOAL := dist
DEPS := $(wildcard *.js *.mjs *.cjs *.ts *.astro)

entries.db: ../dicts
	cd ../dicts && $(MAKE) entries.db
	([ -n "$$ANDROID_DATA" ] && cp ../dicts/entries.db "$@") || ln -f -T ../dicts/entries.db "$@"

dist: $(DEPS)
	npx astro build

dev:
	npx astro dev
