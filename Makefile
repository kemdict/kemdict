export LANG=en_US.UTF-8

.DEFAULT_GOAL := build

# * Processing dictionary data into one combined file
src/lib: src/lib/combined.json src/lib/kisaragi_dict.json

src/lib/combined.json: dicts/moedict-data-twblg/dict-twblg.json dicts/kisaragi/kisaragi_dict.json dicts/ministry-of-education/dict_idioms.json dicts/process-data.el
	cask eval "(load \"dicts/process-data\")"
	cp dicts/combined.json dicts/titles.json src/lib

src/lib/kisaragi_dict.json: dicts/kisaragi/kisaragi_dict.json
	cp dicts/kisaragi/kisaragi_dict.json src/lib/

dicts/kisaragi/kisaragi_dict.json: dicts/kisaragi/kisaragi-dict.org dicts/kisaragi/generate.el
	cask eval "(load \"dicts/kisaragi/generate\")"

dicts/moedict-data-twblg/dict-twblg.json:
	git submodule update --init -- dicts/moedict-data-twblg

dicts/ministry-of-education/dict_idioms.json:
	git submodule update --init -- dicts/ministry-of-education

# * Development tasks
.PHONY: dev kisaragi-dict-rebuild clear-dev-dicts

dev-tailwind:
	npx tailwindcss --postcss -i css/src.css -o _site/b.css --watch

kisaragi-dict-rebuild: dicts/kisaragi/kisaragi_dict.json
clear-dev-dicts:
	rm dev-dict*.json

static: dicts/ministry-of-education/license
	cp -r dicts/ministry-of-education/license static/l

dev: static src/lib
	npx vite dev

build: static src/lib
	npx vite build

preview:
	npx vite preview

