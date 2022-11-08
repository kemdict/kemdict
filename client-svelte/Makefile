export LANG=en_US.UTF-8

.DEFAULT_GOAL := build

static/combined.json:
	cd ../dicts && make combined.json
	cp ../dicts/combined.json ../dicts/titles.json src/static/
	cp ../dicts/titles.json src/lib/

src/lib/kisaragi_dict.json:
	cd ../dicts && make kisaragi/kisaragi_dict.json
	cp ../dicts/kisaragi/kisaragi_dict.json src/lib/

dev: static/combined.json src/lib/kisaragi_dict.json
	npx vite dev

build: static/combined.json
	npx vite build

.PHONY: build dev
