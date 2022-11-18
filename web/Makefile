export LANG=en_US.UTF-8

.DEFAULT_GOAL := build

src/lib/entries.db.gz: ../dicts
	cd ../dicts && make entries.db.gz
	cp ../dicts/entries.db.gz src/lib/
	cp ../dicts/titles.json static/
	cp ../dicts/titles.json src/lib/

src/lib/kisaragi_dict.json:
	cd ../dicts && make kisaragi/kisaragi_dict.json
	cp ../dicts/kisaragi/kisaragi_dict.json src/lib/

dev: src/lib/kisaragi_dict.json src/lib/entries.db.gz
	npx vite dev

# This refers to a folder here.
build: static/l src/lib/kisaragi_dict.json src/lib/entries.db.gz
	npx vite build

static/l: ../dicts
	cd ../dicts && make ministry-of-education
	mkdir -p static/l
	cp ../dicts/ministry-of-education/license/* static/l/

.PHONY: dev