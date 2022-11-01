export LANG=en_US.UTF-8

.DEFAULT_GOAL := build

.PHONY: dev-11ty dev-tailwind dev process-data

# * Processing dictionary data into one combined file
src/_data: src/_data/combined.json kisaragi-dict/kisaragi_dict.json

src/_data/combined.json: moedict-data/dict-revised.json moedict-data-twblg/dict-twblg.json kisaragi-dict/kisaragi_dict.json process-data.el
	cask eval "(load \"process-data\")"

src/_data/kisaragi_dict.json: kisaragi-dict/kisaragi_dict.json
	cp kisaragi-dict/kisaragi_dict.json src/_data/

kisaragi-dict/kisaragi_dict.json: kisaragi-dict/kisaragi-dict.org kisaragi-dict/generate.el
	cask eval "(load \"kisaragi-dict/generate\")"

moedict-data/dict-revised.json:
	git submodule update --init -- moedict-data

moedict-data-twblg/dict-twblg.json:
	git submodule update --init -- moedict-data-twblg

# * Development tasks
dev-11ty:
	npx "@11ty/eleventy" --input=./src --output=./_site --serve

dev-tailwind:
	npx tailwindcss --postcss -i css/src.css -o _site/b.css --watch

dev: _site/s.js
	export DEV=true
	make src/_data
	npx concurrently "make dev-11ty" "make dev-tailwind"

# * Making the site itself
_site: src/*.njk src/_data _site/b.css _site/s.js .eleventy.js
	npx "@11ty/eleventy" --quiet

gh-pages: src/_data _site/b.css
	npx "@11ty/eleventy" --pathprefix "kemdict" --quiet

build: _site

public.zip: _site
	cd _site/ && 7z a ../public.zip .

_site/b.css: css/src.css
	npx tailwindcss --minify --postcss -i css/src.css -o _site/b.css

_site/%.js: client-side-js/%.ts .babelrc.json
	npx babel "$<" --out-file "$@"

