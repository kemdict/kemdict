export LANG=en_US.UTF-8

.DEFAULT_GOAL := build

.PHONY: dev-11ty dev-tailwind dev

dev-11ty:
	npx "@11ty/eleventy" --input=./src --output=./_site --serve

dev-tailwind:
	npx tailwindcss --postcss -i css/src.css -o _site/css/built.css --watch

_site/css/built.css: css/src.css
	npx tailwindcss --minify --postcss -i css/src.css -o _site/css/built.css

_site: src/_data _site/css/built.css
	npx "@11ty/eleventy" --input=./src --output=./_site --quiet

src/_data: src/_data/combined.json

src/_data/combined.json: src/_data/dict-moe-revised.json src/_data/dict-moe-twblg.json process-data.el
	emacs -batch -f batch-byte-compile process-data.el
	cask emacs --script process-data.elc

src/_data/dict-moe-revised.json:
	mkdir -p src/_data
	cp moedict-data/dict-revised.json src/_data/dict-moe-revised.json

src/_data/dict-moe-twblg.json:
	mkdir -p src/_data
	cp moedict-data-twblg/dict-twblg.json src/_data/dict-moe-twblg.json

dev: src/_data
	npx concurrently "make dev-11ty" "make dev-tailwind"

build: _site
