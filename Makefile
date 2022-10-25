export LANG=en_US.UTF-8

.DEFAULT_GOAL := build

.PHONY: dev-11ty dev-tailwind dev

dev-11ty:
	npx "@11ty/eleventy" --input=./src --output=./_site --serve

dev-tailwind:
	npx tailwindcss --postcss -i css/src.css -o _site/css/built.css --watch

_site/css/built.css: css/src.css
	npx tailwindcss --minify --postcss -i css/src.css -o _site/css/built.css

_site: _data _site/css/built.css
	npx "@11ty/eleventy" --input=./src --output=./_site

_data: _data/dict-moe-revised.json _data/dict-moe-twblg.json

_data/dict-moe-revised.json:
	mkdir -p _data
	cp moedict-data/dict-revised.json _data/dict-moe-revised.json

_data/dict-moe-twblg.json:
	mkdir -p _data
	cp moedict-data-twblg/dict-twblg.json _data/dict-moe-twblg.json

dev: _data
	npx concurrently "make dev-11ty" "make dev-tailwind"

build: _site
