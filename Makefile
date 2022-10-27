export LANG=en_US.UTF-8

.DEFAULT_GOAL := build

.PHONY: dev-11ty dev-tailwind dev

dev-11ty:
	npx "@11ty/eleventy" --input=./src --output=./_site --serve

dev-tailwind:
	npx tailwindcss --postcss -i css/src.css -o _site/b.css --watch

_site/b.css: css/src.css
	npx tailwindcss --minify --postcss -i css/src.css -o _site/b.css

_site: src/_data _site/b.css .eleventy.js
	npx "@11ty/eleventy" --quiet

gh-pages: src/_data _site/b.css
	npx "@11ty/eleventy" --pathprefix "kemdict" --quiet

src/_data: src/_data/combined.json

src/_data/combined.json: moedict-data/dict-revised.json moedict-data-twblg/dict-twblg.json
	cask eval "(load \"process-data\")"

dev: src/_data
	npx concurrently "make dev-11ty" "make dev-tailwind"

build: _site
