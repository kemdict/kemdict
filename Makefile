export LANG=en_US.UTF-8

.DEFAULT_GOAL := build

.PHONY: dev-11ty dev-tailwind dev process-data

dev-11ty:
	npx "@11ty/eleventy" --input=./src --output=./_site --serve

dev-tailwind:
	npx tailwindcss --postcss -i css/src.css -o _site/b.css --watch

_site/b.css: css/src.css
	npx tailwindcss --minify --postcss -i css/src.css -o _site/b.css

_site: src/*.njk src/_data _site/b.css .eleventy.js
	npx "@11ty/eleventy" --quiet

gh-pages: src/_data _site/b.css
	npx "@11ty/eleventy" --pathprefix "kemdict" --quiet

src/_data: src/_data/combined.json kisaragi-dict/kisaragi_dict.json
	cp kisaragi-dict/kisaragi_dict.json src/_data/

src/_data/combined.json: moedict-data/dict-revised.json moedict-data-twblg/dict-twblg.json kisaragi-dict/kisaragi_dict.json process-data.el
	make process-data

process-data:
	cask eval "(load \"process-data\")"

dev:
	export DEV=true
	make src/_data
	npx concurrently "make dev-11ty" "make dev-tailwind"

build: _site

public.zip: _site
	cd _site/ && 7z a ../public.zip .
