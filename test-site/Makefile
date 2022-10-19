export LANG=en_US.UTF-8

.DEFAULT_GOAL := build

.PHONY: clean zip css dev

dev-11ty:
	npx "@11ty/eleventy" --serve

dev-tailwind:
	npx tailwindcss --postcss -i css/src.css -o _site/css/built.css --watch

dev:
	npx concurrently "make dev-11ty" "make dev-tailwind"

static/css/built.css: css/src.css
	npx tailwindcss --minify --postcss -i css/src.css -o static/css/built.css

_site: static/css/built.css
	npx "@11ty/eleventy"
