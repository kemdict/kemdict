export LANG=en_US.UTF-8

.DEFAULT_GOAL := build

.PHONY: clean zip css dev

dev-hugo:
	hugo server

dev-tailwind:
	npx tailwindcss --postcss -i css/src.css -o static/css/built.css --watch

dev:
	npx concurrently "make dev-hugo" "make dev-tailwind"

static/css/built.css: css/src.css
	npx tailwindcss --minify --postcss -i css/src.css -o static/css/built.css

public: static/css/built.css
	hugo --minify
