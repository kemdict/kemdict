export LANG=en_US.UTF-8

.DEFAULT_GOAL := build

build:
	cd astro && make build-no-data

dev:
	cd web && make dev

preview: build
	cd astro && env PORT=5173 node dist/server/entry.mjs
