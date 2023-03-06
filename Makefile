export LANG=en_US.UTF-8

.DEFAULT_GOAL := build

build:
	cd web && make build-no-data

dev:
	cd web && make dev

preview: build
	cd web && env PORT=5173 node dist/server/entry.mjs
