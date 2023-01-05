export LANG=en_US.UTF-8

.DEFAULT_GOAL := build

build:
	cd web && make build-no-data

dev:
	cd web && make dev
