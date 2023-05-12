DEPS := $(wildcard *.js *.mjs *.cjs *.ts *.json) ../dicts/data.mjs

.PHONY: dev
dist: $(DEPS)
	npx vite build

dev:
	npx vite dev
