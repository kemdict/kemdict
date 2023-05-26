DEPS := $(wildcard *.js *.mjs *.cjs *.ts *.json) ../dicts/data.mjs

.PHONY: dev
dist: $(DEPS)
	npx unbuild

dev:
	npx vite dev
