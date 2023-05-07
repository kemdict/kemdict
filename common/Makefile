DEPS := $(wildcard *.js *.mjs *.cjs *.ts *.json)

.PHONY: dev
dist: $(DEPS)
	npx vite build

dev:
	npx vite dev
