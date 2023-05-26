DEPS := $(wildcard src/* *.js *.mjs *.cjs *.ts *.json) ../dicts/data.mjs

dist: $(DEPS)
	npx unbuild

dev: $(DEPS)
	npx chokidar $(DEPS) --debounce 1000 --command "$(MAKE) dist"
