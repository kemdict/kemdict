export LANG=en_US.UTF-8

.cask: Cask
	cask install

combined.json: moedict-data-twblg/dict-twblg.json kisaragi/kisaragi_dict.json ministry-of-education/package.json process-data.el .cask
	cask eval "(load \"process-data\")"

entries.db: combined.json combined-to-sqlite.mjs
	node combined-to-sqlite.mjs

entries.db.gz: entries.db
	gzip --keep --force entries.db
	touch entries.db.gz

kisaragi/kisaragi_dict.json: kisaragi/kisaragi-dict.org kisaragi/generate.el .cask
	cask eval "(load \"kisaragi/generate\")"

moedict-data-twblg/dict-twblg.json:
	git submodule update --init --depth 1 -- moedict-data-twblg

ministry-of-education/package.json:
	git submodule update --init --depth 1 -- ministry-of-education
