export LANG=en_US.UTF-8

.cask: Cask
	cask install

combined.json: moedict-data-twblg kisaragi/kisaragi_dict.json ministry-of-education process-data.el .cask
	cask eval "(load \"process-data\")"

entries.db: combined.json combined-to-sqlite.mjs
	node combined-to-sqlite.mjs

entries.db.gz: entries.db
	gzip --keep --force entries.db
	touch entries.db.gz

kisaragi/kisaragi_dict.json: kisaragi/kisaragi-dict.org kisaragi/generate.el .cask
	cask eval "(load \"kisaragi/generate\")"

moedict-data-twblg:
	git submodule update --init -- moedict-data-twblg

ministry-of-education:
	git submodule update --init -- ministry-of-education
