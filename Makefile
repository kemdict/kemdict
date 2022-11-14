export LANG=en_US.UTF-8

.cask: Cask
	cask install

combined.json: moedict-data-twblg/dict-twblg.json kisaragi/kisaragi_dict.json ministry-of-education/dict_idioms.json process-data.el .cask
	cask eval "(load \"process-data\")"

entries.db: combined.json
	node combined-to-sqlite.mjs

kisaragi/kisaragi_dict.json: kisaragi/kisaragi-dict.org kisaragi/generate.el .cask
	cask eval "(load \"kisaragi/generate\")"

moedict-data-twblg/dict-twblg.json:
	git submodule update --init -- moedict-data-twblg

ministry-of-education/dict_idioms.json:
	git submodule update --init -- ministry-of-education
