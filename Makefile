export LANG=en_US.UTF-8

.DEFAULT_GOAL := build

build: entries.db
.PHONY: build

.cask: Cask
	cask install

DICT_TARGETS := moedict-data-twblg/dict-twblg.json
DICT_TARGETS += kisaragi/kisaragi_dict.json
DICT_TARGETS += ministry-of-education/package.json
DICT_TARGETS += itaigi/ChhoeTaigi_iTaigiHoataiTuichiautian.json

combined.json: $(DICT_TARGETS) process-data.el .cask
	cask eval "(load \"process-data\")"

entries.db: combined.json combined-to-sqlite.mjs
	node combined-to-sqlite.mjs

entries.db.gz: entries.db
	gzip --keep --force entries.db
	touch entries.db.gz

kisaragi/kisaragi_dict.json: kisaragi/kisaragi-dict.org kisaragi/generate.el .cask
	cask eval "(load \"kisaragi/generate\")"

# - HanLoTaibunPoj -> title, because I've assumed that each entry has
#   a main title and I need to pick one. It's not an ideal assumption.
# - HuaBun -> definition, again because of Kemdict's assumptions.
# - HACK: id -> het_sort, to make it work with the code originally
#   written to deal with dict_revised.
# - Dropping the Input columns -- which use numbers to represent tones,
#   not unicode sequences -- because they're not needed.
# - The original header is (without the newline):
#     "DictWordID","PojUnicode","PojInput","KipUnicode","KipInput",
#     "HanLoTaibunPoj","HanLoTaibunKip","HoaBun","DataProvidedBy"
itaigi/ChhoeTaigi_iTaigiHoataiTuichiautian.json: ChhoeTaigiDatabase/README.md
	mkdir -p itaigi
	npx csvtojson --ignoreColumns='/Input/' --headers='["het_sort","poj","PojInput","kip","KipInput","title","HanLoTaibunKip","definition","DataProvidedBy"]' ChhoeTaigiDatabase/ChhoeTaigiDatabase/ChhoeTaigi_iTaigiHoataiTuichiautian.csv > "$@"

# Automatic submodule fetching. The specified file is just used to
# mark whether the submodule has been populated or not.
moedict-data-twblg/dict-twblg.json:
	git submodule update --init --depth 1 -- moedict-data-twblg

ministry-of-education/package.json:
	git submodule update --init --depth 1 -- ministry-of-education

ChhoeTaigiDatabase/README.md:
	git submodule update --init --depth 1 -- ChhoeTaigiDatabase
