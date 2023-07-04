.ONESHELL:
.DEFAULT_GOAL := build

export LANG=en_US.UTF-8

admin.deploy: entries.db.gz
	@[ "$$SSH_HOST"x != x ] || (echo 'Please specify $$SSH_HOST'; exit 1)
	rsync entries.db.gz "$$SSH_HOST:/home/kisaragi/deployed/kemdict.db.gz"
	ssh "$$SSH_HOST" bash << HERE
	  gzip -d -f deployed/kemdict.db.gz
	  systemctl restart --user kemdict
	HERE

build: entries.db
.PHONY: build

.eask: Eask
	eask install-deps
	touch .eask

node_modules: package.json
	pnpm install

DICT_TARGETS := moedict-data-twblg/dict-twblg.json
DICT_TARGETS += kisaragi/kisaragi_dict.json
DICT_TARGETS += ministry-of-education/package.json
DICT_TARGETS += chhoetaigi/ChhoeTaigi_iTaigiHoataiTuichiautian.json
DICT_TARGETS += chhoetaigi/ChhoeTaigi_TaijitToaSutian.json
DICT_TARGETS += chhoetaigi/ChhoeTaigi_TaioanPehoeKichhooGiku.json
DICT_TARGETS += unihan.json

dicts: $(DICT_TARGETS)
.PHONY: dicts

heteronyms.json: $(DICT_TARGETS) process-data.el .eask node_modules
	eask eval "(load \"process-data\")"

entries.db: heteronyms.json heteronyms-to-sqlite.mjs
	node heteronyms-to-sqlite.mjs

entries.db.gz: entries.db
	gzip --keep --force entries.db
	touch entries.db.gz

unihan.json:
	unihan-etl -F json -d $(abspath unihan.json) -f kAccountingNumeric kCangjie kCompatibilityVariant kDefinition kMandarin kOtherNumeric kPhonetic kPrimaryNumeric kRSUnicode kSemanticVariant kSimplifiedVariant kSpecializedSemanticVariant kTotalStrokes kTraditionalVariant kZVariant

kisaragi/kisaragi_dict.json: kisaragi/kisaragi-dict.org kisaragi/generate.el .eask
	eask eval "(load \"kisaragi/generate\")"

# FIXME: I'm just throwing away *UnicodeOthers when it contains
# alternative pronunciations.
#
# - HanLoTaibunPoj -> title, because I've assumed that each entry has
#   a main title and I need to pick one. It's not an ideal assumption.
# - HuaBun -> definition, again because of Kemdict's assumptions.
# - Dropping the Input columns -- which use numbers to represent tones,
#   not unicode sequences -- because they're not needed.
# - The original header is (without the newline):
#     "DictWordID","PojUnicode","PojInput","KipUnicode","KipInput",
#     "HanLoTaibunPoj","HanLoTaibunKip","HoaBun","DataProvidedBy"
chhoetaigi/ChhoeTaigi_iTaigiHoataiTuichiautian.json: ChhoeTaigiDatabase/README.md
	mkdir -p chhoetaigi
	npx csvtojson --ignoreColumns='/Input/' --headers='["id","poj","PojInput","kip","KipInput","titlePoj","title","definition","DataProvidedBy"]' ChhoeTaigiDatabase/ChhoeTaigiDatabase/ChhoeTaigi_iTaigiHoataiTuichiautian.csv > "$@"

# Original header:
# "DictWordID","PojUnicode","PojUnicodeOthers","PojInput","PojInputOthers","HanLoTaibunPoj","KaisoehHanLoPoj","LekuHanLoPoj","KipUnicode","KipUnicodeOthers","KipInput","KipInputOthers","HanLoTaibunKip","KaisoehHanLoKip","LekuHanLoKip","PageNumber","GoanchhehPoochhiongChuliau"
# HanLoTaibunPoj -> title
# KaisoehHanLoPoj -> definition
# LekuHanLoKip -> example
chhoetaigi/ChhoeTaigi_TaijitToaSutian.json: ChhoeTaigiDatabase/README.md
	mkdir -p chhoetaigi
	npx csvtojson --ignoreColumns='/Input|Page|Others|Goanchheh/' --headers='["id","poj","PojUnicodeOthers","PojInput","PojInputOthers","titlePoj","definitionPoj","examplePoj","kip","KipUnicodeOthers","KipInput","KipInputOthers","title","definition","example","PageNumber","GoanchhehPoochhiongChuliau"]' ChhoeTaigiDatabase/ChhoeTaigiDatabase/ChhoeTaigi_TaijitToaSutian.csv > "$@"

# 1956 台灣白話基礎語句
# Original header:
# "DictWordID","PojUnicode","PojUnicodeOthers","PojInput","PojInputOthers","KipUnicode","KipUnicodeOthers","KipInput","KipInputOthers","HoaBun","EngBun","KaisoehEngbun","NounClassifier","LesuPoj","Opposite","LekuPoj","LekuEngbun","LekuHoabun","Confer","PageNumber"
#
# We don't add another title. We'll copy props.kip to props.title in
# process-data.
chhoetaigi/ChhoeTaigi_TaioanPehoeKichhooGiku.json: ChhoeTaigiDatabase/README.md
	mkdir -p chhoetaigi
	npx csvtojson --ignoreColumns='/Input|Page|Others/' --headers='["id","poj","PojUnicodeOthers","PojInput","PojInputOthers","kip","KipUnicodeOthers","KipInput","KipInputOthers","zh","en","descEn","NounClassifier","例詞POJ","antonym","examplePOJ","exampleEn","exampleZh","參照","PageNumber"]' ChhoeTaigiDatabase/ChhoeTaigiDatabase/ChhoeTaigi_TaioanPehoeKichhooGiku.csv > "$@"

# Automatic submodule fetching. The specified file is just used to
# mark whether the submodule has been populated or not.
moedict-data-twblg/dict-twblg.json:
	git submodule update --init --depth 1 -- moedict-data-twblg

ministry-of-education/package.json:
	git submodule update --init --depth 1 -- ministry-of-education

ChhoeTaigiDatabase/README.md:
	git submodule update --init --depth 1 -- ChhoeTaigiDatabase
