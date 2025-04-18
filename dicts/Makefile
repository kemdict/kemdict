.ONESHELL:
.DEFAULT_GOAL := build

export LANG=en_US.UTF-8

.PHONY: mark_new
mark_new:
	touch heteronyms.json
	touch entries.db

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
	npx eask install-deps
	touch .eask

DICT_TARGETS := moedict-data-twblg/dict-twblg.json
DICT_TARGETS += kisaragi/kisaragi_dict.json
DICT_TARGETS += ministry-of-education/package.json
DICT_TARGETS += chhoetaigi/ChhoeTaigi_iTaigiHoataiTuichiautian.json
DICT_TARGETS += chhoetaigi/ChhoeTaigi_TaijitToaSutian.json
DICT_TARGETS += chhoetaigi/ChhoeTaigi_TaioanPehoeKichhooGiku.json
DICT_TARGETS += unihan.json

dicts: $(DICT_TARGETS)
.PHONY: dicts

entries.db: $(DICT_TARGETS) process-data.el .eask
	npx eask eval "(load \"process-data\")"

entries.db.gz: entries.db
	gzip --keep --force entries.db
	touch entries.db.gz

# Generate a Taiwanese names index from our digitization result of
# List of Plants of Formosa.
lopof-nan_TW.json lopof-hak_TW.json: list-of-plants-of-formosa/data/plants.sqlite
	bun lopof.ts

unihan.json:
	# 2024-12-27
	# Versions newer than this currently breaks for the set of fields that
	# I'm asking from it. So just pin it to the last known working
	# version. For now.
	# pip install unihan-etl==0.34.0 # this is done in CI
	# unihan-etl uses Unihan.zip from latest Unicode, but right
	# now does not appear to work with Unihan.zip from Unicode
	# 15.1. It did work with the one from Unicode 15.0, so use
	# that instead.
	mkdir -p ~/.cache/unihan_etl/downloads
	curl -L "https://www.unicode.org/Public/15.0.0/ucd/Unihan.zip" > ~/.cache/unihan_etl/downloads/Unihan.zip
	unihan-etl -F json -d $(abspath unihan.json) -f kAccountingNumeric kCangjie kCompatibilityVariant kDefinition kMandarin kOtherNumeric kPhonetic kPrimaryNumeric kRSUnicode kSemanticVariant kSimplifiedVariant kSpecializedSemanticVariant kTotalStrokes kTraditionalVariant kZVariant

kisaragi/kisaragi_dict.json: kisaragi/kisaragi-dict.org kisaragi/generate.el .eask
	npx eask eval "(load \"kisaragi/generate\")"

# - HanLoTaibunPoj -> title, because I've assumed that each entry has
#   a main title and I need to pick one. It's not an ideal assumption.
# - HuaBun -> definition, again because of Kemdict's assumptions.
# - The original header is (without the newline):
#     "DictWordID","PojUnicode","PojInput","KipUnicode","KipInput",
#     "HanLoTaibunPoj","HanLoTaibunKip","HoaBun","DataProvidedBy"
chhoetaigi/ChhoeTaigi_iTaigiHoataiTuichiautian.json: ChhoeTaigiDatabase/README.md
	mkdir -p chhoetaigi
	bun x csvtojson --headers='["id","poj","pojInput","kip","kipInput","titlePoj","title","definition","DataProvidedBy"]' ChhoeTaigiDatabase/ChhoeTaigiDatabase/ChhoeTaigi_iTaigiHoataiTuichiautian.csv > "$@"

# Original header:
# "DictWordID","PojUnicode","PojUnicodeOthers","PojInput","PojInputOthers","HanLoTaibunPoj","KaisoehHanLoPoj","LekuHanLoPoj","KipUnicode","KipUnicodeOthers","KipInput","KipInputOthers","HanLoTaibunKip","KaisoehHanLoKip","LekuHanLoKip","PageNumber","GoanchhehPoochhiongChuliau"
# HanLoTaibunPoj -> title
# KaisoehHanLoPoj -> definition
# LekuHanLoKip -> example
chhoetaigi/ChhoeTaigi_TaijitToaSutian.json: ChhoeTaigiDatabase/README.md
	mkdir -p chhoetaigi
	bun x csvtojson --ignoreColumns='/Page|Goanchheh/' --headers='["id","poj","pojOthers","pojInput","pojInputOthers","titlePoj","definitionPoj","examplePoj","kip","kipOthers","kipInput","kipInputOthers","title","definition","example","PageNumber","GoanchhehPoochhiongChuliau"]' ChhoeTaigiDatabase/ChhoeTaigiDatabase/ChhoeTaigi_TaijitToaSutian.csv > "$@"

# 1956 台灣白話基礎語句
# Original header:
# "DictWordID","PojUnicode","PojUnicodeOthers","PojInput","PojInputOthers","KipUnicode","KipUnicodeOthers","KipInput","KipInputOthers","HoaBun","EngBun","KaisoehEngbun","NounClassifier","LesuPoj","Opposite","LekuPoj","LekuEngbun","LekuHoabun","Confer","PageNumber"
#
# We don't add another title. We'll copy props.kip to props.title in
# process-data.
chhoetaigi/ChhoeTaigi_TaioanPehoeKichhooGiku.json: ChhoeTaigiDatabase/README.md
	mkdir -p chhoetaigi
	bun x csvtojson --ignoreColumns='/Page/' --headers='["id","poj","pojOthers","pojInput","pojInputOthers","kip","kipOthers","kipInput","kipInputOthers","zh","en","descEn","NounClassifier","例詞POJ","antonym","examplePOJ","exampleEn","exampleZh","參照","PageNumber"]' ChhoeTaigiDatabase/ChhoeTaigiDatabase/ChhoeTaigi_TaioanPehoeKichhooGiku.csv > "$@"

# Automatic submodule fetching. The specified file is just used to
# mark whether the submodule has been populated or not.
moedict-data-twblg/dict-twblg.json:
	git submodule update --init --depth 1 -- moedict-data-twblg

ministry-of-education/package.json:
	git submodule update --init --depth 1 -- ministry-of-education

ChhoeTaigiDatabase/README.md:
	git submodule update --init --depth 1 -- ChhoeTaigiDatabase

# If we don't want to use the committed database and want to generate it again
# instead, this is the place to do it.
list-of-plants-of-formosa/data/plants.sqlite:
	git submodule update --init --depth 1 -- list-of-plants-of-formosa
