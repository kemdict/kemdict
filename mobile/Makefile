COMMON := common/index.ts common/db.ts
DEPS := assets/entries.db $(COMMON)

$(COMMON): common/%.ts: ../common/%.ts
	mkdir -p common
	([ -n "$$ANDROID_DATA" ] && cp "$<" "$@") || ln -f -T "$<" "$@"

assets/entries.db: ../dicts
	cd ../dicts && make entries.db
	([ -n "$$ANDROID_DATA" ] && cp ../dicts/entries.db "$@") || ln -f -T ../dicts/entries.db "$@"

dev: $(DEPS)
	npx expo "run:android" --device

dev.expo-go: $(DEPS)
	npx expo start # maybe set EXPO_DEBUG?

# web:
# 	npx expo start --web
