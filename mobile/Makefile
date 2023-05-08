DEPS := assets/entries.db common

common: ../common
	cd ../common && make dist

assets/entries.db: ../dicts
	cd ../dicts && make entries.db
	([ -n "$$ANDROID_DATA" ] && cp ../dicts/entries.db "$@") || ln -f -T ../dicts/entries.db "$@"

dev: $(DEPS)
	npx expo "run:android" --device

dev.expo-go: $(DEPS)
	npx expo start # maybe set EXPO_DEBUG?

build.android.debug: $(DEPS)
	cd android && ./gradlew installRelease
	find . -name "*.apk"

# web:
# 	npx expo start --web
