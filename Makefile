.ONESHELL:
.DEFAULT_GOAL := build

.PHONY: dev-pojtl-api preview
.PHONY: admin.deploy.kemdict admin.deploy.pojtl-api

export LANG=en_US.UTF-8

.PHONY: build
build:
	cd web && make -j4 build-no-data

dev.web:
	cd web && make -j4 dev

.PHONY: dev
dev:
	npx concurrently "make dev.web"

dev-pojtl-api:
	cd pojtl-api && make dev

preview: build
	cd web && env PORT=5173 bun dist/server/entry.mjs

admin.deploy.kemdict: build
	@[ "$$ANDROID_DATA"x != x ] && (echo "The database is reduced on Android to make it fit in my phone's RAM for testing. Deploying here would use this reduced database. Exiting."; exit 1)
	@[ "$$SSH_HOST"x == x ] && (echo 'Please specify $$SSH_HOST'; exit 1)
	cd web

	# Still have to figure out a way to dostinguish between missing DB
	# and every other error
	# @echo "Testing that the application starts..."
	# (cd dist && ./start &)
	# pid=$$!
	# sleep 15
	# ps -p $$pid > /dev/null || exit 1
	# kill $$pid

	tar -czf dist.tar.gz -a dist
	rsync dist.tar.gz "$$SSH_HOST:/home/kisaragi"
	ssh "$$SSH_HOST" bash << HERE
	  tar -xf dist.tar.gz
	  rm dist.tar.gz
	  mkdir -p deployed
	  test -d deployed/kemdict && rm -r deployed/kemdict
	  mv dist deployed/kemdict
	  systemctl restart --user kemdict
	HERE

admin.deploy.pojtl-api:
	@[ "$$SSH_HOST"x != x ] || (echo 'Please specify $$SSH_HOST'; exit 1)
	tar -czf pojtl-api.tar.gz -a pojtl-api
	rsync pojtl-api.tar.gz "$$SSH_HOST:/home/kisaragi"
	ssh "$$SSH_HOST" bash << HERE
	  tar -xf pojtl-api.tar.gz
	  rm pojtl-api.tar.gz
	  mkdir -p deployed
	  test -d deployed/pojtl-api && rm -r deployed/pojtl-api
	  mv pojtl-api deployed/pojtl-api
	  systemctl restart --user pojtl-api
	HERE
