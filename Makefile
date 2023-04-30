.ONESHELL:
.DEFAULT_GOAL := build

.PHONY: build dev preview admin.deploy

export LANG=en_US.UTF-8

build:
	cd web && make build-no-data

dev:
	cd web && make dev

preview: build
	cd web && env PORT=5173 node dist/server/entry.mjs

admin.deploy.kemdict: build
	@[ "$$SSH_HOST"x != x ] || (echo 'Please specify $$SSH_HOST'; exit 1)
	cd web
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
