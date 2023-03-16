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

admin.deploy: build
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
