CURRENT_UID := $(shell id -u)
CURRENT_GID := $(shell id -g)
MODELS_DIR ?= $(shell pwd)
PORT ?= 8421
OCAML_COMPILER := 5.1.0

DC_RUN_VARS := USER_NAME=${USER} \
CURRENT_UID=${CURRENT_UID} \
CURRENT_GID=${CURRENT_GID} \
OCAML_COMPILER=${OCAML_COMPILER} \
MODELS_DIR=${MODELS_DIR} \
PORT=${PORT} \


fmt:
	@dune build --auto-promote @fmt

build: fmt
	@dune build

run:
	@dune exec magizhchi

clean:
	@dune clean

WATCH ?= @all
watch:
	@dune build $(WATCH) -w


.PHONY: magizhchi
magizhchi:
	sudo ${DC_RUN_VARS} docker compose -f docker-compose.yml run --service-ports magizhchi bash

.PHONY: magizhchi-rebuild
magizhchi-rebuild:
	sudo ${DC_RUN_VARS} docker compose -f docker-compose.yml build

.PHONY: kill
kill:
	sudo docker kill $(shell docker ps -q)
