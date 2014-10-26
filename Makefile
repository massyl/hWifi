WIFI_SSID=AndroidAP-tony

pr:
	hub pull-request -b lambdatree:master

check-env:
	uname -a
	lsb_release -a

install:
	./install-platform.sh

cabal-init:
	cabal init

build:
	cabal build

run:
	cabal run

test:
	cabal test

clean-wifi:
	sudo nmcli con delete id $(WIFI_SSID)

nix-build: setup
	./sandbox-run.sh build

nix-run:
	./sandbox-run.sh run

nix-test:
	./sandbox-run.sh "test --show-details=always"

manual-release: build
	cp ./dist/build/hWifi/hWifi ~/.cabal/bin/
