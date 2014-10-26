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

init:
	cabal update
	# cabal sandbox init
	# cabal install base process mtl QuickCheck HUnit text
	cabal configure --enable-tests

build:
	cabal build

run:
	cabal run

test:
	cabal test

clean-wifi:
	sudo nmcli con delete id $(WIFI_SSID)

nix-init:
	./sandbox-run.sh "cabal update; cabal configure --enable-tests"

nix-build:
	./sandbox-run.sh cabal build

nix-run:
	./sandbox-run.sh cabal run

nix-test:
	./sandbox-run.sh cabal test --show-details=always

manual-release: build
	cp ./dist/build/hWifi/hWifi ~/.cabal/bin/

cabal2nix:
	cabal2nix --sha256 dummy hWifi.cabal
