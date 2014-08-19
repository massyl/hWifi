WIFI_SSID=AndroidAP-tony

pr:
	hub pull-request -b lambdatree:master

install:
	uname -a
	sudo apt-get install -y haskell-platform network-manager checkbox
	cabal --version
	cabal update &&	cabal install cabal-install

deps:
	ghc --version
	cabal --version
	cabal install base process mtl QuickCheck HUnit text

sandbox-init:
	cabal sandbox init

sandbox-delete:
	cabal sandbox delete

cabal-init:
	cabal init

run:
	cabal run

test:
	cabal test --show-details=always

build:
	cabal configure --enable-tests
	cabal build

run-nix-shell:
	nix-shell --pure hwifi.nix

clean-wifi:
	sudo nmcli con delete id $(WIFI_SSID)
