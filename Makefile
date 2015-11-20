# sandbox variable, `n` means no sandbox (by default)
# otherwise, runs in sandbox environment (for ci or for local dev not in env-sandbox)
SANDBOX=n
# Default ssid to use
WIFI_SSID=AndroidAP-tony

env-sandbox:
	nix-shell --pure hwifi.nix

pr:
	hub pull-request -b lambdatree:master

check-env:
	uname -a
	lsb_release -a

install:
	./install-platform.sh

cabal-init:
	cabal init

clean-wifi:
	sudo nmcli con delete id $(WIFI_SSID)

manual-release: build
	cp ./dist/build/hWifi/hWifi ~/.cabal/bin/

to-nix:
	cabal2nix --shell . > hwifi.nix
	nix-shell hwifi.nix --command 'cabal configure'

init:
	./run.sh $(SANDBOX) "cabal update && cabal configure --enable-tests"

sandbox-init:
	./run.sh $(SANDBOX) "cabal sandbox init"

sandbox-delete:
	./run.sh $(SANDBOX) "cabal sandbox delete"

configure:
	./run.sh $(SANDBOX) "cabal configure"

build:
	./run.sh $(SANDBOX) "cabal build"

run:
	./run.sh $(SANDBOX) "cabal run"

test:
	./run.sh $(SANDBOX) "cabal test --show-details=always"
