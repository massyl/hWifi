WIFI_SSID=AndroidAP-tony

pr:
	hub pull-request -b lambdatree:master

check-env:
	uname -a
	lsb_release -a

install:
	./.install-platform.sh

cabal-init:
	cabal init

run:
	nix-shell --pure hwifi.nix --command "cabal run"

test:
	nix-shell --pure hwifi.nix --command "cabal test --show-details=always"

run-nix-shell:
	nix-instantiate -I$(HOME)/.nix-defexpr/channels/nixpkgs --eval -E '<nixpkgs>'
	# $(NIX_SHELL) -I$(HOME)/.nix-defexpr/channels/nixpkgs --pure hwifi.nix

clean-wifi:
	sudo nmcli con delete id $(WIFI_SSID)

manual-release: build
	cp ./dist/build/hWifi/hWifi ~/.cabal/bin/
