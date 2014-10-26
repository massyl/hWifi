WIFI_SSID=AndroidAP-tony
NIX_ENV=/nix/store/clnpynyac3hx3a6z5lsy893p7b4rwnyf-nix-1.7/bin/nix-env
NIX_CHANNEL=/nix/store/clnpynyac3hx3a6z5lsy893p7b4rwnyf-nix-1.7/bin/nix-channel
NIX_SHELL=$(HOME)/.nix-profile/bin/nix-shell
NIX_INS=$(HOME)/.nix-profile/bin/nix-instantiate

pr:
	hub pull-request -b lambdatree:master

check-env:
	uname -a
	lsb_release -a

prepare-nix:
	whoami
	sudo mkdir /nix
	sudo chown travis: /nix
	sudo apt-get install -y libwww-curl-perl libdbd-sqlite3-perl

install-nix: check-env prepare-nix
	curl https://nixos.org/nix/install | sh
	# sudo find / -type f -name "nix-env"
	# sudo find / -type f -name "nix-channel"
	# wget http://hydra.nixos.org/build/10272830/download/1/nix_1.7-1_amd64.deb -O /tmp/nix_1.7-1_amd64.deb
	# sudo dpkg -i /tmp/nix_1.7-1_amd64.deb

setup-nix:
	. $(HOME)/.profile
	export PATH=$(HOME)/.nix-profile/bin:$(PATH)
	$(NIX_CHANNEL) --add http://nixos.org/channels/nixpkgs-unstable
	$(NIX_CHANNEL) --update
	ls -l $(HOME)/.nix-defexpr/channels/
	pwd
	ls -la $(HOME)
	[ -f $(HOME)/.nix-channels ] && cat $(HOME)/.nix-channels
	[ -d $(HOME)/.nix-profile ] && ls -l $(HOME)/.nix-profile
	[ -d $(HOME)/.nix-defexpr ] && ls -l $(HOME)/.nix-defexpr
	# $(NIX_ENV) -qaP | grep nix-shell
	# $(NIX_ENV) -i nix-shell

install: install-nix setup-nix run-nix-shell

cabal-init:
	cabal init

run:
	$(NIX_SHELL) --pure hwifi.nix --command	"cabal run"

test:
	$(NIX_SHELL) --pure hwifi.nix --command	"cabal test --show-details=always"

run-nix-shell:
	# echo $(NIX_PATH)
	# export NIX_PATH="nixpkgs=$(HOME)/.nix-defexpr/channels/nixpkgs:$(NIX_PATH)"
	# echo $(NIX_PATH)
	$(NIX_INS) -I"nixpkgs=$(HOME)/.nix-defexpr/channels/nixpkgs" --eval -E '<nixpkgs>'
	$(NIX_SHELL) -I"nixpkgs=$(HOME)/.nix-defexpr/channels/nixpkgs" --pure hwifi.nix

clean-wifi:
	sudo nmcli con delete id $(WIFI_SSID)

manual-release: build
	cp ./dist/build/hWifi/hWifi ~/.cabal/bin/
