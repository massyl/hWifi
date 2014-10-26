WIFI_SSID=AndroidAP-tony

pr:
	hub pull-request -b lambdatree:master

check-env:
	uname -a
	lsb_release -a

prepare-nix:
	sudo mkdir /nix
	sudo chown travis: /nix
	sudo apt-get install -y libwww-curl-perl libdbd-sqlite3-perl

install-nix: check-env prepare-nix
	curl https://nixos.org/nix/install | sh
	sudo find / -type f -name "nix-env"
	# wget http://hydra.nixos.org/build/10272830/download/1/nix_1.7-1_amd64.deb -O /tmp/nix_1.7-1_amd64.deb
	# sudo dpkg -i /tmp/nix_1.7-1_amd64.deb
	# nix-channel --add http://nixos.org/channels/nixpkgs-unstable
	# nix-channel --update
	. $(HOME)/.profile
	[ -f $(HOME)/.nix-channel ] && cat $(HOME)/.nix-channel
	[ -f $(HOME)/.nix-defexpr ] && ls -l $(HOME)/.nix-defexpr
	[ -f $(HOME)/.nix-profile ] && ls -l $(HOME)/.nix-profile
	/nix/store/clnpynyac3hx3a6z5lsy893p7b4rwnyf-nix-1.7/bin/nix-env -qaP | grep nix-shell
	/nix/store/clnpynyac3hx3a6z5lsy893p7b4rwnyf-nix-1.7/bin/nix-env -i nix-shell

install: install-nix run-nix-shell

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

manual-release: build
	cp ./dist/build/hWifi/hWifi ~/.cabal/bin/
