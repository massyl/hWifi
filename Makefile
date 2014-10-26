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
	./sandbox-run.sh run

test:
	./sandbox-run.sh "test --show-details=always"

clean-wifi:
	sudo nmcli con delete id $(WIFI_SSID)

build: setup
	./sandbox-run.sh build

manual-release: build
	cp ./dist/build/hWifi/hWifi ~/.cabal/bin/
