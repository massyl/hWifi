# Default ssid to use
WIFI_SSID=AndroidAP-tony
# hwifi deployed by stack
PROG=.stack-work/install/x86_64-linux/lts-3.4/7.10.2/bin/hWifi

pr:
	hub pull-request -b lambdatree:master

check-env:
	uname -a
	lsb_release -a

clean-wifi:
	sudo nmcli con delete id $(WIFI_SSID)

install:
	stack install

init:
	stack init

setup:
	stack setup

build:
	stack build

test:
	stack test

run:
	stack exec $(PROG)

.PHONY: test
