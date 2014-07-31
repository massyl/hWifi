pr:
	hub pull-request -b lambdatree:master

install:
	uname -a
	sudo apt-get install -y haskell-platform
	cabal --version
	cabal update &&	cabal install cabal-install

deps:
	ghc --version
	cabal --version
	cabal install base process mtl QuickCheck HUnit

sandbox-init:
	cabal sandbox init

sandbox-delete:
	cabal sandbox delete

cabal-init:
	cabal init

run:
	cabal run

test:
	cabal test

build:
	cabal configure --enable-tests
	cabal build

run-nix-shell:
	nix-shell --pure hwifi.nix
