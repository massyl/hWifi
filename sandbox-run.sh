#!/bin/bash -x

. $HOME/.profile

nix-shell --pure hwifi.nix --command "cabal $*"
