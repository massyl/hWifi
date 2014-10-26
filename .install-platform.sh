#!/bin/bash -x

sudo mkdir /nix
sudo chown travis: /nix
# sudo apt-get install -y libwww-curl-perl libdbd-sqlite3-perl

bash <(curl https://nixos.org/nix/install)

# sudo find / -type f -name "nix-env"
# sudo find / -type f -name "nix-channel"
# wget http://hydra.nixos.org/build/10272830/download/1/nix_1.7-1_amd64.deb -O /tmp/nix_1.7-1_amd64.deb
# sudo dpkg -i /tmp/nix_1.7-1_amd64.deb

echo "if [ -e $HOME/.nix-profile/etc/profile.d/nix.sh ]; then . $HOME/.nix-profile/etc/profile.d/nix.sh; fi" >> .profile
echo "export PATH=$HOME/.nix-profile/bin:$PATH" >> .profile

. $HOME/.profile

# nix-channel --add http://nixos.org/channels/nixpkgs-unstable
# nix-channel --update

# ls -l $HOME/.nix-defexpr/channels/
# pwd
# ls -la $HOME

# [ -f $HOME/.nix-channels ] && cat $HOME/.nix-channels
# [ -d $HOME/.nix-profile ] && ls -l $HOME/.nix-profile
# [ -d $HOME/.nix-defexpr ] && ls -l $HOME/.nix-defexpr/channels

# $(NIX_ENV) -qaP | grep nix-shell
# $(NIX_ENV) -i nix-shell

# echo $(NIX_PATH)
# export NIX_PATH="nixpkgs=$(HOME)/.nix-defexpr/channels/nixpkgs:$(NIX_PATH)"
# echo $(NIX_PATH)

env

ls -l ~/.nix-profile/bin/
