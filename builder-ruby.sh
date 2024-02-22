#!/usr/bin/env bash

# NOTE: not going to work for darwin-rebuild.

# using lua configs via lazy/mason makes vim configuration much easier and i simply prefer it.
# so, because i CANNOT figure out how to get neovim and nixos to use my .lua files:

CONFDIR="/home/please/.config"
ARGS="$*"

function initialize () {

    printf "initializing flake...\n"

    [ ! -d $CONFDIR ] && mkdir -p $CONFDIR

    cp -r ./hosts/ruby/app-config/nvim/ /home/please/.config/
    cp -r ./* /home/please/.config/nix/

}

case "${ARGS}"
    in
    "-i" | "--init") initialize
    ;;
esac

printf "rebuilding from flake 'nix#ruby'...\n"
sudo nixos-rebuild switch --flake /home/please/.config/nix#ruby
