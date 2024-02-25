#!/usr/bin/env bash

CONFDIR="/home/$USER/.config/home-manager"
SYSFLAKE="$CONFDIR#$HOSTNAME"
HOMEFLAKE="$CONFDIR#please@$HOSTNAME"

function all () {
    printf "\nrebuilding system and home-manager from flakes: \n\n"
    sudo nixos-rebuild switch --flake $SYSFLAKE
    home-manager switch --flake $HOMEFLAKE
    printf "\ndone.\n"
}

function home () {
    printf "\nrebuilding home-manager from flake: \n\n"
    home-manager switch --flake $HOMEFLAKE
    printf "\ndone.\n"
}

function system () {
    printf "\nrebuilding system from flake: \n\n"
    sudo nixos-rebuild switch --flake $SYSFLAKE
    printf "\ndone.\n"
}

case $@ in
    '-h'|'--home') home;;
    '-s'|'--system') system;;
    *) all;;
esac
