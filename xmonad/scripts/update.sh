#!/bin/bash

set -euox pipefail
XMONAD_BASE="/home/please/.config/xmonad"
GIT_REPO_DIRS=( "xmonad-git" "xmonad-contrib-git" )

for DIR in "${GIT_REPO_DIRS[@]}"; do
    cd "$XMONAD_BASE/$DIR" || exit 1
    /usr/bin/git pull
done

cd "$XMONAD_BASE" || exit 1
/home/please/.ghcup/bin/stack install
/home/please/.local/bin/xmonad --recompile
/home/please/.local/bin/xmonad --restart
