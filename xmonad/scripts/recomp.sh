#!/bin/bash
set -euox pipefail

mkdir -p "/home/please/.config/xmonad/logs"
TIMESTAMP=$(date +'%Y_%d_%m-%H%M.%S-%2N')
LOGFILE="/home/please/.config/xmonad/logs/xmonad-recomp_$TIMESTAMP.log"


XMONAD_BASE="/home/please/.config/xmonad"
XMONAD_MAIN="$XMONAD_BASE/xmonad-git"
XMONAD_CONTRIB="$XMONAD_BASE/xmonad-contrib-git"

printf '[*] Updating XMonad/Contrib repos:\n' | tee "$LOGFILE"
cd "$XMONAD_MAIN" || exit 1 | tee "$LOGFILE"
git pull | tee "$LOGFILE"

cd "$XMONAD_CONTRIB" || exit 1
git pull

cd "$XMONAD_BASE" || exit 1
stack install

