#!/usr/bin/env bash

mkdir -p "/home/pls/.config/xmonad/logs"
TIMESTAMP=$(date +'%Y_%d_%m-%H%M.%S-%2N')
LOGFILE="/home/pls/.config/xmonad/logs/xmonad-recomp_$TIMESTAMP.log"

echo '[*] Recompiling XMonad & updating binary at /usr/local/bin/xmonad...' | tee "$LOGFILE"

if xmonad --recompile 2>&1 | tee -a "$LOGFILE"; then
    echo '[*] Compile done (0):' | tee -a "$LOGFILE" # uhioknji
    sudo cp /home/pls/.local/bin/xmonad /usr/local/bin/xmonad
    xmonad --restart
    notify-send "XMonad recompiler" "XMonad recompile job complete - any errors logged to $LOGFILE ."
else
    echo "[!] Compile failed." | tee -a "$LOGFILE"
    notify-send "XMonad recompiler" "Recompile failed. Logged to $LOGFILE."
fi
