#!/usr/bin/env bash

for window in "$(wmctrl -l | awk '{ print $1 }')"; do
    wmctrl -i -c "$window";
    sleep 0.2;
done
pkill xmonad
