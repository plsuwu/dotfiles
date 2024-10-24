#!/usr/bin/env bash

box='(box :class "workspaces" :orientation "h" :halign "center" :spacing 5'

xprop -notype -spy -root 8t _XMONAD_LOG | stdbuf -o0 cut -d'=' -f 2 | stdbuf -o0 sed -u -e "s/^ \"/$box/" -e 's/"$/)/'
