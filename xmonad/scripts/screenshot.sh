#!/usr/bin/env bash

path="$HOME/Pictures/Screenshots"
name=$(date "+%Y%m%d_%H%M%S.png")

case "$1" in
    "-s" )
        maim -m 10 -su "$path/$name" && feh "$path/$name"
        ;;
    "-sc" )
        maim -su | xclip -selection clipboard -t image/png -i
        ;;
    "-f" )
        maim -m 10 -u "$path/$name"; feh "$path/$name"
        ;;
esac
