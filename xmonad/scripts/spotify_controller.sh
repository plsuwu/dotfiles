#!/usr/bin/env bash

get_spotify() {
    playerctl -p spotify_player --follow metadata --format '{{lc(status)}} > {{title}} | {{artist}} > {{duration(position)}}|{{duration(mpris:length)}}'
}

case "$1" in
    "-s" )
        get_spotify
        ;;
esac
