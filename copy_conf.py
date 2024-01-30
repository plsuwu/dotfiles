#!/usr/bin/env python

import subprocess
import shlex

CONF_DIR="/home/please/.config/"

# "repository_dir" : "local_items"
target_items = {
        "alacritty": [
            "alacritty/alacritty.toml"
            ],

        # idk how to do globbing in subprocess but its
        # not worth googling rn ("eww": "eww/*")
        "eww": [
            "eww/palette/",
            "eww/scripts/",
            "eww/components.yuck",
            "eww/eww.scss",
            "eww/eww.yuck",
            "eww/tests.yuck",
            "eww/variables.yuck"
            ],
        "feh": [
            "feh/main.png"
            ],
        "gtk-3.0": [
            "gtk-3.0/settings.ini"
            ],
        "neofetch": [
            "neofetch/config.conf"
            ],
        "nvim": [
            "nvim/lua/",
            "nvim/init.lua"
        ],
        "picom": [
            "picom/picom.conf"
            ],
        "tmux": [
            "tmux/tmux.conf"
            ],
        "xmonad": [
            "xmonad/scripts/",
            "xmonad/xmonad.hs"
        ]
    }

for t, c in target_items.items():
    for i in c:
        cmd = shlex.split(f"cp -rt ./{t} {CONF_DIR}{i} ")
        subprocess.call(cmd)

