#!/usr/bin/env python

import subprocess
import shlex
import os

# typing & os module changes are not tested i did these on a whim!!!
HOME=os.getenv("HOME")
CONF_DIR=f"{HOME}/.config/"

# "repository_dir" : ["path/to/local_item_one", "path/to/local_item_two"]
target_items: dict[str,list[str]] = {
        "alacritty": [
            "alacritty/alacritty.toml"
        ],
        
        # i can't remember how to insert items into a nested dict list; 
        # but this seems like it would work, right?.
        
        # instead of globbing, we could instead use something like:
        # ```
        # ... = { 
        #         "eww": [],
        # ...
        # }
        # 
        # eww_dir: list[str] = os.listdir(f"{CONF_DIR}/eww")
        # for i in eww_dir:
        #        target_items["eww"].append(f"{CONF_DIR}/{i}")
        # ```
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

