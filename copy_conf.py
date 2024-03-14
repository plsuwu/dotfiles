#!/usr/bin/env python

import subprocess
import shlex
import os
import json

# typing & os module changes are not tested i did these on a whim!!!
HOME = os.getenv("HOME")
CONF_DIR = f"{HOME}/.config/"


def walk_dir(dir: str, omit: (list[str] | None) = None):
    walked = os.listdir(f"{CONF_DIR+dir}")
    app_name = dir.lstrip("/")

    if omit:
        for i in omit:
            if i in walked:
                walked.remove(i)

    for item in walked:
        target_items[app_name].append(f"{app_name}/{item}")


# "repository_dir" : ["path/to/local_item_one", "path/to/local_item_two"]
target_items: dict[str, list[str]] = {
    "alacritty": ["alacritty/alacritty.toml", "alacritty/themes/themes/tokyo-night.toml"],
    "eww": [],
    "feh": [],
    "gtk-3.0": ["gtk-3.0/settings.ini"],
    "neofetch": ["neofetch/config.conf"],
    "nvim": ["nvim/lua/", "nvim/init.lua"],
    "picom": ["picom/picom.conf"],
    "tmux": ["tmux/tmux.conf", "tmux/tokyonight.tmux"],
    "xmonad": [],
}

walk_dir("/eww", ["eww-git"])
walk_dir("/feh")
walk_dir("/xmonad", [".stack-work", "xmonad-contrib-git", "xmonad-git"]) # better/working hls support in neovim via hie.yaml & stack.yaml.

pp_target_items = json.dumps(target_items, indent=2)

print(f"The following items with be copied to this repository:\n{pp_target_items}")
confirm = input("Confirm and continue? [Y]es/[n]o: ")
if confirm.strip().lower() in ["", "y", "yes"]:
    for t, c in target_items.items():
        for i in c:
            cmd = shlex.split(f"cp -rt ./{t} {CONF_DIR}{i} ")
            subprocess.call(cmd)

else:
    print("Operation cancelled.")
    exit(1)
