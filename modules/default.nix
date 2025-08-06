{
  inputs,
  pkgs,
  lib,
  system,
  ...
}:
{
  home.stateVersion = "25.11";

  imports = [
    ./discord
    ./hypr
    ./nvim
    ./obs
    ./pipewire
    ./term
    ./waybar
    ./zoom-us
    ./zsh
    ./media-utils
  ];

  home.packages =
    (import ../pkg {
      inherit pkgs lib;
    })
    ++ (import (../pkg + "/${pkgs.system}.nix") {
      inherit pkgs lib;
    });
}
