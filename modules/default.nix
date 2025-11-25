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
    ./nh
  ];

  home.packages =
    (import ../pkg {
      inherit pkgs lib;
    })
    ++ (import (../pkg + "/${pkgs.stdenv.hostPlatform.system}.nix") {
      inherit pkgs lib;
    })
    ++ (import ../pkg/steam.nix {
      inherit pkgs lib;
    });
}

