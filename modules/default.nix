{
  inputs,
  pkgs,
  lib,
  config,
  ...
}:
{
  home.stateVersion = "25.11";

  imports = [
    ./discord
    ./hypr
    ./nvim
    ./pipewire
    ./term
    ./zoom-us
    ./zsh
  ];

  home.packages =
    (import ../pkg {
      inherit pkgs;
      inherit lib;
    })
    ++ (import (../pkg + "/${pkgs.system}.nix") {
      inherit pkgs;
    });

  # programs.direnv = {
  #   enable = true;
  #   enableZshIntegration = true;
  #   nix-direnv.enable = true;
  #
  #   config = {
  #     global.hide_env_diff = true;
  #   };
  # };
}
