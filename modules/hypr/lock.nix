{
  pkgs,
  lib,
  config,
  ...
}:
let
  cfg = config.modules.lock;
in
{
  # config = {
  #   programs.hyprlock = {
  #     enable = true;
  #
  #   };
  # };
}
