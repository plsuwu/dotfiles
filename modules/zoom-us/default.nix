{
  config,
  lib,
  pkgs,
  ...
}:
let
  cfg = config.modules.zoom-us;
in
{
  options.modules.zoom-us = {
    enable = lib.mkEnableOption "zoom-us";
  };

  # config = lib.mkIf cfg.enable {
  # };
}
