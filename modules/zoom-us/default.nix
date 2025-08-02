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

  config = lib.mkIf cfg.enable {
    xdg.mimeApps = {
      enable = true;
      defaultApplications = {
        "x-scheme-handler/zoommtg" = [ "Zoom.desktop" ];
        "x-scheme-handler/zoomus" = [ "Zoom.desktop" ];
      };
    };
  };
}
