{
  pkgs,
  config,
  lib,
  ...
}:
let
  cfg = config.modules.media-utils;
in
{
  options.modules.media-utils = {
    enable = lib.mkEnableOption "media-utils";
  };

  config = lib.mkIf cfg.enable {
    programs = {
      feh.enable = true;
      mpv = {
        enable = true;
      };
    };
  };
}
