{
  lib,
  config,
  pkgs,
  user,
  inputs,
  ...
}:
let
  cfg = config.modules.thunar;
in
{
  options.modules.thunar = {
    enable = lib.mkEnableOption "thunar";
  };

  config = lib.mkIf cfg.enable {
    programs.thunar.enable = true;

    environment.systemPackages = with pkgs; [
      thunar-archive-plugin
      ffmpegthumbnailer
      webp-pixbuf-loader
      freetype
    ];

    programs.xfconf.enable = true;
    services.gvfs.enable = true;
    services.tumbler.enable = true;
  };
}
