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
    programs.thunar.plugins = with pkgs.xfce; [
      thunar-archive-plugin
    ];

    environment.systemPackages = with pkgs; [
      ffmpegthumbnailer
      webp-pixbuf-loader
      freetype
    ];

    programs.xfconf.enable = true;
    services.gvfs.enable = true;
    services.tumbler.enable = true;
  };
}
