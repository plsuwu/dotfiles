{
  pkgs,
  lib,
  config,
  ...
}:
let
  cfg = config.systemModules.desktop;
in
{
  config = lib.mkIf (cfg.enable && pkgs.stdenv.isLinux) {
    home.packages = with pkgs; [
      openmoji-color
      font-awesome_5
      iosevka
      inter
      aporetic
      nerd-fonts.iosevka
      nerd-fonts.symbols-only

      # fix broken chinese/japanese/.. font rendering in chromium derivatives
      noto-fonts-cjk-serif
      noto-fonts-cjk-sans
    ];

    fonts.fontconfig = {
      enable = true;
      defaultFonts.emoji = [ "OpenMoji Color" ];
    };
  };
}
