{
  pkgs,
  lib,
  config,
  inputs,
  ...
}:
let
  cfg = config.systemModules.desktop;
in
{
  config = lib.mkIf (cfg.enable && pkgs.stdenv.isLinux) {
    xdg.portal = {
      enable = true;
      configPackages = with pkgs; [
        xdg-desktop-portal-wlr
        xdg-desktop-portal-gtk
        xdg-desktop-portal-hyprland
      ];

      extraPortals = with pkgs; [
        xdg-desktop-portal-wlr
        xdg-desktop-portal-gtk
        xdg-desktop-portal-hyprland
      ];

      config.common.default = [ "gtk" ];
    };

    xdg.autostart.enable = true;

    xdg.userDirs = {
      enable = true;
      setSessionVariables = true;
      pictures = "${config.home.homeDirectory}/Pictures";
    };
  };
}
