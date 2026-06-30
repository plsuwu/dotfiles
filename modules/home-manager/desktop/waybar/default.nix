{
  pkgs,
  lib,
  config,
  ...
}:
let
  cfg = config.systemModules.desktop;
  confDir = "${config.home.homeDirectory}/src/dotfiles/modules/home-manager/desktop/waybar/conf";
in
{
  config = lib.mkIf (cfg.enable && pkgs.stdenv.isLinux) {
    home.packages = with pkgs; [
      pipewire
      pulseaudio
      pwvucontrol
      crosspipe
    ];

    programs.waybar = {
      enable = true;
      systemd.enable = true;
      systemd.targets = [ "hyprland-session.target" ];
    };

    xdg.configFile."waybar/config.jsonc" = {
      source = config.lib.file.mkOutOfStoreSymlink "${confDir}/config.jsonc";
    };

    xdg.configFile."waybar/style.css" = {
      source = config.lib.file.mkOutOfStoreSymlink "${confDir}/style.css";
    };
  };
}
