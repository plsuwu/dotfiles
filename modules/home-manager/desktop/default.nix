{
  config,
  pkgs,
  lib,
  ...
}:
{
  imports = [
    ./awww
    ./hyprland
    ./quickshell
    ./swayimg
    ./waybar

    ./files.nix
    ./fonts.nix
    ./gtk.nix
    ./polkit.nix
    ./qt.nix
    ./screenshot.nix
    ./swaync.nix
    ./xdg.nix
  ];

  options.systemModules.desktop = {
    enable = lib.mkEnableOption "desktop";
  };

  config = lib.mkIf config.systemModules.desktop.enable {
    home.packages = with pkgs; [
      spotify
      mpv

      aseprite
    ];

    services.remmina.enable = true;
  };
}
