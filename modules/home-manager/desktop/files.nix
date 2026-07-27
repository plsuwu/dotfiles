{
  pkgs,
  config,
  lib,
  ...
}:
let
  cfg = config.systemModules.desktop;
in
{
  config = lib.mkIf (cfg.enable && pkgs.stdenv.isLinux) {
    xdg.desktopEntries.thunar = {
      name = "Thunar";
      exec = "${pkgs.thunar}/bin/thunar";
    };

    xdg.mimeApps = {
      enable = true;
      defaultApplications = {
        "inode/directory" = [ "thunar.desktop" ];
        "application/x-gnome-saved-search" = [ "thunar.desktop" ];
      };
    };

    # dconf = {
    #   settings = {
    #     "org/gnome/desktop/applications/terminal" = {
    #       exec = "alacritty";
    #       # exec-arg = ""; # argument
    #     };
    #   };
    # };
  };
}
