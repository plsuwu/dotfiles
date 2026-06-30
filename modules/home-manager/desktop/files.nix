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
    home.packages = with pkgs; [
      (nemo-with-extensions.override {
        extensions = with pkgs; [
          nemo-fileroller
          nemo-emblems
          nemo-preview
        ];
      })
    ];

    xdg.desktopEntries.nemo = {
      name = "Nemo";
      exec = "${pkgs.nemo-with-extensions}/bin/nemo";
    };
    xdg.mimeApps = {
      enable = true;
      defaultApplications = {
        "inode/directory" = [ "nemo.desktop" ];
        "application/x-gnome-saved-search" = [ "nemo.desktop" ];
      };
    };

    dconf = {
      settings = {
        "org/cinnamon/desktop/applications/terminal" = {
          exec = "alacritty";
          # exec-arg = ""; # argument
        };
      };
    };
  };
}
