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
    # home.packages = with pkgs; [
    #   papirus-folders
    #   papirus-icon-theme
    # ];
    
    gtk = {
      enable = true;
      cursorTheme = {
        name = "myramors";
        package = pkgs.myramors;
      };
      theme = {
        name = "Tokyonight-Dark";
        package = pkgs.tokyonight-gtk-theme;
      };

      # TODO find a nice icon package, perhaps...
      iconTheme = {
        name = "Adwaita-dark";
        package = pkgs.adwaita-icon-theme;
      };

      gtk4.theme = config.gtk.theme;
    };

    dconf = {
      settings = {
        "org/gnome/desktop/interface" = {
          color-scheme = "prefer-dark";
        };
      };
    };

    home.pointerCursor = {
      package = pkgs.myramors;
      name = "myramors";
      size = 32;
      gtk.enable = true;
    };
  };
}
