{
  config,
  pkgs,
  lib,
  ...
}:
let
  myracursor = pkgs.callPackage ./myracursor { }; # see: `myracursor/default.nix`
  cfg = config.modules.myracursor;
in
{

  options.modules.myracursor = {
    enable = lib.mkEnableOption "myracursor";
  };

  config = lib.mkIf cfg.enable {
    dconf = {
      settings = {
        "org/gnome/desktop/interface" = {
          cursor-theme = "myracursor";
        };
      };
    };

    home.pointerCursor = {
      gtk.enable = true;
      name = "myracursor";
      package = myracursor;
    };
    gtk = {
      cursorTheme = {
        name = "myracursor";
        package = myracursor;
      };
    };

    # home.file.".icons/myracursor" = {
    #   recursive = true;
    #   source = ./myracursor;
    # };
  };
}
