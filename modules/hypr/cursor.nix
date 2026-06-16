{
  config,
  pkgs,
  lib,
  ...
}:
let
  myramors = pkgs.callPackage ./myracursor { }; # see: `myracursor/default.nix`
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
          cursor-theme = "myramors";
        };
      };
    };

    home.pointerCursor = {
      gtk.enable = true;
      name = "myramors";
      package = myramors;
    };
    gtk = {
      cursorTheme = {
        name = "myramors";
        package = myramors;
      };
    };

    home.file.".icons/myracursor" = {
      recursive = true;
      source = ./myracursor;
    };
  };
}
