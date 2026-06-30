{
  lib,
  config,
  pkgs,
  ...
}:
let
  cfg = config.systemModules.desktop;
in
{
  config = lib.mkIf cfg.enable {
    services.awww = {
      enable = true;
    };

    xdg.configFile."wallpaper.jpg".source =
      config.lib.file.mkOutOfStoreSymlink "${config.home.homeDirectory}/src/dotfiles/modules/home-manager/desktop/awww/default-img.jpg";
  };
}
