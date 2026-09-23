{
  config,
  lib,
  pkgs,
  ...
}:
let
  cfg = config.systemModules.lutris;
in
{
  options.systemModules.lutris = {
    enable = lib.mkEnableOption "lutris";
  };

  config = lib.mkIf cfg.enable {
    home.packages = with pkgs; [
      umu-launcher
    ];
    programs.lutris = {
      enable = true;
      steamPackage = pkgs.steam;

      protonPackages = [ pkgs.proton-ge-bin ];
      winePackages = [
        pkgs.wine64Packages.full
        pkgs.wine64Packages.waylandFull
      ];
      defaultWinePackage = pkgs.proton-ge-bin;
      extraPackages = with pkgs; [
        mangohud
        winetricks
        protontricks
        gamescope
        gamemode
        umu-launcher
      ];

      runners.wine.settings = {
        runner = {
          esync = true;
          fsync = true;
        };
        # system = {};
      };
    };
    xdg.desktopEntries."net.lutris.Lutris" = {
      name = "Lutris";
      exec = "env WINEPREFIX=/data/shared/__gamedata/.main lutris %U";
      icon = "net.lutris.Lutris";
      categories = [ "Game" ];
      mimeType = [ "x-scheme-handler/lutris" ];
    };
  };
}
