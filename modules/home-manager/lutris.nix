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
    # environment.systemPackages = with pkgs; [
    #   protonup-qt
    # ];
    programs.lutris = {
      enable = true;
      steamPackage = pkgs.steam;
      defaultWinePackage = pkgs.proton-ge-bin;
      extraPackages = with pkgs; [
        mangohud
        winetricks
        gamescope
        gamemode
        umu-launcher
      ]; 
      protonPackages = [
        pkgs.proton-ge-bin
      ];

      winePackages = [
        pkgs.winePackages.full
      ];
    };
  };
}
