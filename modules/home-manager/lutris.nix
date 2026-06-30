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
    programs.lutris = {
      enable = true;
      steamPackage = pkgs.steam;
      defaultWinePackage = pkgs.proton-ge-bin;
      protonPackages = [
        pkgs.proton-ge-bin
      ];
    };
  };
}
