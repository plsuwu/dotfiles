{
  lib,
  pkgs,
  config,
  ...
}:
let
  cfg = config.systemModules.desktop;
in
{
  config = lib.mkIf (cfg.enable && pkgs.stdenv.isLinux) {
    qt = {
      enable = true;
      platformTheme.name = "gtk";
    };
  };
}
