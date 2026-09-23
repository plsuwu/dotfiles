{
  inputs,
  config,
  pkgs,
  lib,
  ...
}:
let
  cfg = config.systemModules.quickshell;
in
{
  options.systemModules.quickshell = {
    enable = lib.mkEnableOption "quickshell system UI";
    package = lib.mkOption {
      type = lib.types.package;
      default = inputs.quickshell.packages.${pkgs.stdenv.hostPlatform.system}.default;
    };
  };

  config = lib.mkIf cfg.enable {
    home.packages = [ cfg.package ];
  };
}
