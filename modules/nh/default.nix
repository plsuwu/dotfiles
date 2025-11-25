{
  lib,
  config,
  pkgs,
  ...
}:
let
  cfg = config.modules.nh;
in
{
  options.modules.nh = {
    enable = lib.mkEnableOption "nh";
  };

  config = lib.mkIf cfg.enable {
    programs.nh = {
      enable = true;
      clean.enable = true;
      clean.extraArgs = "--keep-since 7d --keep 10";
      flake = "/etc/nixos";
    };
  };
}
