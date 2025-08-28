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
      flake = "/etc/nixos";
    };
  };
}
