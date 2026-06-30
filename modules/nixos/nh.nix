{
  config,
  lib,
  ...
}:
let
  cfg = config.systemModules.nh;
in
{
  options.systemModules.nh = {
    enable = lib.mkEnableOption "nh";
    flake = lib.mkOption {
      type = lib.types.str;
      default = "/home/please/src/dotfiles";
      description = "flake.nix used to build the configuration";
    };
  };

  config = lib.mkIf cfg.enable {
    programs.nh = {
      inherit (cfg) flake;

      enable = true;
      clean.enable = true;
      clean.extraArgs = "--keep-since 7d --keep 10";
    };
  };
}
