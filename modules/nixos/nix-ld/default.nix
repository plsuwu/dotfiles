{
  pkgs,
  lib,
  config,
  ...
}:
let
  cfg = config.modules.nix-ld;
in
{
  options.modules.nix-ld = {
    enable = lib.mkEnableOption "nix-ld";
  };

  config = lib.mkIf cfg.enable {
    programs.nix-ld.enable = true;
  };
}
