{
  config,
  lib,
  ...
}:
let
  cfg = config.systemModules.nix-ld;
in
{
  options.systemModules.nix-ld = {
    enable = lib.mkEnableOption "nix-ld";
  };

  config = lib.mkIf cfg.enable {
    programs.nix-ld.enable = true;
  };
}



