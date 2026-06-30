{
  lib,
  config,
  ...
}:
let
  cfg = config.systemModules.docker;
in
{
  options.systemModules.docker = {
    enable = lib.mkEnableOption "docker";
  };

  config = lib.mkIf cfg.enable {
    virtualisation.docker = {
      enable = true;
      enableOnBoot = false;
      rootless.enable = true;
      autoPrune.enable = true;
    };
  };
}
