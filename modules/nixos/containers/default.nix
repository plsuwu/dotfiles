{
  lib,
  config,
  pkgs,
  ...
}:
let
  cfg = config.modules.containers;
in
{
  options.modules.containers = {
    enable = lib.mkEnableOption "containers";
    compose = lib.mkOption {
      default = false;
    };
    dockerCompat = lib.mkOption {
      default = false;
    };
  };

  config = lib.mkIf cfg.enable {
    virtualisation.podman = {
      inherit (cfg) dockerCompat;

      enable = true;
      dockerSocket.enable = cfg.dockerCompat;

      extraPackages = lib.optional cfg.compose [
        pkgs.podman-compose
      ];
      autoPrune = {
        enable = true;
        dates = "weekly";
      };
    };

    virtualisation.containers = {
      enable = true;
      containersConf.cniPlugins = lib.optional cfg.compose [
        pkgs.cniPlugins
        pkgs.dnsname-cni
      ];
    };
  };
}
