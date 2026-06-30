{
  lib,
  config,
  pkgs,
  ...
}:
let
  cfg = config.systemModules.display;
in
{
  options.systemModules.display = {
    enable = lib.mkEnableOption "display";
  };

  config = lib.mkIf cfg.enable {
    services.gnome.gnome-keyring.enable = true;
    services.dbus.packages = [ pkgs.gcr ];

    hardware = {
      graphics = {
        enable = true;
      };

      nvidia = {
        open = true;
        package = config.boot.kernelPackages.nvidiaPackages.latest;

        nvidiaSettings = true;
        modesetting.enable = true;
      };
    };

    services.xserver.videoDrivers = [ "nvidia" ];
    environment.sessionVariables.NIXOS_OZONE_WL = "1";
  };
}
