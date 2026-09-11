{
  config,
  lib,
  pkgs,
  ...
}:
let
  cfg = config.systemModules.boot;
in
{
  options.systemModules.boot = {
    enable = lib.mkEnableOption "boot";
  };

  config = lib.mkIf cfg.enable {
    # networking.enableIPv6 = false;
    # boot.kernel.sysctl."net.ipv6.conf.wlo1.disable_ipv6" = true;

    boot = {

      loader = {
        efi.canTouchEfiVariables = true;
        systemd-boot = {
          enable = true;
          configurationLimit = 5;
          consoleMode = "auto";
        };
      };

      kernelPackages = pkgs.linuxPackages_latest;
      extraModprobeConfig = ''
        options hid_apple fnmode=2
      '';
    };
  };
}
