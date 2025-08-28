{ config, lib, ... }:
let
  cfg = config.modules.net;
in
{
  options.modules.net = {
    enable = lib.mkEnableOption "net";
    enableBluetooth = lib.mkOption {
      default = false;
    };
  };

  config = {
    networking.networkmanager.enable = true;
    hardware.bluetooth.enable = cfg.enableBluetooth;

    services.resolved.enable = true;
    services.mullvad-vpn.enable = true;

    networking.firewall = {
      enable = true;
      logReversePathDrops = true;
    };
  };
}
