{
  lib,
  pkgs,
  ...
}:
{
  networking = {
    networkmanager.enable = lib.mkDefault true;
    firewall = {
      enable = lib.mkDefault true;
      logReversePathDrops = true;
      allowedTCPPorts = [ 5173 ];
    };
  };

  services.mullvad-vpn.enable = true;
  hardware.bluetooth.enable = false;
}
