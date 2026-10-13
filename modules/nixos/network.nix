{
  lib,
  pkgs,
  ...
}:
let
  mkOVPNPath = name: "config /home/please/Documents/OVPN/${name}.ovpn";
in
{
  networking = {
    networkmanager = {
      enable = lib.mkDefault true;
      plugins = [
        pkgs.networkmanager-openvpn
      ];
    };

    firewall = {
      enable = lib.mkDefault true;
      logReversePathDrops = true;
      allowedTCPPorts = [
        5173
        4321
      ];
    };
  };

  networking.nftables.enable = true;

  services.dnsmasq = {
    enable = true;
    settings = {
      hostsdir = "/var/lib/htb-hosts";
      listen-address = "127.0.0.1";
      bind-interfaces = true;
      server = [ "1.1.1.1" ];
    };
  };

  networking.nameservers = [ "127.0.0.1" ];
  systemd.tmpfiles.rules = [ "d /var/lib/htb-hosts 0775 root wheel -" ];

  services.mullvad-vpn.enable = true;
  hardware.bluetooth.enable = false;

  services.openvpn.servers = {
    htbAU2 = {
      config = mkOVPNPath "machines_au-2";
      autoStart = false;
    };
    huntress2026 = {
      config = mkOVPNPath "huntress2026";
      autoStart = false;
    };
  };
}
