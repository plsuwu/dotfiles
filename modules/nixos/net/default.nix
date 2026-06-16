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
    hardware.bluetooth.enable = false;

    # services.resolved.enable = true;
    services.mullvad-vpn.enable = true;

    networking.firewall = {
      enable = true;
      allowedTCPPorts = [ 5173 ];
      logReversePathDrops = true;
      extraCommands = ''
        iptables -A nixos-fw -p tcp -s 172.16.0.0/12 --dport 11434 -j nixos-fw-accept
      '';
    };

    # services.cloudflared = {
    #   enable = true;
    #   tunnels = {
    #     "00000000-0000-0000-0000-000000000000" = {
    #       credentialsFile = "${config.sops.secrets.cloudflared-creds.path}";
    #       default = "http_status:404";
    #     };
    #   };
    # };

    networking.hosts =
      let
        logins = [
          "www"
          "hempie"
          "womfyy"
          "chikogaki"
          "b0barley"
          "kyoharuvt"
          "misspeggyx"
          "parasi"
          "vacu0usly"
          "snoozy"
          "sleepiebug"
          "chocojax"
          "meiya"
          "batatvideogames"
          "noi_vt"
          "kyundere"
          "kumomomomomomomo"
          "niupao"
          "unipiu"
          "souly_ch"
          "liljuju"
          "nanolather"
          "lcolonq"
          "myramors"
          "gibbbons"
          "harupi"
          "miaelou"
          "krumroll"
          "dearpekoe"
          "saltae"
          "substituber"
          "vixi"
          "limealicious"
          "kokopimento"
          "myrmidonvt"
          "byebi"
          "rena_chuu"
          "madmad01"
          "miffygeist"
          "walfas"
          "baikenvt"
          "bexvalentine"
          "netuserhael"
          "milia"
          "pachi"
          "flippersphd"
          "bibibiscuitch"
          "herakita"
          "plss"
          "kkcyber"
          "tini"
          "aaallycat"
          "odessavt"
          "tear"
        ];

        domains = map (s: "${s}.piss.local") logins;
      in
      {
        "127.0.0.1" = [
          "piss.local"
        ]
        ++ domains;


        "10.129.245.100" = [ "connected.htb" "pbxconnect.htb" ];
      };
  };
}
