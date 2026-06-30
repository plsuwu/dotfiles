{
  lib,
  config,
  ...
}:
let
  cfg = config.systemModules.sshd;
in
{
  options.systemModules.sshd = {
    enable = lib.mkEnableOption "sshd";
    address = lib.mkOption {
      type = lib.types.str;
      description = "sshd bind listener address";
      default = "0.0.0.0";
    };

    port = lib.mkOption {
      type = lib.types.port;
      default = 22;
    };
  };

  config = lib.mkIf cfg.enable {
    services.openssh = {
      enable = true;
      ports = [ cfg.port ];

      startWhenNeeded = true;
      openFirewall = true;

      listenAddresses = [
        {
          addr = cfg.address;
          port = cfg.port;
        }
      ];

      settings = {
        UsePAM = false;
        PermitRootLogin = "no";
        PasswordAuthentication = false;
        KbdInteractiveAuthentication = false;
      };
    };
  };
}
