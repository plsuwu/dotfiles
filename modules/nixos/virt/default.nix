{
  lib,
  config,
  pkgs,
  ...
}:
let
  cfg = config.systemModules.virt;
in
{
  imports = [
    ./inetsim
    ./lab
  ];

  options.systemModules.virt = {
    enable = lib.mkEnableOption "virt";
    inetsim = lib.mkOption {
      type = lib.types.bool;
      default = true;
    };
    lab = lib.mkOption {
      type = lib.types.bool;
      default = true;
    };
  };

  config = lib.mkIf cfg.enable {
    virtualisation.libvirtd = {
      enable = true;
      qemu = {
        package = pkgs.qemu_kvm;
        runAsRoot = false;
        swtpm.enable = true;
      };

      onBoot = "ignore";
      onShutdown = "shutdown";
    };

    services.malwareLab = lib.mkIf cfg.lab {
      enable = true;
      agentUser = "analyst-agent";
      ingestUsers = [ "please" ];
      guestUser = "Peas";
    };

    users.users.analyst-agent = {
      isNormalUser = true;
      description = "automated malware analyst agent";
    };

    environment = {
      systemPackages = [
        pkgs.virt-manager
        pkgs.dmidecode
        pkgs.inetsim
      ];
      etc."qemu/firmware".source =
        "${config.virtualisation.libvirtd.qemu.package}/share/qemu/firmware";
    };

    networking.firewall.interfaces."virbr0".allowedTCPPortRanges = [
      {
        from = 8000;
        to = 10000;
      }
    ];
  };
}
