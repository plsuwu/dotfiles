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
  imports = [ ./inetsim ];

  options.systemModules.virt = {
    enable = lib.mkEnableOption "virt";
    inetsim = lib.mkOption {
      type = lib.types.bool;
      default = true;
    };
  };

  config = lib.mkIf cfg.enable {
    virtualisation.libvirtd = {
      enable = true;
      qemu = {
        package = pkgs.qemu_kvm;
        runAsRoot = true;
        swtpm.enable = true;
      };
    };

    environment = {
      systemPackages = [
        pkgs.virt-manager
        pkgs.dmidecode
        pkgs.inetsim
      ];
      etc = {
        "qemu/firmware".source = "${pkgs.qemu}/share/qemu/firmware";
      };
    };

    networking.firewall.interfaces."virbr0".allowedTCPPortRanges = [
      {
        from = 8000;
        to = 10000;
      }
    ];
  };
}
