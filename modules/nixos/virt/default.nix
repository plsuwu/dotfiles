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
  options.systemModules.virt = {
    enable = lib.mkEnableOption "virt";
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
      systemPackages = [ pkgs.virt-manager ];
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
