{
  lib,
  config,
  pkgs,
  inputs,
  user,
  ...
}:
let
  cfg = config.modules.virt;
in
{
  options.modules.virt = {
    enable = lib.mkEnableOption "virt";
  };

  config = lib.mkIf cfg.enable {
    virtualisation.libvirtd = {
      enable = true;
      qemu = {
        package = pkgs.qemu_kvm;
        runAsRoot = true;
        swtpm.enable = true;

        ovmf = {
          enable = true;
          packages = [ pkgs.OVMFFull.fd ];
        };
      };
    };

    environment.etc = {
      # "libvirt/hooks/qemu".source = "${inputs.vfio-hooks}/libvirt_hooks/qemu";
      "qemu/firmware".source = "${pkgs.qemu}/share/qemu/firmware";
    };

    programs.virt-manager.enable = true;
    environment.systemPackages = [ pkgs.gnome-boxes ];
    networking.firewall.interfaces."virbr0".allowedTCPPortRanges = [
      {
        from = 8000;
        to = 10000;
      }
    ];
  };
}
