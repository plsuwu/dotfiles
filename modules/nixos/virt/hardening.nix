{
  config,
  lib,
  pkgs,
  ...
}:
let
  cfg = config.modules.virt.hardened;
in
{
  options.modules.virt.hardened = {
    enable = lib.mkEnableOption "virtualization configuration: anti-anti-analysis";

    storageRoot = lib.mkOption {
      type = lib.types.path;
      default = /var/lib/libvirt/images;
      description = "root directory for storage pools";
    };
  };

  config = lib.mkIf cfg.enable {
    systemd.tmpfiles.rules = [
      "d ${cfg.storageRoot}            0711 root root -"
      "d ${cfg.storageRoot}/base       0711 root root -"
      "d ${cfg.storageRoot}/analysis   0711 root root -"
      "d ${cfg.storageRoot}/iso        0711 root root -"
    ];

    environment.systemPackages = with pkgs; [
      virt-viewer
      qemu-utils
      libguestfs
    ];
  };

}
