{
  config,
  lib,
  pkgs,
  modulesPath,
  ...
}:
{
  imports = [
    (modulesPath + "/installer/scan/not-detected.nix")
  ];

  boot.initrd.availableKernelModules = [
    "vmd"
    "xhci_pci"
    "ahci"
    "nvme"
    "usbhid"
    "usb_storage"
    "sd_mod"
  ];

  boot.kernelModules = [ "kvm-intel" ];
  boot.initrd.kernelModules = [ ];
  boot.extraModulePackages = [ ];

  swapDevices = [
    { device = "/dev/disk/by-uuid/7517fe9c-ff28-445c-8223-4b301e1a4e39"; }
  ];

  fileSystems."/boot" = {
    device = "/dev/disk/by-uuid/C5D5-F030";
    fsType = "vfat";
    options = [
      "fmask=0022"
      "dmask=0022"
    ];
  };

  fileSystems."/" = {
    device = "/dev/disk/by-uuid/ab127bbd-c18f-421c-8aa5-c6b9a52094c3";
    fsType = "ext4";
  };

  fileSystems."/data" = {
    device = "/dev/disk/by-uuid/c267f99b-8e9e-4e0f-bc2c-bfc735ab3d31";
    fsType = "ext4";
  };

  # fileSystems."/data/shared" = {
  #   device = "/dev/disk/by-uuid/EE37-6447";
  #   fsType = "exfat";
  #   depends = [ "/data" ];
  #   options = [
  #     "uid=1000"
  #     "gid=1000"
  #   ];
  # };

  boot.supportedFilesystems = [ "ntfs" ];
  fileSystems."/data/shared" = {
    device = "/dev/disk/by-uuid/4C03820E768D9B4B";
    fsType = "ntfs3";
    options = [
      "rw"
      "uid=1000"
      "gid=100"
      "umask=022"
      "nofail"
    ];
  };
}
