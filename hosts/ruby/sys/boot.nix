{ config, pkgs, lib, ... }:

{
  boot = {
    loader = {
      systemd-boot.enable = true;
      efi.canTouchEfiVariables = true;
    };

    # nvidia driver compile segfaults on default kernel, using latest for driver
    # and kernel instead.
    kernelPackages = pkgs.linuxPackages_latest;

    # sets f-key map to non-media keys during boot by rolling fnmode correction into initrd.
    extraModprobeConfig = ''
      options hid_apple fnmode=2
    '';
  };

}
