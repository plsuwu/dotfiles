{ config, pkgs, ... }:

{
  imports = [
    ./etc.nix
    ./hardware-configuration.nix
    ./nvidia.nix
    ./boot.nix
  ];

  hardware.pulseaudio.enable = true;
}
