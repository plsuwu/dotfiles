# https://nixos.wiki/wiki/Creating_a_NixOS_live_CD
# https://nix.dev/tutorials/nixos/building-bootable-iso-image
#
# custom images can be built from a config using the nix package manager:
#         -> (on an arch system, for example - `sudo pacman -S nix`).
#
# ```
# nix-channel --add https://nixos.org/channels/nixpkgs-unstable nixpkgs
# nix-channel --update
# nix-build '<nixpkgs/nixos>' -A config.system.build.isoImage -I nixos-config=iso.nix
# dd if=result/iso/* of=/dev/sdX status=progress conv=sync
# ```

{ config, pkgs, lib, ... }:
{

  imports = [
    <nixpkgs/nixos/modules/installer/cd-dvd/installation-cd-minimal.nix>
    <nixpkgs/nixos/modules/installer/cd-dvd/channel.nix>
  ];

  environment.systemPackages = [ pkgs.neovim pkgs.git ];
  boot.kernelPackages = pkgs.linuxPackages_latest;
  boot.supportedFilesystems = lib.mkForce [ "btrfs" "reiserfs" "vfat" "f2fs" "xfs" "ntfs" "cifs" ];
}
