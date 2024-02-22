# any `mod.nix` files should actually be `default.nix` to meet home-manager expectations
# lol

{ config, lib, pkgs, inputs, ... }:

{
  imports =
    [
      ./sys/mod.nix
      ./user/mod.nix
      ./app-config/mod.nix
      inputs.home-manager.nixosModules.default
    ];

  nix.settings.experimental-features = [ "nix-command" "flakes" ];
  system.stateVersion = "23.11";
}
