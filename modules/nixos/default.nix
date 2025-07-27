{
  lib,
  config,
  inputs,
  ...
}:
{
  imports = [
    ./greetd.nix

    inputs.home-manager.nixosModules.home-manager
  ];

  home-manager.useGlobalPkgs = true;
  home-manager.useUserPackages = true;
}
