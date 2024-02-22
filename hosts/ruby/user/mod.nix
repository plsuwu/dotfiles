{ config, lib, pkgs, inputs, ... }:
# unsure whether i like this inheriting stuff; maybe a bit too abstracted given idk what is
# even happening rn

{
  imports = [
    ./please.nix
    inputs.home-manager.nixosModules.default
  ];

  programs.zsh.enable = true;
  please.enable = true;
  please.userName = "please";

  home-manager = {

    # what does inheriting an input mean?
    # can i/how do i get common modules to flow into my machine-specific configurations?
    extraSpecialArgs = { inherit inputs; };
    users = {
      "please" = import ./home.nix;
    };
  };
}
