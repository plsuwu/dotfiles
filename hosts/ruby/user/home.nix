{ config, pkgs, inputs, ... }:

{
  home.username = "please";
  home.homeDirectory = "/home/please";

  programs = {
    zsh = {
      enableAutosuggestions = true;
      enableCompletion = true;
    };
  };

  nixpkgs.config.allowUnfree = true;
  home.packages = with pkgs; [
    google-chrome
    discord
    bitwarden-cli
    bitwarden
  ];

  home.file = {
    # ??
  };

  home.sessionVariables = {
    EDITOR = "nvim";
  };

  home.stateVersion = "23.11";

  # Let Home Manager install and manage itself.
  programs.home-manager.enable = true;
}
