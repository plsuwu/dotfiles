{ config, pkgs, ... }:

{
  home = {
    username = "please";
    homeDirectory = "/home/please";
  };

  programs = {
    zsh = {
      enable = true;
      enableCompletion = true;
      enableAutosuggestions = true;
      oh-my-zsh = {
        enable = true;
        custom = "./zsh_custom/";
        theme = "kphoen";
        plugins = ["git"];
      };
    };
    zoxide = {
      enable = true;
      enableZshIntegration = true;
    };
  };

  home.stateVersion = "24.05";
  home.packages = [
    pkgs.vesktop
    pkgs.zsh
  ];

  home.file = {

    # # Building this configuration will create a copy of 'dotfiles/screenrc' in
    # # the Nix store. Activating the configuration will then make '~/.screenrc' a
    # # symlink to the Nix store copy.
    # ".screenrc".source = dotfiles/screenrc;

    # # You can also set the file content immediately.
    # ".gradle/gradle.properties".text = ''
    #   org.gradle.console=verbose
    #   org.gradle.daemon.idletimeout=3600000
    # '';
  };

  home.sessionVariables = {
    EDITOR = "nvim";
  };

  programs.home-manager.enable = true;
}
