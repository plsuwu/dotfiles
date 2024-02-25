{ config, pkgs, inputs, outputs, ... }:

{
  imports = [
    outputs.modules.monitors
  ];

  gtk.enable = true;
  home = {
    username = "please";
    homeDirectory = "/home/please";
  };

  programs = {
    eww = {
        enable = true;
        configDir = ./eww;
    };
    zsh = {
      enable = true;
      enableCompletion = true;
      enableAutosuggestions = true;
      shellAliases = {
          ll = "ls -lh";
          la = "ls -lah";
          zi = "cdi";
      };
      oh-my-zsh = {
        enable = true;
        custom = "/home/please/.config/home-manager/home/please/zsh_custom";
        theme = "kphoen";
        plugins = [ "git" ];
      };
    };
    zoxide = {
      enable = true;
      enableZshIntegration = true;
      options = [ "--cmd cd" ];
    };
  };

  home.pointerCursor =
    let
      getFrom = url: hash: name: {
        gtk.enable = true;
        x11.enable = true;
        name = name;
        size = 32;
        package =
          pkgs.runCommand "moveUp" { } ''
            mkdir -p $out/share/icons
            ln -s ${pkgs.fetchzip {
              url = url;
              hash = hash;
            }} $out/share/icons/${name}
          '';
      };
    in
    getFrom
      "https://github.com/ful1e5/apple_cursor/releases/download/v2.0.0/macOS-Monterey.tar.gz"
      "sha256-MHmaZs56Q1NbjkecvfcG1zAW85BCZDn5kXmxqVzPc7M="
      "Monterey";


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

  systemd.user.startServices = "sd-switch";
  programs.home-manager.enable = true;
}
