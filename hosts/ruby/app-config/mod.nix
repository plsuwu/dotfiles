{ config, lib, pkgs, inputs, ... }:
{

  nixpkgs.config = {
    allowUnfree = true;
  };

  environment.systemPackages = with pkgs; [
    neovim
    wget
    git
    gh
    curl
    fd
    ripgrep
    zsh
    oh-my-zsh
    alacritty
    tmux
    picom
    feh
    rofi
    maim
    eww
    unzip
    p7zip
    gcc
    rustup
    python3Full
    nodejs
    go
    php
    luajitPackages.luarocks
    dotnet-sdk
    dotnet-runtime
    mono
    maven
    julia
  ];

  fonts.packages = with pkgs; [
    (nerdfonts.override { fonts = [ "Ubuntu" "UbuntuMono" "Noto" "JetBrainsMono" ]; })
  ];
}
