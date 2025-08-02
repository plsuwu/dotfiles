{ pkgs, ... }:
with pkgs;
[
  git
  wget
  curl
  # neovim
  vim
  wayland
  neofetch
  btop
  remmina
  zoom-us

  inter
  nerd-fonts.noto
  nerd-fonts.jetbrains-mono
]
