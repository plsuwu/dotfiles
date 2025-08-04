{
  pkgs,
  lib,
  ...
}:
with pkgs;
[
  git
  wget
  curl
  pavucontrol
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
