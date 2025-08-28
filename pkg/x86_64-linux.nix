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

  vim
  wayland
  neofetch
  btop
  remmina
  zoom-us
  usbutils

  inter
  nerd-fonts.noto
  nerd-fonts.jetbrains-mono
]
