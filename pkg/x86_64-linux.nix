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

  iw
  pavucontrol

  vim
  wayland
  neofetch
  btop
  remmina
  zoom-us
  usbutils

  nix-index
  morph

  davinci-resolve

  ffmpeg-full
  cudatoolkit
  file-roller

  inter
  nerd-fonts.noto
  nerd-fonts.jetbrains-mono
  nerd-fonts.iosevka
]
