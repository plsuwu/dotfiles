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

  davinci-resolve

  ffmpeg-full
  cudatoolkit
  file-roller

  inter
  nerd-fonts.noto
  nerd-fonts.jetbrains-mono
]
