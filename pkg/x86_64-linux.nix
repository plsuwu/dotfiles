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
  jq

  obsidian

  iw
  pavucontrol

  vim
  wayland

  btop
  remmina
  zoom-us
  usbutils

  nix-index
  morph

  davinci-resolve
  brave
  spotify

  ffmpeg-full
  cudatoolkit
  file-roller

  inter
  nerd-fonts.noto
  nerd-fonts.jetbrains-mono
  nerd-fonts.iosevka

  aseprite

  aporetic
  nerd-fonts.iosevka-term
  iosevka-comfy.comfy

  # fixes e.g. Chinese, Japanese character rendering issues in Chromium-based programs
  noto-fonts-cjk-serif
  noto-fonts-cjk-sans
]
