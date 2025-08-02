{ pkgs, ... }:
with pkgs;
[
  zip
  unzip
  p7zip
  xz

  file
  doctl

  fzf
  ripgrep
  gh
  github-cli

  file
  which
  gnused
  gnutar
  gawk
]
