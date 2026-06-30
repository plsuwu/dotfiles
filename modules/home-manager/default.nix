{
  pkgs,
  lib,
  ...
}:
{
  imports = [
    ./desktop
    ./nvim
    ./shell
    ./terminal
    ./vesktop

    ./browsers.nix
    ./direnv.nix
    ./git.nix
    ./zoom.nix
    ./lutris.nix
  ];

  programs.mullvad-vpn.enable = true;
  home.packages = [
    # ..
  ] ++ (import ../../packages { inherit pkgs lib; });
}
