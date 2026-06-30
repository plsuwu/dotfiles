{
  lib,
  ...
}:
{
  imports = [
    ./alacritty.nix
    ./tmux.nix
  ];

  options.systemModules.terminal = {
    enable = lib.mkEnableOption "terminal";
  };
}
