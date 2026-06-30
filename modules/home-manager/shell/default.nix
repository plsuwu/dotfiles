{
  lib,
  ...
}:
{
  imports = [
    ./eza.nix
    ./zsh.nix
    ./zoxide.nix
  ];

  options.systemModules.shell = {
    enable = lib.mkEnableOption "shell";
  };
}
