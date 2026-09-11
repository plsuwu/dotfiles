{
  inputs,
  pkgs,
  username,
  config,
  lib,
  ...
}:
{
  users.users.${username} = {
    isNormalUser = true;
    shell = pkgs.zsh;
    extraGroups = [
      "wheel"
      "networkmanager"
    ]
    ++ lib.optional config.systemModules.docker.enable "docker"
    ++ lib.optional config.systemModules.virt.enable "libvirtd";
  };

  nix.registry.pls.flake = inputs.self;

  programs.zsh.enable = true;
  environment.variables = {
    XDG_DATA_HOME = "$HOME/.local/share";
  };

  environment.systemPackages = [
    pkgs.reaper
  ];

  programs.steam = {
    enable = true;
    package = pkgs.steam;
  };
}
