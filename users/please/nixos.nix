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

  services.solaar = {
    enable = true;
    window = "hide";
    batteryIcons = "regular";
    extraArgs = "";
  };

  environment.systemPackages = with pkgs; [
    reaper

    # mono

    winetricks
    protontricks
    protonup-qt
    wine64
    wine64Packages.full
    wine64Packages.waylandFull
  ];

  programs = {
    steam = {
      enable = true;
      package = pkgs.steam;
    };
    gamemode.enable = true;
  };
}
