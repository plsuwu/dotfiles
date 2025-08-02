{
  pkgs,
  user,
  inputs,
  system,
  ...
}:
{
  imports = [
    ./auth
    ./boot
    ./desktop-manager
    ./net
    ./sound
    ./thunar
    ./tz
    ./users
    ./virt
  ];

  system.stateVersion = "25.11";
  i18n.defaultLocale = "en_US.UTF-8";

  nixpkgs.config.allowUnfree = true;
  nixpkgs.config.permittedInsecurePackages = [
    "dotnet-core-combined"
    "dotnet-sdk-6.0.428"
    "dotnet-sdk-wrapped-6.0.428"
  ];

  # nix.optimise.automatic = true;
  nix.settings = {
    experimental-features = [
      "nix-command"
      "flakes"
    ];
    trusted-users = [
      "root"
      user.name
    ];
    auto-optimise-store = true;
    download-buffer-size = 524288000;
  };

  documentation = {
    enable = true;
    man.enable = true;
    man.generateCaches = true;
  };

  environment.pathsToLink = [
    "/share/xdg-desktop-portal"
    "/share/applications"
    "/share/zsh"
  ];

  services.gvfs.enable = true;
  services.gnome.gnome-keyring.enable = true;
  services.dbus.packages = [ pkgs.gcr ];
}
