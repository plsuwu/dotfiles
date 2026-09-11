{
  inputs,
  hostname,
  ...
}:
{
  imports = [ ./hardware-configuration.nix ];

  config.systemModules = {
    boot.enable = true;
    display.enable = true;
    docker.enable = true;
    greeter.enable = true;
    nh.enable = true;
    nix-ld.enable = true;
    sound.enable = true;
    sshd.enable = true;
    virt.enable = true;
  };

  config = {
    fonts.enableDefaultPackages = true;
    networking.hostName = hostname;
    time.timeZone = "Australia/Brisbane";
    system.stateVersion = "25.11";
    i18n.defaultLocale = "en_US.UTF-8";
    services.fstrim.enable = true;

    nixpkgs.config = {
      allowUnfree = true;
      permittedInsecurePackages = [
        "pnpm-10.29.2"
      ];
    };

    nix.settings = {
      download-buffer-size = 524288000;
      experimental-features = [
        "nix-command"
        "flakes"
      ];

      trusted-users = [
        "root"
        "@wheel"
      ];
    };
    documentation = {
      enable = true;
      man.enable = true;
      man.cache.enable = true;
    };
  };
}
