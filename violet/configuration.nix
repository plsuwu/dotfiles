{
  inputs,
  config,
  pkgs,
  ...
}:
{
  imports = [
    ./hardware-configuration.nix
  ];

  greeter.greetd.enable = true;
  greeter.command = "hyprland";

  boot = {
    loader.systemd-boot.enable = true;
    loader.efi.canTouchEfiVariables = true;

    kernelPackages = pkgs.linuxPackages_latest;
    extraModprobeConfig = ''
      options hid_apple fnmode=2
    '';
  };

  networking.hostName = "violet";
  networking.networkmanager.enable = true;

  security.polkit.enable = true;
  services.gnome.gnome-keyring.enable = true;

  fonts.packages = with pkgs; [
    nerd-fonts.jetbrains-mono
    nerd-fonts.noto
  ];

  time.timeZone = "Australia/Brisbane";
  i18n.defaultLocale = "en_US.UTF-8";

  hardware = {
    graphics.enable = true;
    nvidia = {
      open = true;
      package = config.boot.kernelPackages.nvidiaPackages.latest;

      nvidiaSettings = true;
      modesetting.enable = true;
    };
  };

  services = {
    pipewire = {
      enable = true;
      pulse.enable = true;
    };

    xserver = {
      xkb.layout = "us";
      xkb.options = "eurosign:e,caps:escape";

      enable = true;
      videoDrivers = [ "nvidia" ];
    };

    fstrim.enable = true;
  };

  programs.zsh.enable = true;
  users.defaultUserShell = pkgs.zsh;
  users.users.please = {
    home = "/home/please";
    isNormalUser = true;
    extraGroups = [
      "wheel"
      "networkmanager"
    ];
    packages = [
      pkgs.tree
    ];
  };

  environment.systemPackages = with pkgs; [
    git
    neovim
    wget

    jq

    grim
    slurp
    wayland
    wl-clipboard
    wlr-randr
    mako

    inputs.home-manager.packages.${pkgs.system}.default
    # inputs.alejandra.packages.${pkgs.system}.default
  ];

  environment.sessionVariables.EDITOR = "nvim";
  environment.pathsToLink = [
    "/share/xdg-desktop-portal"
    "/share/applications"
    "/share/zsh"
  ];

  nixpkgs.config.allowUnfree = true;

  nix.optimise.automatic = true;
  nix.settings = {
    experimental-features = [
      "nix-command"
      "flakes"
    ];
    trusted-users = [
      "root"
      "@wheel"
    ];
    download-buffer-size = 524288000;
  };

  # Enable the OpenSSH daemon.
  # services.openssh.enable = true;

  # Open ports in the firewall.
  # networking.firewall.allowedTCPPorts = [ ... ];
  # networking.firewall.allowedUDPPorts = [ ... ];
  # Or disable the firewall altogether.
  # networking.firewall.enable = false;

  system.stateVersion = "25.11";
}
