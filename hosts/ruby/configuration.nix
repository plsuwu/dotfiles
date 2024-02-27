{ inputs, config, lib, pkgs, ... }:

{
  imports =
    [
      ./hardware-configuration.nix
      inputs.sops-nix.nixosModules.sops
    ];

  boot = {
    kernelPackages = pkgs.linuxPackages_latest;
    loader = {
      systemd-boot.enable = true;
      efi.canTouchEfiVariables = true;
    };

    # force remap of QK100's f-keys in the kernel
    extraModprobeConfig = ''
      options hid_apple fnmode=2
    '';
  };

  networking = {
    hostName = "ruby";
    networkmanager.enable = true;
  };


  services = {
    xserver = {
      enable = true;
      xkb.layout = "us";
      videoDrivers = [ "nvidia" ];
      libinput.enable = true;
      autoRepeatDelay = 200;
      autoRepeatInterval = 50;

      # xrandrOptions = {
      #     output = "DP-0";
      #     primary = true;
      # };

      windowManager.xmonad = {
        enable = true;
        enableContribAndExtras = true;
        config = builtins.readFile ./xmonad/xmonad.hs;
      };
      # picom = {};
      displayManager = {
        sessionCommands = ''
          xset r rate 200 50
        '';
        gdm = {
          enable = true;
        };
      };
    };
    fstrim.enable = true;
  };

  hardware = {
    pulseaudio.enable = true;
    opengl = {
      enable = true;
      driSupport = true;
      driSupport32Bit = true;
    };
    nvidia = {
      modesetting.enable = true;
      nvidiaSettings = true;
      open = false;

      package = config.boot.kernelPackages.nvidiaPackages.latest;
    };
  };

  sops = {
    defaultSopsFile = ../../home/please/secrets/secrets.yaml;
    defaultSopsFormat = "yaml";
    age.keyFile = "/home/please/.config/sops/age/keys.txt";
  };

  users.users.please = {
    isNormalUser = true;
    extraGroups = [ "wheel" "networkmanager" ];
    shell = pkgs.zsh;
    packages = with pkgs; [
      tree
    ];
  };

  programs = {
      zsh.enable = true;
      neovim.enable = true;
  };
  fonts.packages = with pkgs; [
    (nerdfonts.override { fonts = [ "Ubuntu" "UbuntuMono" "Noto" "JetBrainsMono" ]; })
  ];

  nixpkgs.config = {
    allowUnfree = true;
    allowBroken = true;
  };

  # home-manager.useGlobalPkgs = true;

  environment = {
    shells = with pkgs; [ zsh ];
    variables.EDITOR = "nvim";
    systemPackages = with pkgs; [
      neovim vscode
      netcat wget curl killall
      git gh
      gnumake platformio
      python3Full cp210x-program
      gcc_multi rustup luajitPackages.luarocks
      nodejs go php dotnet-sdk dotnet-runtime
      mono maven julia cmake csharpier pkg-config openssl
      openssl_legacy
      gptfdisk parted
      p7zip unzip
      fd ripgrep jq fzf
      alacritty tmux zsh zoxide neofetch
      google-chrome
      bitwarden bitwarden-cli
      xorg.xrandr xorg.xprop xdotool wmctrl
      playerctl pamixer
      clipmenu xclip rofi
      solaar trayer polkit_gnome picom feh
    ];
  };

  time.timeZone = "Australia/Brisbane";
  i18n.defaultLocale = "en_US.UTF-8";
  sound.enable = true;

  nix.settings = {
    experimental-features = [ "nix-command" "flakes" ];
    trusted-users = ["root" "please" ];
    auto-optimise-store = true;
  };

  system.stateVersion = "24.05";
}

