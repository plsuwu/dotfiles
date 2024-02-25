{ config, lib, pkgs, ... }:

{
  imports =
    [
      ./hardware-configuration.nix
    ];

  boot = {
    kernelPackages = pkgs.linuxPackages_latest;
    loader = {
      systemd-boot.enable = true;
      efi.canTouchEfiVariables = true;
    };
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

      # xmonad-related stuff is untested!!
      # compiles ok but i wanna push this prior to testing.
      windowManager.xmonad = {
        enable = true;
        enableContribAndExtras = true;
        config = builtins.readFile ./xmonad/xmonad.hs;
      };
      displayManager = {
        sessionCommands = ''
          xset r rate 250 40
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

  users.users.please = {
    isNormalUser = true;
    extraGroups = [ "wheel" "networkmanager" ];
    shell = pkgs.zsh;
    packages = with pkgs; [
      tree
    ];
  };

  programs.zsh.enable = true;
  fonts.packages = with pkgs; [
    (nerdfonts.override { fonts = [ "Ubuntu" "UbuntuMono" "Noto" "JetBrainsMono" ]; })
  ];

  nixpkgs.config.allowUnfree = true;

  environment = {
    shells = with pkgs; [ zsh ];
    variables.EDITOR = "nvim";
    systemPackages = with pkgs; [
      neovim
      wget
      curl
      git
      gh
      gnumake
      gcc_multi rustup luajitPackages.luarocks
      python3Full nodejs go php dotnet-sdk dotnet-runtime
      mono maven julia
      cmake csharpier pkg-config openssl
      p7zip unzip
      fd ripgrep jq fzf
      alacritty tmux zsh zoxide
      google-chrome
      bitwarden bitwarden-cli
      xorg.xrandr xorg.xprop gnome.gpaste xdotool wmctrl
      playerctl
      clipmenu rofi
      eww solaar trayer polkit_gnome picom
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

