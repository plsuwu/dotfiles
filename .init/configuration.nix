{ config, lib, pkgs, ... }:

# this is the initial `configuration.nix` used to spawn the system's base.
#
# also see `iso.nix` for the live nix iso config, which was used as a workaround to
# several consecutive issues i encountered with nvidia drivers, my cpu, the linux kernel,
# and nixos during the installation.
# maybe also solvable with a graphical image using calamares but i resent gparted.
# -------------
#
# BASE INSTALL:
#
# > i've probably forgotten something so take this as an approximation.
#
# - build the updated iso with instructions in `iso.nix`.
# - partition, format, and mount filesystem.
# - run `nixos-generate-config --root /mnt` to generate the `hardware-configuration.nix`, which
#       generates the fstab (so the system can boot).
# - make a home dir (e.g `mkdir -p /mnt/home/please/Repositories`)
# - clone this repository into that dir and checkout the correct branch:
#
# ```
#   cd /mnt/home/please/Repositories && git clone https://github.com/plsuwu/dotfiles && cd \
#       dotfiles && git checkout nixos
# ```
#
# - copy over this configuration.nix: `sudo cp ./configuration.nix /etc/nixos/configuration.nix`
# - run `nixos-install` to install this configuration to the machine.
# - reboot, uncomment the `nvidia`/`xserver` configuration items, and run `nixos-rebuild switch`.
# - ideally everything works, but there is probably something i've forgotten, so it may take a
#       handful of attempts to correctly comment out nvidia driver stuff correctly.
#
# HOME-MANAGER:
# ```
# nix-channel --add https://github.com/nix-community/home-manager/archive/master.tar.gz home-manager
# nix-channel --add https://nixos.org/channels/nixos-unstable nixos
# nix-channel --update
# nix-shell '<home-manager>' -A install
# ```
# then, copy over entire config from nixos branch to `~/.config/home-manager`. cd into directory and run
#       `./builder.sh` to build.
#
# ------------------


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
    # xserver = {
    #   enable = true;
    #   xkb.layout = "us";
    #   videoDrivers = [ "nvidia" ];
    #   libinput.enable = true;
    #   autoRepeatDelay = 200;
    #   autoRepeatInterval = 50;
    #   desktopManager.gnome = {
    #     enable = true;
    #   };
    #   displayManager.gdm = {
    #     enable = true;
    #   };
    # };
    fstrim.enable = true;
  };

  hardware = {
    pulseaudio.enable = true;
    # opengl = {
    #   enable = true;
    #   driSupport = true;
    #   driSupport32Bit = true;
    # };
    # nvidia = {
    #   modesetting.enable = true;
    #   nvidiaSettings = true;
    #   open = false;
    #   package = config.boot.kernelPackages.nvidiaPackages.latest;
    # };
  };

  users.users.please = {
    isNormalUser = true;
    extraGroups = [ "wheel" "networkmanager" ];
    packages = with pkgs; [
      tree
    ];
  };

  fonts.packages = with pkgs; [
    (nerdfonts.override { fonts = [ "Ubuntu" "UbuntuMono" "Noto" "JetBrainsMono" ]; })
  ];

  nixpkgs.config.allowUnfree = true;

  environment = {
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
      p7zip unzip
      fd ripgrep jq fzf
      alacritty tmux zsh zoxide
      google-chrome
      bitwarden bitwarden-cli
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

  # system.copySystemConfiguration = true;
  system.stateVersion = "24.05";

}
