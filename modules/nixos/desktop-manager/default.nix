{
  lib,
  config,
  pkgs,
  user,
  inputs,
  ...
}:
let
  cfg = config.modules.desktop-manager;
in
{
  options.modules.desktop-manager = {
    enable = lib.mkEnableOption "desktop-manager";
  };

  config = lib.mkIf cfg.enable {

    services = {
      xserver = {
        enable = true;
        videoDrivers = [ "nvidia" ];

        xkb.layout = "us";
        xkb.options = "eurosign:e,caps:escape";
      };

      fstrim.enable = true;

      greetd = {
        enable = true;
        settings =
          let
            greeter = "${pkgs.greetd.tuigreet}/bin/tuigreet";
            command = "${pkgs.hyprland}/bin/Hyprland";
          in
          {
            default_session = {
              command = "${greeter} --asterisks --time --cmd '${command}'";
              user = "greeter";
            };
          };
      };

      libinput.enable = true;
    };

    fonts = {
      packages = with pkgs; [
        nerd-fonts.jetbrains-mono
        nerd-fonts.noto
        openmoji-color
      ];

      fontconfig = {
        hinting.autohint = true;
        defaultFonts = {
          emoji = [ "Noto Color Emoji" ];
        };
      };
    };

    xdg.autostart.enable = true;
    xdg.portal = {
      enable = true;
      configPackages = with pkgs; [
        xdg-desktop-portal-wlr
        xdg-desktop-portal-gtk
        xdg-desktop-portal-hyprland
      ];
      extraPortals = with pkgs; [
        xdg-desktop-portal-wlr
        xdg-desktop-portal-gtk
        xdg-desktop-portal-hyprland
      ];
    };

    programs.hyprland.enable = true;
    environment.sessionVariables = {
      NIXOS_OZONE_WL = "1";
      MOZ_ENABLE_WAYLAND = "1";
      EDITOR = "nvim";
    };

    hardware = {
      graphics.enable = true;
      nvidia = {
        open = true;
        package = config.boot.kernelPackages.nvidiaPackages.latest;

        nvidiaSettings = true;
        modesetting.enable = true;
      };
    };

    systemd.services.greetd.serviceConfig = {
      Type = "idle";
      StandardInput = "tty";
      StandardOutput = "tty";
      StandardError = "journal";

      TTYReset = true;
      TTYVHangup = true;
      TTYVTDisallocate = true;
    };
  };
}
