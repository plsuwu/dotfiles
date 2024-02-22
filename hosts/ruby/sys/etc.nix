{ lib, config, pkgs, ... }:
{
  # locale, machine identifiers
  time.timeZone = "Australia/Brisbane";
  i18n.defaultLocale = "en_US.UTF-8";
  sound.enable = true;
  networking = {
    hostName = "ruby";
    networkmanager.enable = true;
  };


  # xorg
  services.xserver = {
    videoDrivers = [ "nvidia" ];
    enable = true;
    xkb.layout = "us";

    # swap this over to xmonad when it makes sense
    desktopManager.cinnamon = {
      enable = true;
    };
  };
}
