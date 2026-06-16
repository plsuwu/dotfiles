{
  lib,
  config,
  pkgs,
  ...
}:
let
  cfg = config.modules.auth;
in
{
  options.modules.auth = {
    enable = lib.mkEnableOption "auth";
  };

  config = lib.mkIf cfg.enable {
    services.gnome.gnome-keyring.enable = true;
    services.dbus.packages = [ pkgs.gcr ];

    security = {
      pam.services.hyprlock = { };
      pam.services.swaylock = { };
      pam.services.gdm = { };
    };
  };
}
