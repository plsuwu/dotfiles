{
  lib,
  config,
  user,
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
    security = {
      pam.services.hyprlock = { };
    };
  };
}
