{
  pkgs,
  lib,
  config,
  ...
}:
let
  cfg = config.modules.browser;
in
{
  options.modules.browser = {
    enable = lib.mkEnableOption "browser";
  };

  config = lib.mkIf cfg.enable {
    programs.chromium.enable = true;
    programs.firefox = {
      enable = true;
      configPath = ".mozilla/firefox";
      policies = {
        EnableTrackingProtection = {
          Fingerprinting = true;
        };
        
        # ublock
        ExtensionSettings = {
          "uBlock0@raymondhill.net" = {
            install_url = "https://addons.mozilla.org/firefox/downloads/latest/ublock-origin/latest.xpi";
            installation_mode = "force_installed";
          };

          # bitwarden
          "{446900e4-71c2-419f-a6a7-df9c091e268b}" = {
            install_url = "https://addons.mozilla.org/firefox/downloads/latest/bitwarden-password-manager/latest.xpi";
            installation_mode = "force_installed";
          };
        };
      };
    };
  };
}
