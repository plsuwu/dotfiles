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
    chromium = {
      enable = lib.mkEnableOption "chromium";
      ephemeral = lib.mkEnableOption "ephemeral";
    };
  };

  config = lib.mkIf cfg.enable {
    programs.firefox = {
      enable = true;
      policies = {
        EnableTrackingProtection = {
          Value = true;
          Cryptomining = true;
          Fingerprinting = true;
        };
        ExtensionSettings = {
          # ublock
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

    programs.chromium = lib.mkIf cfg.chromium.enable {
      enable = true;
      extraOpts = {
        ForceEphemeralProfiles = cfg.chromium.ephemeral;
      };
    };
  };
}
