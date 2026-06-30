{
  config,
  lib,
  pkgs,
  ...
}:
let
  cfg = config.systemModules.browsers;
in
{
  options.systemModules.browsers = {
    enable = lib.mkEnableOption "browser (brave)";

    enableFirefox = lib.mkOption {
      type = lib.types.bool;
      default = true;
      description = "enable firefox";
    };

    enableChromium = lib.mkOption {
      type = lib.types.bool;
      default = true;
      description = "enable chromium";
    };
  };

  config = lib.mkIf cfg.enable {
    home.packages = with pkgs; [
      (brave.override {
        commandLineArgs = [
          "--ozone-platform-hint=auto"
          "--enable-features=UseOzonePlatform"
          "--ozone-platform=wayland"
        ];
      })
    ];

    programs.chromium = {
      enable = cfg.enableChromium;
      package = pkgs.chromium.override {
        enableWideVine = true;
        commandLineArgs = [
          "--ozone-platform-hint=auto"
          "--enable-features=UseOzonePlatform"
          "--ozone-platform=wayland"
        ];
      };
    };

    programs.firefox = {
      enable = cfg.enableFirefox;
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

    xdg.mimeApps = {
      enable = true;
      defaultApplications =
        let
          browser = "brave.desktop";
        in
        {
          "text/html" = "${browser}";
          "application/pdf" = "${browser}";
          "x-scheme-handler/http" = "${browser}";
          "x-scheme-handler/https" = "${browser}";
          "x-scheme-handler/about" = "${browser}";
          "x-scheme-handler/mailto" = "${browser}";
          "x-scheme-handler/webcal" = "${browser}";
          "x-scheme-handler/unknown" = "${browser}";
        };
    };
  };
}
