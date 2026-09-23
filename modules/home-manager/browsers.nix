{
  inputs,
  config,
  lib,
  pkgs,
  ...
}:
let
  cfg = config.systemModules.browsers;
  mkExtension = shortId: guid: {
    name = guid;
    value = {
      install_url = "https://addons.mozilla.org/en-US/firefox/downloads/latest/${shortId}/latest.xpi";
      installation_mode = "normal_installed";
    };
  };
  ffExtensions = [
    (mkExtension "ublock-origin" "uBlock0@raymondhill.net")
    (mkExtension "bitwarden-password-manager" "{446900e4-71c2-419f-a6a7-df9c091e268b}")
    (mkExtension "tampermonkey" "firefox@tampermonkey.net")
  ];

  ffPrefs = {
    "app.update.auto" = false;
    "extensions.autoDisableScopes" = 0;
    "toolkit.tabbox.switchByScrolling" = true;
    "zen.window-sync.enabled" = false;
    "zen.window-sync.prefer-unsynced-windows" = true;
  };
in
{
  options.systemModules.browsers = {
    enable = lib.mkEnableOption "browser (brave)";
    enableFirefox = lib.mkOption {
      type = lib.types.bool;
      default = true;
    };
    enableChromium = lib.mkOption {
      type = lib.types.bool;
      default = true;
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

      (pkgs.wrapFirefox
        inputs.zen-browser.packages.${pkgs.stdenv.hostPlatform.system}.zen-browser-unwrapped
        {
          extraPolicies = {
            DisableTelemtry = true;
            ExtensionSettings = builtins.listToAttrs ffExtensions;

            extraPrefs = lib.concatLines (
              lib.mapAttrsToList (
                name: value:
                "lockPref(${lib.strings.toJSON name}, ${lib.strings.toJSON value});"
              ) ffPrefs
            );

            SearchEngines = {
              Default = "ddg";
              Add = [
                {
                  Name = "nixpkgs packages";
                  URLTemplate = "https://search.nixos.org/packages?query={searchTerms}";
                  IconURL = "https://wiki.nixos.org/favicon.ico";
                  Alias = "@np";
                }
                {
                  Name = "nixos options";
                  URLTemplate = "https://search.nixos.org/options?query={searchTerms}";
                  IconURL = "https://wiki.nixos.org/favicon.ico";
                  Alias = "@no";
                }
                {
                  Name = "nixos wiki";
                  URLTemplate = "https://wiki.nixos.org/w/index.php?search={searchTerms}";
                  IconURL = "https://wiki.nixos.org/favicon.ico";
                  Alias = "@nw";
                }
                {
                  Name = "youtube";
                  URLTemplate = "https://www.youtube.com/results?search_query={searchTerms}";
                  IconURL = "https://youtube.com/favicon.ico";
                  Alias = "@yt";
                }
                {
                  Name = "twitch";
                  URLTemplate = "https://www.twitch.tv/search?term={searchTerms}";
                  IconURL = "https://twitch.tv/favicon.ico";
                  Alias = "@ttv";
                }
              ];
            };
          };
        }
      )
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
        ExtensionSettings = builtins.listToAttrs ffExtensions;
        EnableTrackingProtection = {
          Fingerprinting = true;
        };
      };
    };

    xdg.mimeApps = {
      enable = true;
      defaultApplications =
        let
          browser = "zen.desktop";
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
