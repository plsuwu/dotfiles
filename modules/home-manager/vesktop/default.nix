{
  lib,
  config,
  pkgs,
  ...
}:
let
  cfg = config.systemModules.vesktop;

  p = import ./plugins.nix { };
  plugins = lib.genAttrs p.enabled (
    name: { enabled = true; } // (p.settings.${name} or { })
  );
in
{
  options.systemModules.vesktop = {
    enable = lib.mkEnableOption "vesktop";
  };

  config = lib.mkIf cfg.enable {
    programs.vesktop = {
      enable = true;
      settings = {
        arRPC = false;
        tray = true;
        appBadge = true;
        hardwareAcceleration = true;
        winNativeTitleBar = true;
        discordBranch = "stable";
        spellCheckLanguages = [
          "en-US"
          "en"
        ];
      };

      vencord.settings = {
        inherit plugins;
        useQuickCss = false;
      };
    };
  };
}
