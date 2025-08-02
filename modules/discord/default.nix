{ lib, config, ... }:
let
  cfg = config.modules.discord;

  p = import ./plugins.nix { };
  plugins = lib.genAttrs p.enabled (
    name: { enabled = true; } // (p.settings.${name} or { })
  );
in
{
  options.modules.discord = {
    enable = lib.mkEnableOption "discord";
  };

  config = lib.mkIf cfg.enable {
    programs.vesktop = {
      enable = true;
      settings = {
        arRPC = false;
        tray = true;
        appBadge = true;
        discordBranch = "stable";
        hardwareAcceleration = true;
        spellCheckLanguages = [
          "en-US"
          "en"
        ];
      };

      vencord.settings = {
        inherit plugins;
      };
    };
  };
}
