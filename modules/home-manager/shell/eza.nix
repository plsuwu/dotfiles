{
  lib,
  config,
  pkgs,
  ...
}:
let
  cfg = config.systemModules.shell;
  source = ./eza-theme.yml;
in
{
  config = lib.mkIf cfg.enable {
    programs.eza = {
      enable = true;
      enableZshIntegration = true;
      git = true;
      icons = "auto";

      extraOptions = [
        "--group-directories-first"
      ];
    };

    xdg.configFile."eza/theme.yml" = {
      inherit source;
    };
  };
}
