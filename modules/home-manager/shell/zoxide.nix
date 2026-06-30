{
  pkgs,
  lib,
  config,
  ...
}:
let
  cfg = config.systemModules.shell;
in
{
  config = lib.mkIf cfg.enable {
    programs.zoxide = {
      enable = true;
      enableZshIntegration = true;
      options = [ "--cmd cd" ];
    };
  };
}
